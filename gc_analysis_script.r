###################################################################################
########################  2016-2017 SIBERIA SHRUB SAPFLOW  ########################
########################  Low Density and German Site      ########################
###################################################################################
######################## extraction of heat balance sensor ########################
######################## measurements and the calculation  ########################
######################## of sap flow for measurements at   ########################
######################## in a floodplain (german site)     ########################
######################## and a low density upland forest   ########################
###################################################################################
######################## R version 3.4.3                   ######################## 
###################################################################################
######################## outputs:                          ########################
######################## sensor (list) sensor info         ########################
######################## sapFlow (list) sapflow g m-2 s-1  ########################
######################## specFlow (list) species averaged  ########################
########################     sapflow g m-2 s-1             ########################
######################## specTday (list) species averaged  ########################
########################     total daily transpiration     ########################
########################     L m-2 s-1                     ########################
######################## gcSpec (list) species averaged    ########################
########################     stomatal conductance          ########################
########################     mol m-2 s-1                   ########################
###################################################################################


##################################
# read in data                   #
##################################
source("/Users/hkropp/Documents/GitHub/ch_shrub_sap/sap_flow_process.r")
library(ggplot2)

#read in met data
datDir <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/viperData"
datRH <- read.csv(paste0(datDir,"/sensor/decagon/met/RH.VP4.csv"))
datTC <- read.csv(paste0(datDir,"/sensor/decagon/met/TempC.VP4.csv"))
metG <- read.csv(paste0(datDir,"/German_met.csv"))
datAir <- read.csv(paste0(datDir,"/sensor/airport/airport.csv"))
datQa <- read.csv(paste0(datDir,"/sensor/decagon/met/PAR.QSOS Par.csv"))
datQa <- datQa[datQa$site=="ld",]

#take only revlant info for par and rename to simplier
datQ <- data.frame(datQa[,1:3], PAR=datQa$PAR.QSOS.Par)
##################################
# set up directories             #
##################################
plotDir <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/plots"

#############################################
##### read in allometry data         #######

alloUc <- read.csv(paste0(datDir,"/2012 - 2017 Density Gradient Shrubs.csv"))
alloUc  <- alloUc %>%
  filter(substr(alloUc$Site,1,1) == "L")

alloF <- read.csv(paste0(datDir,"/flood_allom.csv"))

#only look at whole plant allometry 
alloF <- alloF %>%
  filter(alloF$type == "plant")
##########################################
# organize met data for all sites   ######
	
#join RH and TC
datM <- inner_join(datRH, datTC, by=c("doy","year","hour","site"))
datLt <- datM %>%
  filter(datM$site=="ld"&datM$year==2016)
datLR <- data.frame(doy=datLt$doy,year=datLt$year,hour=datLt$hour,RH=datLt$RH*100,temp=datLt$TempC.VP4)


#date and time for G
dateG <- dmy_hm(metG$Date.Time)
metG$doy <- yday(dateG)
metG$year <- year(dateG)
metG$hour <- hour(dateG)+(minute(dateG)/60)

#calculate vpd for each site
#saturated water vapor
datLR$e.sat <- 0.611*exp((17.502*datLR$temp)/(datLR$temp+240.97))
metG$e.sat <- 0.611*exp((17.502*metG$temp)/(metG$temp+240.97))

#fix any RH greater than 100
datLR$RHf <- ifelse(datLR$RH>99.9,99.9,datLR$RH)
metG$RHf <- ifelse(metG$RH>99.9,99.9,metG$RH)
#calculate vapor pressure deficit
datLR$D <- datLR$e.sat-((datLR$RHf/100)*datLR$e.sat)
metG$D <- metG$e.sat-((metG$RHf/100)*metG$e.sat)



datLR <- left_join(datLR,datAir, by=c("doy","year"))
metG <- left_join(metG,datAir, by=c("doy","year"))
metG$siteid <- rep(1,nrow(metG),)
datLR$siteid <- rep(2,nrow(datLR),)
names(metG)
names(datLR)

datG <- metG %>%
  select(doy,year,hour,siteid,temp,RH,e.sat,RHf,D,Pkpa,Pr.mm,Pkpa.gap)
datLRr <- datLR %>%
  select(doy,year,hour,siteid,temp,RH,e.sat,RHf,D,Pkpa,Pr.mm,Pkpa.gap)
metDF <- rbind(datLRr,datG)
#add a site id
#1=floodplain, 2= upland

#join in PAR data
metDF <- left_join(metDF,datQ, by=c("doy","year","hour"))

metDF <- metDF %>%
  filter(metDF$year == 2016)

##########################################
####### organize gc data        ##########

#add a site id
#1=floodplain, 2= upland
for(i in 1:2){
	gcSpec[[i]]$siteid <- rep(i, dim(gcSpec[[i]])[1])
}


#get a summary of the days in each dataframe
gcAll <- rbind(gcSpec[[1]],gcSpec[[2]])
#get number of observations on each day

daysAll <- gcAll %>%
  group_by(doy, year, species, siteid) %>%
  summarize(nGc = n())
  

#exclude days with too few observations
daysAll <- daysAll %>%
  filter(nGc>=5)


#join back into gcAll

gcDays <- inner_join(gcAll,daysAll,by=c("doy","year","species","siteid"))

#join met to gcDays
gcDays <- left_join(gcDays, metDF, by=c("doy","year","hour","siteid"))

#get a dataframe of just site days

siteDays <- unique(data.frame(doy=gcDays$doy,siteid=gcDays$siteid))

gcDays$spsID <- ifelse(gcDays$siteid==1&gcDays$species == "Alnus",1,
				ifelse(gcDays$siteid==1&gcDays$species == "Salix",2,
				ifelse(gcDays$siteid==2&gcDays$species == "Betula",3,
				ifelse(gcDays$siteid==2&gcDays$species == "Salix",4,NA))))


###################################################
########### organize allometry data        #######

#pull out only low density sites
#all site ids start with L for low density


flAllo <- alloF[alloF$site == "y4" | alloF$site == "amb",] 

upAllo <- alloF[alloF$site == "exp" | alloF$site == "ldf",] 

#############################################
###### Daily transpiration and met    #######


#combine transpiration from list
#1=floodplain, 2= upland
for(i in 1:2){
  specTday[[i]]$siteid <- rep(i, dim(specTday[[i]])[1])
}
tdayDF <- rbind(specTday[[1]],specTday[[2]])
#create site species id for graphing
tdayDF$spsID <- ifelse(tdayDF$siteid==1&tdayDF$species == "Alnus",1,
                       ifelse(tdayDF$siteid==1&tdayDF$species == "Salix",2,
                              ifelse(tdayDF$siteid==2&tdayDF$species == "Betula",3,
                                     ifelse(tdayDF$siteid==2&tdayDF$species == "Salix",4,NA))))

metDaily <- metDF %>%
  group_by(doy, year, siteid) %>%
  summarise(
            aveVPD = mean(D),
            aveTemp = mean(temp))

Pr.air <- datAir %>%
  filter(year==2016)

Pr.week <- rep(NA,6)
for(i in 7:nrow(Pr.air)){
  Pr.week[i] <- sum(Pr.air$Pr.mm[(i-6):i])
}
Pr_week <- data.frame(doy=Pr.air$doy,
                      year=Pr.air$year,
                      Pr_week = Pr.week)
metDaily <- left_join(metDaily, Pr_week, by=c("doy","year"))
	
#################################################################
################## Whole plant T         ########################


#get sla from sensors
flSLA <- unique(data.frame(Species=sensor[[1]]$species,SLA=sensor[[1]]$SLA))
flSLA$species <- c("sal","aln")

#not recognizing names as different even though they are exactly the same...
upSLA <- 	unique(data.frame(Species=sensor[[2]]$species,SLA=sensor[[2]]$SLA))[1:2,]
upSLA $species <- c("bet","sal")
#join SLA into allometry
upAllo <- left_join(upAllo,upSLA, by="species")
flAllo <- left_join(flAllo,flSLA, by="species")
#calculate leaf area in m2
upAllo$aleaf <- (upAllo$mass * upAllo$SLA)
flAllo$aleaf <- (flAllo$mass * flAllo$SLA) 

#aggregate
upLeaf <- aggregate(upAllo$aleaf, by=list(upAllo$Species), FUN="mean", na.rm=TRUE)
colnames(upLeaf) <- c("Species","leafP")
upLeaf$siteid <- rep(2,2)
floodLeaf <- aggregate(flAllo$aleaf, by=list(flAllo$Species), FUN="mean", na.rm=TRUE)
colnames(floodLeaf) <- c("Species","leafP")
floodLeaf$siteid <- rep(1,2)
plantL <- rbind(upLeaf, floodLeaf)

plantL$spsID <- ifelse(plantL$siteid==1&plantL$Species == "Alnus",1,
                       ifelse(plantL$siteid==1&plantL$Species == "Salix",2,
                              ifelse(plantL$siteid==2&plantL$Species == "Betula",3,
                                     ifelse(plantL$siteid==2&plantL$Species == "Salix",4,NA))))

#join into transpiration
tdayDF <- left_join(tdayDF, plantL, by="spsID")

#transpiration estimates at the whole plant level
tdayDF$L.plant.day <- tdayDF$L.m2.day*tdayDF$leafP
tdayDF$L.plant.daySD <- tdayDF$L.m2.daySD*tdayDF$leafP

#################################################################
################## gc organize           ########################

gcDF <- rbind(gcSpec[[1]],gcSpec[[2]])

gcDF <- left_join(gcDF, metDF, by=c("hour","doy","year", "siteid"))

gcDF$spsID <- ifelse(gcDF$siteid==1&gcDF$species == "Alnus",1,
                     ifelse(gcDF$siteid==1&gcDF$species == "Salix",2,
                            ifelse(gcDF$siteid==2&gcDF$species == "Betula",3,
                                   ifelse(gcDF$siteid==2&gcDF$species == "Salix",4,NA))))
# limit low PAR
gcDF <- gcDF %>%
  filter(PAR > 20)

gcCount <- gcDF %>%
  group_by(doy,year,spsID)%>%
  summarise(n_obs=n()) %>%
  filter(n_obs>6)%>%
  filter(doy >= 187 & doy <= 230) # only days when all sensors had obs

gcMod <- inner_join(gcDF, gcCount, by=c("doy","year","spsID"))
unique(gcMod$doy)


#################################################################
################## gc model              ########################

# library(rjags)
# library(MCMCvis)
# 
# #finalize data org for model
# spsDay <- unique(data.frame(doy=gcMod$doy,
#                             year=gcMod$year,
#                             siteid=gcMod$siteid,
#                             spsID=gcMod$spsID))
# 
# spsData <- left_join(spsDay, metDaily, by=c("doy","year","siteid"))
# spsData$spsDayID <- seq(1,nrow(spsData))
# 
# spsIDJoin <- spsData %>%
#   select(doy,year,siteid,spsID,spsDayID)
# 
# gcMod <- left_join(gcMod,spsIDJoin, by=c("doy","year","siteid","spsID"))
# gcMod$gc.mmol.m2.s <- gcMod$gc.mol.m2.s*1000
# # organize data for the model
# datalist <- list(Nobs=nrow(gcMod), 
#                  gs=gcMod$gc.mmol.m2.s,
#                  spsID.obs=gcMod$spsID,
#                  PAR = gcMod$PAR,
#                  spsDay = gcMod$spsDayID,
#                  D = gcMod$D,
#                  NspsDay=nrow(spsData),
#                  SPS=spsData$spsID, 
#                  airTcent = spsData$aveTemp-mean(spsData$aveTemp),
#                  pastpr = spsData$Pr_week,Nparm=3,
#                  NSPS=4)
#                  
# parms <- c( "a", "b", "d","l.slope", "slope.temp", "S","gref","rep.gs","sig.gs")
# 
# init=list(list(
#                a=matrix(c(130,130,130,130,
#                           0.1,0.1,0.1,0.1,
#                           0.01,0.01,0.01,0.01),ncol=4, byrow=TRUE),
#                b=matrix(c(1,1,1,1,
#                           0.01,0.01,0.01,0.01,
#                           0.001,0.001,0.001,0.001),ncol=4, byrow=TRUE),
#                d=matrix(c(-8,-8,-8,-8,
#                           0.01,0.01,0.01,0.01,
#                           0.001,0.001,0.001,0.001),ncol=4, byrow=TRUE),
#                sig.gs=c(100,100,100,100)),
#           list( a=matrix(c(230,230,230,230,
#                            0.01,0.01,0.01,0.01,
#                            0.001,0.001,0.001,0.001),ncol=4, byrow=TRUE),
#                 b=matrix(c(1.2,1.2,1.2,1.2,
#                            -0.01,-0.01,-0.01,-0.01,
#                            -0.001,-0.001,-0.001,-0.001),ncol=4, byrow=TRUE),
#                 d=matrix(c(-5,-5,-5,-5,
#                            -0.01,-0.01,-0.01,-0.01,
#                            -0.001,-0.001,-0.001,-0.001),ncol=4, byrow=TRUE),
#                 sig.gs=c(120,120,120,120)),
#           list(a=matrix(c(180,180,180,180,
#                           -0.01,-0.01,-0.01,-0.01,
#                           -0.001,-0.001,-0.001,-0.001),ncol=4, byrow=TRUE),
#                b=matrix(c(0.8,0.8,0.8,0.8,
#                           0.2,0.2,0.2,0.2,
#                           -0.1,-0.1,-0.1,-0.1),ncol=4, byrow=TRUE),
#                d=matrix(c(-3,-3,-3,-3,
#                           -0.2,-0.2,-0.2,-0.2,
#                           -0.1,-0.1,-0.1,-0.1),ncol=4, byrow=TRUE),
#                sig.gs=c(60,60,60,60)))
# 
# gc_mod <- jags.model(file="/Users/hkropp/Documents/GitHub/ch_shrub_sap/gc_model_code.r",
#                      data=datalist, inits=init,
#                      n.adapt=25000,
#                      n.chains=3)
# 
# gc_sample <- coda.samples(gc_mod, variable.names=parms, n.iter=90000, thin=30)
# 
# MCMCtrace(gc_sample, params=c("a", "b","d", "l.slope","S","gref","sig.gs"),
#           pdf=TRUE, 
#           wd="/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/plots/model",
#           filename="gc_model.pdf")
# 
# out <- MCMCsummary(gc_sample,params=c("a", "b","d","sig.gs"))
# S_out <- MCMCsummary(gc_sample,params=c( "S"))
# gr_out <- MCMCsummary(gc_sample,params=c( "gref"))
# l_out <- MCMCsummary(gc_sample,params=c( "l.slope"))
# grep <- MCMCsummary(gc_sample,params=c( "rep.gs"))
# log_slope <- MCMCsummary(gc_sample,params=c( "slope.temp"))
# plot(grep$mean, gcMod$gc.mmol.m2.s)
# fit <- lm(gcMod$gc.mmol.m2.s ~ grep$mean)
# summary(fit)
# 
# # save results
# write.csv(out, 
# "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/parms_out.csv")
# write.csv(S_out, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/S_out.csv")
# write.csv(gr_out, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/gr_out.csv")
# write.csv(l_out, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/l_out.csv")
# write.csv(grep, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/grep_out.csv")
# 
# write.csv(log_slope, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/log_slope_out.csv")
# 
# 
# 
# outMet <- spsData
# outMet$S <- S_out$mean
# outMet$gref <- gr_out$mean
# outMet$log_slope <- log_slope$mean
# ggplot(outMet, aes(aveTemp, S, color=as.factor(spsID)))+
#   geom_point()
# ggplot(outMet, aes(aveTemp, log_slope, color=as.factor(spsID)))+
#   geom_point()
# ggplot(outMet, aes(aveTemp, gref, color=as.factor(spsID)))+
#   geom_point()
# 
# ggplot(outMet, aes(Pr_week, gref, color=as.factor(spsID)))+
#   geom_point()
# ggplot(outMet, aes(Pr_week, log_slope, color=as.factor(spsID)))+
#   geom_point()
# ggplot(outMet, aes(Pr_week, S, color=as.factor(spsID)))+
#   geom_point()


######## basic hierarchical model with no environmental drivers of parms
# run basic model to ensure that covariate model of parms adds more explanatory
# and predictive power
# organize data for the model
# datalistB <- list(Nobs=nrow(gcMod), 
#                  gs=gcMod$gc.mmol.m2.s,
#                  spsID.obs=gcMod$spsID,
#                  PAR = gcMod$PAR,
#                  spsDay = gcMod$spsDayID,
#                  D = gcMod$D,
#                  NspsDay=nrow(spsData),
#                  SPS=spsData$spsID, 
#                  NSPS=4)
# 
# parmsB <- c( "alpha", "beta", "sig.alpha","delta","sig.delta","l.slope","slope.temp",  "sig.beta", "S","gref","rep.gs","sig.gs")
# 
# initB=list(list(sig.alpha=c(0.1,0.1,0.1,0.1),
#                sig.beta=c(0.01,0.01,0.01,0.01),
#                alpha=c(30,30,30,30),
#                beta=c(1,1,1,1),
#                sig.gs=c(10,10,10,10),
#                delta=c(-4,-4,-4,-4),
#                sig.delta=c(0.1,0.1,0.1,0.1)),
#           list(sig.alpha=c(0.2,0.2,0.2,0.2),
#                sig.beta=c(0.1,0.1,0.1,0.1),
#                alpha=c(200,200,200,200),
#                beta=c(1.2,1.2,1.2,1.2),
#                sig.gs=c(30,30,30,30),
#                delta=c(-1,-1,-1,-1),
#                sig.delta=c(0.5,0.5,0.5,0.5)),
#           list(sig.alpha=c(0.5,0.5,0.5,0.5),
#                sig.beta=c(0.05,0.05,0.05,0.05),
#                alpha=c(500,500,500,500),
#                beta=c(0.8,0.8,0.8,0.8),
#                sig.gs=c(100,100,100,100),
#                delta=c(-6,-6,-6,-6),
#                sig.delta=c(0.7,0.7,0.7,0.7)))
# 
# gc_modB <- jags.model(file="/Users/hkropp/Documents/GitHub/ch_shrub_sap/gc_model_code_basic.r",
#                      data=datalistB, inits=initB,
#                      n.adapt=20000,
#                      n.chains=3)
# 
# gc_sampleB <- coda.samples(gc_modB, variable.names=parmsB, n.iter=90000, thin=30)
# 
# MCMCtrace(gc_sampleB, params=c("alpha", "beta","delta", "sig.alpha", "sig.beta", "sig.delta","S","l.slope","gref","sig.gs"),
#           pdf=TRUE, 
#           wd="/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/plots/model",
#           filename="gc_model_basic.pdf")
# 
# 
# outB <- MCMCsummary(gc_sampleB,params=c("alpha", "beta","delta", "sig.alpha", "sig.beta", "sig.delta","sig.gs"))
# S_outB <- MCMCsummary(gc_sampleB,params=c( "S"))
# gr_outB <- MCMCsummary(gc_sampleB,params=c( "gref"))
# l_outB <- MCMCsummary(gc_sampleB,params=c( "l.slope"))
# grepB <- MCMCsummary(gc_sampleB,params=c( "rep.gs"))
# log_slopeB <- MCMCsummary(gc_sampleB,params=c( "slope.temp"))
# 
# plot(grepB$mean, gcMod$gc.mmol.m2.s)
# fitB <- lm(gcMod$gc.mmol.m2.s ~ grepB$mean)
# summary(fitB)
# summary(fit)
# 
# #similar R2, but hierarchical model has a little more bias.
# 
# 
# # save results
# write.csv(outB, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/basic_model_var/08_13_25/parms_out.csv")
# write.csv(S_outB, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/basic_model_var/08_13_25/S_out.csv")
# write.csv(gr_outB, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/basic_model_var/08_13_25/gr_out.csv")
# write.csv(l_outB, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/basic_model_var/08_13_25/l_out.csv")
# write.csv(grepB, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/basic_model_var/08_13_25/grep_out.csv")
# 
# write.csv(log_slopeB, 
#           "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/basic_model_var/08_13_25/log_slope_out.csv")

#######################################################
######## read in model results          ###############


# save results
out <- read.csv( 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/parms_out.csv")
S_out <- read.csv( 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/S_out.csv")
gr_out <- read.csv( 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/gr_out.csv")
l_out <- read.csv( 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/l_out.csv")
grep <- read.csv( 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/grep_out.csv")

log_slope <- read.csv( 
          "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/model_output/full_model_var/08_13_25_15_36/log_slope_out.csv")



outMet <- spsData
outMet$S <- S_out$mean
outMet$gref <- gr_out$mean
outMet$log_slope <- log_slope$mean
