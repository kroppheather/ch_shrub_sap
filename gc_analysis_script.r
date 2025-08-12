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


##################################
# organize met data              #
##################################	
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

##################################
# organize data                  #
##################################
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
