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
source("c:\\Users\\cpham\\Documents\\GitHub\\ch_shrub_sap\\sap_flow_process.r")
library(rjags)
library(plyr)
library(dplyr)
#read in met data

datRH <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\decagon\\met\\RH.VP4.csv")
datTC <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\decagon\\met\\TempC.VP4.csv")
metG <- read.csv("z:\\data_repo\\field_data\\viperData\\German_met.csv")
datAir <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\airport\\airport.csv")
datQa <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\decagon\\met\\PAR.QSOS Par.csv")
datQa <- datQa[datQa$site=="ld",]

#take only revlant info for par and rename to simplier
datQ <- data.frame(datQa[,1:3], PAR=datQa$PAR.QSOS.Par)
##################################
# set up directories             #
##################################
plotDir <- "z:\\student_research\\cpham\\Plot"


##################################
# organize met data              #
##################################	
#join RH and TC
datM <- inner_join(datRH, datTC, by=c("doy","year","hour","site"))
datLt <- datM[datM$site=="ld"&datM$year==2016,]
datLR <- data.frame(doy=datLt$doy,year=datLt$year,hour=datLt$hour,RH=datLt$RH*100,temp=datLt$TempC.VP4)


#date and time for G
dateG <- as.Date(metG$Date.Time, "%d.%m.%Y %H:%M")
metG$doy <- yday(dateG)
metG$year <- year(dateG)
metG$hour <- as.numeric(substr(metG$Date.Time,12,13))+(as.numeric(substr(metG$Date.Time,15,16))/60)

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

#add a site id
#1=floodplain, 2= upland

met <- list(metG,datLR)
for(i in 1:2){
	met[[i]]$siteid <- rep(i, dim(met[[i]])[1])
}

met2 <- list()
for(i in 1:2){
	met2[[i]] <- met[[i]][,order(names(met[[i]]))]
}


#turn into a data frame
metDF <- rbind(cbind(D=met2[[1]]$D,met2[[1]][,3:13]),met2[[2]])

#join in PAR data
metDF <- left_join(metDF,datQ, by=c("doy","year","hour"))

metDF <- metDF[metDF$year == 2016,]

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
daysAll <- aggregate(gcAll$gc.mol.m2.s, by=list(gcAll$doy,gcAll$year,gcAll$species,gcAll$siteid),FUN="length")
colnames(daysAll) <- c("doy","year","species","siteid", "nGc")

#exclude days with too few observations
daysAll <- daysAll[daysAll$nGc>=5,]


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


#merge the list of gcSpec into a single dataframe
# gcSpec[[1]]$site <- rep(1, nrow(gcSpec[[1]]))
# gcSpec[[2]]$site <- rep(2, nrow(gcSpec[[2]]))
gc_df <- rbind(gcSpec[[1]],  gcSpec[[2]])

#get vpd at half hour rate
met[[1]]$siteid <- rep(1, nrow(met[[1]]))
met[[2]]$siteid <- rep(2, nrow(met[[2]]))

#create met_df, retaining only "site", "hour", doy", "year", temp", "D"
first_met <- met[[1]][,c("siteid", "hour", "doy", "year", "temp", "D")]
second_met <- met[[2]][,c("siteid", "hour", "doy", "year", "temp", "D")]
met_df_1 <- rbind(first_met, second_met)

#match with gc_df by "site", "doy", "year", 
met_gc <- met_df_1%>% right_join(gc_df, by=c("doy","year","hour","siteid"))

#average daily air temperature for met_df
met_daily_avg <- ddply(met_df_1, .(doy, year), summarize, daily_avg_temp = mean(temp))
met_df_2 <- met_df_1%>% right_join(met_daily_avg, by=c("doy","year"))

#added Pr.mm values from datAir into met_df
met_df <- datAir %>%select("doy", "year", "Pr.mm")%>% right_join(met_df_2, by=c("doy","year"))
met_df <- met_df[,c("hour", "doy", "year","siteid","temp", "D", "daily_avg_temp", "Pr.mm")]

#add spsIDs (site-species) and spsdIDs (site-species-doy) to gc_df
gc_df$spsID <- ifelse(gc_df$siteid==1&gc_df$species == "Alnus",1,
               ifelse(gc_df$siteid==1&gc_df$species == "Salix",2,
               ifelse(gc_df$siteid==2&gc_df$species == "Betula",3,
               ifelse(gc_df$siteid==2&gc_df$species == "Salix",4,NA))))

gc_df <- gc_df %>% 
  mutate(spsdID = group_indices(., spsID, doy))

#join met_df into gc_df
gc_df <- gc_df%>% right_join(met_df %>%select("hour", "doy", "year","temp", "daily_avg_temp", "Pr.mm", "D"),
                             by=c("hour", "doy","year"))

#remove datasets met_df_1 and met_df_2
rm(c="met_df_1", "met_df_2")

