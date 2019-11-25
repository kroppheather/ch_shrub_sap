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
source("c:\\Users\\hkropp\\Documents\\GitHub\\ch_shrub_sap\\sap_flow_process.r")

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
plotDir <- "c:\\Users\\hkropp\\Google Drive\\ch_shrub\\plots\\poster"


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
##################################
# plots                          #
##################################

#################################################################
################## Daily Transpiration   ########################
#################################################################

coli <- c(rgb(0,114,178, maxColorValue = 255), #alnus floodplain
			rgb(213,94,0, maxColorValue = 255), #salix floodplain
			rgb(0,158,115, maxColorValue = 255), #betula upland
			rgb(230,159,0, maxColorValue = 255)) #salix upland

wd <- 15
hd <- 15
xl <- 180
xh <- 240
yl <- 0
yh <- 5


png(paste0(plotDir,"\\Tday.png"), width = 17, height = 17, units = "cm", res=300)
	layout(matrix(c(1),ncol=1), width=lcm(wd),height=lcm(hd))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
		xlab= " ", ylab=" ", axes=FALSE)
	for(i in 1:4){	
		points(tdayDF$doy[tdayDF$spsID==i],tdayDF$L.m2.day[tdayDF$spsID==i], pch=19, col=coli[i],
			type="b")
		arrows(tdayDF$doy[tdayDF$spsID==i],tdayDF$L.m2.day[tdayDF$spsID==i]-tdayDF$L.m2.daySD[tdayDF$spsID==i],
		tdayDF$doy[tdayDF$spsID==i],tdayDF$L.m2.day[tdayDF$spsID==i]+tdayDF$L.m2.daySD[tdayDF$spsID==i],code=0, col=rgb(0.5,0.5,0.5,0.5))
	}
		
		
dev.off()	
	