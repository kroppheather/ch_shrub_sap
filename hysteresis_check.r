###################################################################################
########################  2016-2017 SIBERIA SHRUB SAPFLOW  ########################
########################  Low Density and German Site      ########################
###################################################################################
######################## check for hysteresis in sap flow  ########################
###################################################################################
######################## inputs:                           ########################
######################## sensor (list) sensor info         ########################
######################## sapFlow (list) sapflow g m-2 s-1  ########################
######################## list item 1 is flood plain and    ########################
######################## list item 2 is low density        ########################
###################################################################################

##################################
# sap flow data                  #
##################################
source("c:\\Users\\hkropp\\Documents\\GitHub\\ch_shrub_sap\\sap_flow_process.r")

##################################
# set up directories             #
##################################
#set up plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\ch_shrub\\hysteresis"
siteName <- c("floodplain","upland")
##################################
# library                        #
##################################
library(RColorBrewer)

##################################
# other data                     #
##################################
#read in met data
datRH <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\decagon\\met\\RH.VP4.csv")
datTC <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\decagon\\met\\TempC.VP4.csv")
datG <- read.csv("z:\\data_repo\\field_data\\viperData\\German_met.csv")


#join RH and TC
datM <- join(datRH, datTC, by=c("doy","year","hour","site"), type="inner")
datL <- datM[datM$site=="ld"&datM$year==2016,]
datLR <- data.frame(doy=datL$doy,year=datL$year,hour=datL$hour,RH=datL$RH*100,temp=datL$TempC.VP4)


#date and time for G
dateG <- as.Date(datG$Date.Time, "%d.%m.%Y %H:%M")
datG$doy <- yday(dateG)
datG$year <- year(dateG)
datG$hour <- as.numeric(substr(datG$Date.Time,12,13))+(as.numeric(substr(datG$Date.Time,15,16))/60)

#calculate vpd for each site
#saturated water vapor
datLR$e.sat <- 0.611*exp((17.502*datLR$temp)/(datLR$temp+240.97))
datG$e.sat <- 0.611*exp((17.502*datG$temp)/(datG$temp+240.97))

#fix any RH greater than 100
datLR$RHf <- ifelse(datLR$RH>99.9,99.9,datLR$RH)
datG$RHf <- ifelse(datG$RH>99.9,99.9,datG$RH)
#calculate vapor pressure deficit
datLR$D <- datLR$e.sat-((datLR$RHf/100)*datLR$e.sat)
datG$D <- datG$e.sat-((datG$RHf/100)*datG$e.sat)


met <- list(datG,datLR)
#join into sapflow tables


for(i in 1:2){
	sapFlow[[i]] <- join(sapFlow[[i]],met[[i]], by=c("doy","hour"), type="left")

}


##################################
# calculate vapor pressure       #
##################################

#make a data.frame of days to plot hysteresis over for each site
days <- list()
maxD <- list()
minD <- list()
maxS <- list()
minS <- list()
for(i in 1:2){
	days[[i]] <- unique(data.frame(doy=sapFlow[[i]]$doy))
	maxD[[i]] <- aggregate(sapFlow[[i]]$D,by=list(sapFlow[[i]]$doy),FUN="max",na.rm=TRUE)
	colnames(maxD[[i]]) <- c("doy","maxD")
	days[[i]] <- join(days[[i]],maxD[[i]], by="doy",type="left")
	minD[[i]] <- aggregate(sapFlow[[i]]$D,by=list(sapFlow[[i]]$doy),FUN="min",na.rm=TRUE)
	colnames(minD[[i]]) <- c("doy","minD")
	days[[i]] <- join(days[[i]],minD[[i]], by="doy",type="left")
	maxS[[i]] <- aggregate(sapFlow[[i]]$sapF,by=list(sapFlow[[i]]$doy),FUN="max",na.rm=TRUE)
	colnames(maxS[[i]]) <- c("doy","maxS")
	days[[i]] <- join(days[[i]],maxS[[i]], by="doy",type="left")
	minS[[i]] <- aggregate(sapFlow[[i]]$sapF,by=list(sapFlow[[i]]$doy),FUN="min",na.rm=TRUE)
	colnames(minS[[i]]) <- c("doy","minS")
	days[[i]] <- join(days[[i]],minS[[i]], by="doy",type="left")
	days[[i]] <- days[[i]][days[[i]]$minD!=Inf,]
}

#convert sapflow zeros to NA
for(i in 1:2){
	sapFlow[[i]]$sapFz <- ifelse(sapFlow[[i]]$sapF==0,NA,sapFlow[[i]]$sapF)
}


#make plots of the sensors on each day
#using a diverging palette
coli <- c(rev(brewer.pal(8,"Blues")),brewer.pal(8,"Oranges"))
nameS <- c("floodplain","upland")

for(i in 1:2){
	
	for(k in 1:dim(days[[i]])[1]){
		png(paste0(plotDI,"\\",nameS[[i]],"_doy",days[[i]]$doy[k],".png"))
			plot(c(0,1),c(0,1), type="n", xlim=c(days[[i]]$minD[k],days[[i]]$maxD[k]),
				ylim=c(days[[i]]$minS[k],days[[i]]$maxS[k]), xlab="VPD",ylab="sapflow",xaxs="i",yaxs="i")
			for(j in 1:16){	
				points(sapFlow[[i]]$D[sapFlow[[i]]$sensor==j&sapFlow[[i]]$doy==days[[i]]$doy[k]],
				sapFlow[[i]]$sapFz[sapFlow[[i]]$sensor==j&sapFlow[[i]]$doy==days[[i]]$doy[k]],
				type="b",pch=19,col=coli[j])
				
			}
	dev.off()		
	}
}

#check by hour

for(i in 1:2){
	
	for(k in 1:dim(days[[i]])[1]){
		png(paste0(plotDI,"\\hour",nameS[[i]],"_doy",days[[i]]$doy[k],".png"))
			plot(c(0,1),c(0,1), type="n", xlim=c(0,24),
				ylim=c(days[[i]]$minS[k],days[[i]]$maxS[k]), xlab="VPD",ylab="sapflow",xaxs="i",yaxs="i")
			for(j in 1:16){	
				points(sapFlow[[i]]$hour[sapFlow[[i]]$sensor==j&sapFlow[[i]]$doy==days[[i]]$doy[k]],
				sapFlow[[i]]$sapFz[sapFlow[[i]]$sensor==j&sapFlow[[i]]$doy==days[[i]]$doy[k]],
				type="b",pch=19,col=coli[j])
				
			}
	dev.off()		
	}
}
sapFlow[[2]][sapFlow[[2]]$sensor==2&sapFlow[[2]]$doy==days[[2]]$doy[8],]
