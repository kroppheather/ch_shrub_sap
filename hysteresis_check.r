###################################################################################
########################  2016-2017 SIBERIA SHRUB SAPFLOW  ########################
########################  Low Density and German Site      ########################
###################################################################################
######################## check for hysteresis in sap flow  ########################
###################################################################################
######################## inputs:                           ########################
######################## sensor (list) sensor info         ########################
######################## sapFlow (list) sapflow g m-2 s-1  ########################
######################## specFlow is species averager sap  ######################## 
######################## flow                              ######################## 
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
datQa <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\decagon\\met\\PAR.QSOS Par.csv")
datQa <- datQa[datQa$site=="ld",]

#take only revlant info for par and rename to simplier
datQ <- data.frame(datQa[,1:3], PAR=datQa$PAR.QSOS.Par)

#join RH and TC
datM <- inner_join(datRH, datTC, by=c("doy","year","hour","site"))
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

#join light into met. Using low density for both sites
datLR <- left_join(datLR,datQ, by=c("doy","hour","year"))
datG <- left_join(datG,datQ, by=c("doy","hour","year"))
met <- list(datG,datLR)
#join into sapflow tables


for(i in 1:2){
	sapFlow[[i]] <- left_join(sapFlow[[i]],met[[i]], by=c("doy","hour"))
	specFlow[[i]] <- left_join(specFlow[[i]],met[[i]], by=c("doy","hour"))

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
	days[[i]] <- left_join(days[[i]],maxD[[i]], by="doy")
	minD[[i]] <- aggregate(sapFlow[[i]]$D,by=list(sapFlow[[i]]$doy),FUN="min",na.rm=TRUE)
	colnames(minD[[i]]) <- c("doy","minD")
	days[[i]] <- left_join(days[[i]],minD[[i]], by="doy")
	maxS[[i]] <- aggregate(sapFlow[[i]]$sapF,by=list(sapFlow[[i]]$doy),FUN="max",na.rm=TRUE)
	colnames(maxS[[i]]) <- c("doy","maxS")
	days[[i]] <- left_join(days[[i]],maxS[[i]], by="doy")
	minS[[i]] <- aggregate(sapFlow[[i]]$sapF,by=list(sapFlow[[i]]$doy),FUN="min",na.rm=TRUE)
	colnames(minS[[i]]) <- c("doy","minS")
	days[[i]] <- left_join(days[[i]],minS[[i]], by="doy")
	days[[i]] <- days[[i]][days[[i]]$minD!=Inf,]
}



#make plots of the sensors on each day
#using a diverging palette
coli <- c(rev(brewer.pal(8,"Blues")),brewer.pal(8,"Oranges"))
nameS <- c("floodplain","upland")

for(i in 1:2){
	
	for(k in 1:dim(days[[i]])[1]){
		png(paste0(plotDI,"\\sensors\\D\\",nameS[[i]],"_doy",days[[i]]$doy[k],".png"))
			plot(c(0,1),c(0,1), type="n", xlim=c(days[[i]]$minD[k],days[[i]]$maxD[k]),
				ylim=c(days[[i]]$minS[k],days[[i]]$maxS[k]), xlab="VPD",ylab="sapflow")
			for(j in 1:16){	
				points(sapFlow[[i]]$D[sapFlow[[i]]$sensor==j&sapFlow[[i]]$doy==days[[i]]$doy[k]],
				sapFlow[[i]]$sapF[sapFlow[[i]]$sensor==j&sapFlow[[i]]$doy==days[[i]]$doy[k]],
				type="b",pch=19,col=coli[j])
				
			}
	dev.off()		
	}
}

#check by hour

for(i in 1:2){
	
	for(k in 1:dim(days[[i]])[1]){
		png(paste0(plotDI,"\\sensors\\hour\\",nameS[[i]],"_doy",days[[i]]$doy[k],".png"))
			plot(c(0,1),c(0,1), type="n", xlim=c(0,24),
				ylim=c(days[[i]]$minS[k],days[[i]]$maxS[k]), xlab="VPD",ylab="sapflow")
			for(j in 1:16){	
				points(sapFlow[[i]]$hour[sapFlow[[i]]$sensor==j&sapFlow[[i]]$doy==days[[i]]$doy[k]],
				sapFlow[[i]]$sapF[sapFlow[[i]]$sensor==j&sapFlow[[i]]$doy==days[[i]]$doy[k]],
				type="b",pch=19,col=coli[j])
				
			}
	dev.off()		
	}
}

days2 <- list()
for(i in 1:2){
	days2[[i]] <- unique(data.frame(doy=specFlow[[i]]$doy))

	

}
days2[[2]] <- data.frame(doy=days2[[i]]$doy[days2[[i]]$doy>183])
cols <- c("tomato3","cornflowerblue")	

#get the maximum sap flow and light for plotting
maxQ <- list()
maxS <- list()
maxD <- list()
for(i in 1:2){
	maxQ[[i]] <- aggregate(specFlow[[i]]$PAR, by=list(specFlow[[i]]$doy),FUN="max",na.rm=TRUE)
	colnames(maxQ[[i]]) <- c("doy","PARmax")
	maxS[[i]] <- aggregate(specFlow[[i]]$sapF, by=list(specFlow[[i]]$doy),FUN="max",na.rm=TRUE)
	colnames(maxS[[i]]) <- c("doy","Sapmax")
	maxD[[i]] <- aggregate(specFlow[[i]]$D, by=list(specFlow[[i]]$doy),FUN="max",na.rm=TRUE)
	colnames(maxD[[i]]) <- c("doy","Dmax")
}	

#join backinto days 2
for(i in 1:2){
	days2[[i]] <- left_join(days2[[i]],maxQ[[i]],by="doy")
	days2[[i]] <- left_join(days2[[i]],maxS[[i]],by="doy")
	days2[[i]] <- left_join(days2[[i]],maxD[[i]],by="doy")
	days2[[i]]$Sp <- days2[[i]]$Sapmax+0.05
	days2[[i]]$Pp <- days2[[i]]$PARmax+50
	days2[[i]]$scaleF <- days2[[i]]$Sp/days2[[i]]$Pp
	days2[[i]]$Dp <- days2[[i]]$Dmax+0.25
	days2[[i]]$scaleD <- days2[[i]]$Sp/days2[[i]]$Dp
}

#get unique species data frame
specName <- list()
for(i in 1:2){
	specName[[i]] <- unique(data.frame(species=specFlow[[i]]$species))
}
#look at species average
for(i in 1:2){
	for(k in 1:dim(days2[[i]])[1]){
	png(paste0(plotDI,"\\species\\D\\",nameS[[i]],"_doy",days[[i]]$doy[k],".png"))
		plot(specFlow[[i]]$D[specFlow[[i]]$doy==days2[[i]]$doy[k]],
			specFlow[[i]]$sapF[specFlow[[i]]$doy==days2[[i]]$doy[k]], 
			type="n",
			xlab="VPD",ylab="sapflow")
		for(j in 1:2){
			points(specFlow[[i]]$D[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			specFlow[[i]]$sapF[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			pch=19,col=cols[j],type="b")
		}
	dev.off()	
	}	
}	

#look at species average
for(i in 1:2){
	for(k in 1:dim(days2[[i]])[1]){
	png(paste0(plotDI,"\\species\\hour\\",nameS[[i]],"_doy",days[[i]]$doy[k],".png"))
		plot(specFlow[[i]]$hour[specFlow[[i]]$doy==days2[[i]]$doy[k]],
			specFlow[[i]]$sapF[specFlow[[i]]$doy==days2[[i]]$doy[k]], 
			type="n",
			xlab="hour",ylab="sapflow")
		for(j in 1:2){
			points(specFlow[[i]]$hour[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			specFlow[[i]]$sapF[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			pch=19,col=cols[j],type="b")
		}
	dev.off()	
	}	
}


#look at species average
for(i in 1:2){
	for(k in 1:dim(days2[[i]])[1]){
	png(paste0(plotDI,"\\species\\PAR\\",nameS[[i]],"_doy",days[[i]]$doy[k],".png"))
		plot(specFlow[[i]]$PAR[specFlow[[i]]$doy==days2[[i]]$doy[k]],
			specFlow[[i]]$sapF[specFlow[[i]]$doy==days2[[i]]$doy[k]], 
			type="n",
			xlab="PAR",ylab="sapflow")
		for(j in 1:2){
			points(specFlow[[i]]$PAR[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			specFlow[[i]]$sapF[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			pch=19,col=cols[j],type="b")
		}
	dev.off()	
	}	
}

#look at species average by hour and with light on the same plot

for(i in 1:2){
	for(k in 1:dim(days2[[i]])[1]){
	png(paste0(plotDI,"\\species\\day\\",nameS[[i]],"_doy",days[[i]]$doy[k],".png"), width = 600, height = 600,)
	par(mai=c(1,1,1,1))
		plot(specFlow[[i]]$hour[specFlow[[i]]$doy==days2[[i]]$doy[k]],
			specFlow[[i]]$sapF[specFlow[[i]]$doy==days2[[i]]$doy[k]], 
			type="n",
			xlab="hour",ylab="sapflow", ylim=c(0,days2[[i]]$Sp[k]),lwd=2,cex=2,
			main=paste(nameS[i],days2[[i]]$doy[k]))
		for(j in 1:2){
			points(specFlow[[i]]$hour[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			specFlow[[i]]$sapF[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			pch=19,col=cols[j],type="b",lwd=2,cex=2)
			points(specFlow[[i]]$hour[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			specFlow[[i]]$PAR[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]]*days2[[i]]$scaleF[k],
			pch=17,col="mediumpurple",type="b", lty=3,lwd=2,cex=2)
			points(specFlow[[i]]$hour[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]],
			specFlow[[i]]$D[specFlow[[i]]$doy==days2[[i]]$doy[k]&specFlow[[i]]$species==specName[[i]]$species[j]]*days2[[i]]$scaleD[k],
			pch=18,col="palegreen3",type="b", lty=3,lwd=2,cex=2)
		}
		legend("topleft",c(as.character(specName[[i]]$species),"D","PAR"),col=c(cols,"palegreen3","mediumpurple"),
					pch=c(19,19,18,17),lty=c(1,1,3,3),lwd=2,cex=1.5,bty="n")
		axis(4, seq(0,days2[[i]]$Sp[k],length.out=5),round(seq(0,days2[[i]]$Pp[k],length.out=5)),col.axis="mediumpurple")
		mtext(round(seq(0,days2[[i]]$Dp[k],length.out=5),2), at=seq(0,days2[[i]]$Sp[k],length.out=5),side=4,line=2,col="palegreen3")
	dev.off()	
	}	
}

