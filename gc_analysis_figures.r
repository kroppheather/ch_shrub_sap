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
# read in allometry data         #
##################################	
alloUc <- read.csv("c:\\Users\\hkropp\\Google Drive\\ch_shrub\\2012 - 2017 Density Gradient Shrubs.csv")
alloUc  <- alloUc[substr(alloUc$Site,1,1) == "L",]



alloF <- read.csv("c:\\Users\\hkropp\\Google Drive\\ch_shrub\\flood_allom.csv")

#only look at whole plant allometry 
alloF <- alloF[alloF$type == "plant",]


##################################
# Vegetation colors for plotting #
##################################

coli <- c(rgb(0,114,178, maxColorValue = 255), #alnus floodplain
			rgb(213,94,0, maxColorValue = 255), #salix floodplain
			rgb(0,158,115, maxColorValue = 255), #betula upland
			rgb(230,159,0, maxColorValue = 255)) #salix upland
			
			
colDF <- data.frame(spsID= seq(1,4), 
		species=c("Alnus","Salix","Betula","Salix"), 
		siteid=c(1,1,2,2),
		coli=coli,
		col2=c(rgb(0,114,178,127, maxColorValue = 255), #alnus floodplain
				rgb(213,94,0,127, maxColorValue = 255), #salix floodplain
				rgb(0,158,115,127, maxColorValue = 255), #betula upland
				rgb(230,159,0,127, maxColorValue = 255)), #salix upland
		col3=c(rgb(0,114,178,50, maxColorValue = 255), #alnus floodplain
				rgb(213,94,0,50, maxColorValue = 255), #salix floodplain
				rgb(0,158,115,50, maxColorValue = 255), #betula upland
				rgb(230,159,0,50, maxColorValue = 255)), #salix upland	

		col4=c(rgb(0,114,178,80, maxColorValue = 255), #alnus floodplain
				rgb(213,94,0,80, maxColorValue = 255), #salix floodplain
				rgb(0,158,115,80, maxColorValue = 255), #betula upland
				rgb(230,159,0,80, maxColorValue = 255)) #salix upland	
				
		)
##################################
# organize allometry data        #
##################################	
#pull out only low density sites
#all site ids start with L for low density


flAllo <- alloF[alloF$site == "y4" | alloF$site == "amb",] 

upAllo <- alloF[alloF$site == "exp" | alloF$site == "ldf",] 
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
				
#summarize met information for daily timescales
Tday <- aggregate(metDF$temp, by=list(metDF$doy,metDF$siteid), FUN="mean", na.rm=TRUE)				
colnames(Tday) <- c("doy","siteid","Tday")

#average daily vpd
VPday <- aggregate(metDF$D, by=list(metDF$doy,metDF$siteid), FUN="mean", na.rm=TRUE)
colnames(VPday) <- c("doy","siteid","Dday")

Prday <- 	unique(data.frame(doy=metDF$doy,siteid=metDF$siteid, Prday=metDF$Pr.mm))								
metDay <- full_join(Tday,Prday, by=c("doy","siteid"))	
metDay <- full_join(metDay, VPday, by=c("doy","siteid"))
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

wd <- 35
hd <- 15
xl <- 180
xh <- 240.5
yl2 <- 0
yh2 <- 5

yl <- 0
yh <- 25
prMax <- 35
prScale <- yh/prMax


png(paste0(plotDir,"\\Tday.png"), width = 37, height = 35, units = "cm", res=300)
	layout(matrix(c(1,2),ncol=1), width=lcm(wd),height=rep(lcm(hd),2))
	
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
		xlab= " ", ylab=" ", axes=FALSE)	
	for(i in 1:dim(metDay)[1]){
		polygon(c(metDay$doy[i]-0.25,metDay$doy[i]-0.25,metDay$doy[i]+0.25,metDay$doy[i]+0.25),
			c(0,metDay$Prday[i]*prScale,metDay$Prday[i]*prScale,0), border=NA, col=rgb(115,194,251,100,maxColorValue=255))
	}	
	
	points(metDay$doy[metDay$siteid == 2], metDay$Tday[metDay$siteid == 2], pch=19, type="b")

	
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl2,yh2), xaxs="i",yaxs="i",
		xlab= " ", ylab=" ", axes=FALSE)
	for(i in 1:4){	
		points(tdayDF$doy[tdayDF$spsID==i],tdayDF$L.m2.day[tdayDF$spsID==i], pch=19, col=coli[i],
			type="b")
		arrows(tdayDF$doy[tdayDF$spsID==i],tdayDF$L.m2.day[tdayDF$spsID==i]-tdayDF$L.m2.daySD[tdayDF$spsID==i],
		tdayDF$doy[tdayDF$spsID==i],tdayDF$L.m2.day[tdayDF$spsID==i]+tdayDF$L.m2.daySD[tdayDF$spsID==i],code=0, col=rgb(0.5,0.5,0.5,0.5))
	}
		
		
dev.off()	
	
	
	
	
	
#################################################################
################## Whole plant T         ########################
#################################################################	

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

tdayDF$jitteri <- runif(nrow(tdayDF),0,0.5)


#get quantiles
tquant <- list()
for(i in 1:4){
	tquant[[i]] <- quantile(tdayDF$L.plant.day[tdayDF$spsID == i], prob=c(0.025,0.25,0.50,0.75,0.975))

}

wd <- 15
hd <- 15
xl <- .75
xh <- 4.75
yl <- 0
yh <- 0.7




png(paste0(plotDir,"\\Tplant.png"), width = 17, height = 17, units = "cm", res=300)
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
		xlab= " ", ylab=" ", axes=FALSE)	
	for(i in 1:4){
	points(tdayDF$spsID[tdayDF$spsID == i] +tdayDF$jitteri[tdayDF$spsID == i], tdayDF$L.plant.day[tdayDF$spsID == i], pch=19,col=as.character(colDF$col4[i]))
	arrows(i+0.25,tquant[[i]][1],i+0.25,tquant[[i]][5], code=0, lwd=2, col=as.character(colDF$col3[i]))
	}
	
	for(i in 1:4){
	polygon(c(i,i,i+0.5,i+0.5),
			c(tquant[[i]][2],tquant[[i]][4],tquant[[i]][4],tquant[[i]][2]), border=NA, col=as.character(colDF$col3[i]))
	
	arrows(i,tquant[[i]][3],i+0.5,tquant[[i]][3], code=0, lwd=4, col=coli[i])
	}
	
	
dev.off()	
	
	
#################################################################
################## Patterns in stomatal conductance##############
#################################################################	
#unlist gc

gcDF <- rbind(gcSpec[[1]],gcSpec[[2]])

gcDF <- left_join(gcDF, metDF, by=c("hour","doy","year", "siteid"))

gcDF$spsID <- ifelse(gcDF$siteid==1&gcDF$species == "Alnus",1,
				ifelse(gcDF$siteid==1&gcDF$species == "Salix",2,
				ifelse(gcDF$siteid==2&gcDF$species == "Betula",3,
				ifelse(gcDF$siteid==2&gcDF$species == "Salix",4,NA))))
				
#just look at VPD
#since Par has a strong influence exclude low PAR under 200
gcDF <- gcDF[gcDF$PAR > 150,]

				
#aggregate to figure out how many daily observations
gcDayN <- 	aggregate(gcDF$gc.mol.m2.s, by=list(gcDF$doy, gcDF$spsID), FUN="length")			
colnames(gcDayN) <- c("doy","spsID","Ngc")		
#subset to exclude days with only a few observations
gcDayN <- gcDayN[gcDayN$Ngc > 6,]
#join back into dataframe to subset		
gcDF2 <- inner_join(gcDF, gcDayN, by=c("doy","spsID"))


#fit regression
fit <- list()
Int <- numeric()
Slope <- numeric()
minD <- numeric(0)
maxD <- numeric(0)

for(i in 1:nrow(gcDayN)){
	fit[[i]] <- lm(gcDF2$gc.mol.m2.s[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]] ~ 
					log(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]]))
	Int[i] <- coefficients(fit[[i]])[1]
	Slope[i] <- coefficients(fit[[i]])[2] 
	minD[i] <- min(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]])
	maxD[i] <- max(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]])
}

gcDayN1 <- which(gcDayN$spsID == 1 ) 
gcDayN2 <- which(gcDayN$spsID == 2 ) 
gcDayN3 <- which(gcDayN$spsID == 3 ) 
gcDayN4 <- which(gcDayN$spsID == 4 ) 

wd <- 15
hd <- 15
xl <- 0.5
xh <- 2.75


yl <- 0
yh <- 0.75



png(paste0(plotDir,"\\gcDay.png"), width = 35, height = 20, units = "cm", res=300)
	layout(matrix(c(1,2),ncol=2), width=rep(lcm(wd),2),height=lcm(hd))
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
			xlab= " ", ylab=" ", axes=FALSE)	
		for(i in gcDayN1){
		if(Slope[i] < 0){
			points(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]], pch=19, col=as.character(colDF$col3[1]))
		}
		}
		for(i in gcDayN1){
		if(Slope[i] < 0){
		points(seq(minD[i],maxD[i], length.out=100), Int[i] + (Slope[i]*log(seq(minD[i],maxD[i], length.out=100))), type="l", col=as.character(colDF$coli[1]),
		lwd=2)
		}
		}
		for(i in gcDayN2){
		if(Slope[i] < 0){
			points(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]], pch=19, col=as.character(colDF$col3[2]))
		}
		}
		for(i in gcDayN2){
		if(Slope[i] < 0){
		points(seq(minD[i],maxD[i], length.out=100), Int[i] + (Slope[i]*log(seq(minD[i],maxD[i], length.out=100))), type="l", col=as.character(colDF$coli[2]),
		lwd=2)
		}
		}
		par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
			xlab= " ", ylab=" ", axes=FALSE)	
			
		for(i in gcDayN3){
		if(Slope[i] < 0){
			points(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]], pch=19, col=as.character(colDF$col3[3]))
		}
		}
		for(i in gcDayN3){
		if(Slope[i] < 0){
		points(seq(minD[i],maxD[i], length.out=100), Int[i] + (Slope[i]*log(seq(minD[i],maxD[i], length.out=100))), type="l", col=as.character(colDF$coli[3]),
		lwd=2)
		}
		}
		for(i in gcDayN4){
		if(Slope[i] < 0){
			points(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]], pch=19, col=as.character(colDF$col3[4]))
		}
		}
		for(i in gcDayN4){
		if(Slope[i] < 0){
		points(seq(minD[i],maxD[i], length.out=100), Int[i] + (Slope[i]*log(seq(minD[i],maxD[i], length.out=100))), type="l", col=as.character(colDF$coli[4]),
		lwd=2)
		}
		}
dev.off()	



