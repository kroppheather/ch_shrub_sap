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
# set up directories             #
##################################


plotdir <- "c:\\Users\\hkropp\\Google Drive\\ch_shrub\\diag"

##################################
# indicate if making plots       #
##################################
#turn on plotting
#for sensor output diagnostics
#1 = make plots, 0 = don't make plots
plotcheck <- 0

##################################
# packages                       #
##################################
library(dplyr)
library(lubridate)

##################################
# read in data                   #
##################################
#German site files (floodplain)
datG <-read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\campbell\\sapflow\\ger_f32_2016.csv")
gLA <-read.csv("z:\\data_repo\\field_data\\viperData\\sapflow\\German_la.csv")



#LD files (upland)
datL <- read.csv ("z:\\data_repo\\field_data\\viperData\\sensor\\campbell\\sapflow\\ld_f32_2016.csv")
lLA <- read.csv("z:\\data_repo\\field_data\\viperData\\sapflow\\LD_la.csv")


#set up decimal days for plotting
datG$timeDD <- datG$doy+(datG$hour/24)
datL$timeDD <- datL$doy+(datL$hour/24)

#read in met data for gs calculations

#read in met data
datRH <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\decagon\\met\\RH.VP4.csv")
datTC <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\decagon\\met\\TempC.VP4.csv")
metG <- read.csv("z:\\data_repo\\field_data\\viperData\\German_met.csv")
datAir <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\airport\\airport.csv")
###################################################################################
###################################################################################
######## Part 1: calculate sap flow from data logger data                  ########    
###################################################################################
###################################################################################

##################################
# sap flow calculation           #
##################################
#pull out matrix of raw data for the sensors
#for each site
#first site is always german
C <- list(datG[,6:21],datL[,6:21])
B <- list(datG[,22:37],datL[,22:37])
A <- list(datG[,38:53],datL[,38:53])
Pin <-list(datG[,166:181],datL[,151:166])
#change in temperature
dT <- list(datG[,118:133],datL[,103:118])
#stem area cm squared
SA <- list(gLA$stem.area,lLA$stem.area)
#time info
Time <- list(datG[,1:3],datL[,1:3])

#sensor info
sensor <- list(gLA,lLA)

#calculations:

#Qv = Kst * SA (B-A)/ dx * 0.40 mV/C
#Kst:thermal conductivity of stem 0.42 W/m K for woody
#SA stem area in cm2
#B= B_mv output by sensor
#A= A_mv output by sensor
#dx sapflow thermocouple gap in cm = 40 for SGA9 and SGA10 sensors used here

Qv <- list()
Qvtemp1 <- list()
for(i in 1:2){
	for(j in 1:16){
		#Qv calculation for each sensor
		Qvtemp1[[j]] <-	(0.42*SA[[i]][j]*(B[[i]][,j]-A[[i]][,j]))/(40*0.4)
	
	}
	#save each site into its own list as a matrix
	Qv[[i]] <- matrix(unlist(Qvtemp1),byrow=FALSE,ncol=16)

}


#calculate Ksh apparent 
# Kshapp= (Pin-Qv)/C
Kshapptemp <- list()
Kshapp <- list()
for(i in 1:2){
	for(j in 1:16){
		Kshapptemp[[j]] <- (Pin[[i]][,j]-Qv[[i]][,j])/C[[i]][,j]
	}
	Kshapp[[i]] <- matrix(unlist(Kshapptemp),byrow=FALSE,ncol=16)
}
# need to take daily minimum
Kshapptemp2 <- data.frame()
Kshapptemp3 <- list()
KshappM <- list()

for(i in 1:2){
	#omit negative ksh values

	Kshapptemp2 <- data.frame(K=Kshapp[[i]][,1],Time[[i]],sensor=rep(1,length(Kshapp[[i]][1])))
	for(j in 2:16){
	Kshapptemp2 <- rbind(Kshapptemp2,data.frame(K=Kshapp[[i]][,j],Time[[i]],sensor=rep(j,length(Kshapp[[i]][j]))))
	
	}
	Kshapptemp3[[i]] <- Kshapptemp2[Kshapptemp2$K>0,]

	KshappM[[i]] <- aggregate(Kshapptemp3[[i]]$K, by=list(Kshapptemp3[[i]]$doy,Kshapptemp3[[i]]$sensor),FUN="min")
	colnames(KshappM[[i]]) <- c("doy","sensor","K")
	
}

#organize back into matrix
KshAppt1 <- list()
KshAppt2 <- list()
KshApp <- list()

for(i in 1:2){

	for(j in 1:16){
		
		KshAppt1[[j]] <- left_join(Time[[i]],KshappM[[i]][KshappM[[i]]$sensor==j,],by="doy")
		KshAppt2[[j]] <- KshAppt1[[j]]$K
	}	
		KshApp[[i]] <- matrix(unlist(KshAppt2),byrow=FALSE,ncol=16)
}

#radial heat flow: Qr
#Qr = c*ksh apparent
Qrtemp <- list()
Qr <- list()

for(i in 1:2){

	for(j in 1:16){
		Qrtemp[[j]] <- C[[i]][,j]*KshApp[[i]][,j] 
	}
	Qr[[i]] <- matrix(unlist(Qrtemp),byrow=FALSE,ncol=16)
}	
	
#calculate Qf:
#Qf= Pin-Qv-Qr

Qftemp <- list()
Qf <- list()
for(i in 1:2){

	for(j in 1:16){
		Qftemp[[j]] <- Pin[[i]][,j] - Qv[[i]][,j] - Qr[[i]][,j] 
	}
	Qf[[i]] <- matrix(unlist(Qftemp),byrow=FALSE,ncol=16)
}	


#calculate sapflow in g/s
#Flow= Qf/(Dt*4.186)
#need to set to zero when low flow conditions occur
Flowtemp <- list()
Flow <- 	list()

for(i in 1:2){

	for(j in 1:16){
		
		Flowtemp[[j]] <- ifelse(is.na(Qf[[i]][,j])|is.na(Pin[[i]][,j])|is.na(dT[[i]][,j]),NA,
							ifelse(Qf[[i]][,j]<(0.2*Pin[[i]][,j]) & dT[[i]][,j] <0.75,0,
								ifelse(Qf[[i]][,j]<0,0,
								ifelse(dT[[i]][,j] <=0,NA,
									Qf[[i]][,j] / (dT[[i]][,j]*4.186)))))
	}
	Flow[[i]] <- matrix(unlist(Flowtemp),byrow=FALSE,ncol=16)
}	


FlowLtemp <- list()
FlowL <- 	list()

for(i in 1:2){

	for(j in 1:16){
		FlowLtemp[[j]] <- ifelse(is.na(Flow[[i]][,j]),NA,Flow[[i]][,j]/sensor[[i]]$LA[j])
	}
	FlowL[[i]] <- matrix(unlist(FlowLtemp),byrow=FALSE,ncol=16)
}	


#add time information to sapflow data in g m-2 s-1
#german is first in list
sapFlow <- list()
for(i in 1:2){
	
	sapFlow[[i]] <- data.frame(doy=rep(Time[[i]]$doy,times=16),year=rep(Time[[i]]$year,times=16),
							hour=rep(Time[[i]]$hour,times=16),
							sapF=ifelse(as.vector(FlowL[[i]])>quantile(as.vector(FlowL[[i]]),prob=0.95,na.rm=TRUE),NA, 
								as.vector(FlowL[[i]])),
							sensor=rep(seq(1,16),each=dim(Time[[i]])[1]))
							

}

###################################################################################
###################################################################################
######## Part 2: calculate half hourly and daily transpiration by species  ########   
###################################################################################
###################################################################################	

##################################
# aggregate by species           #
##################################
#aggregate sap flow across species
specDF <- list()
for(i in 1:2){
	specDF[[i]] <- data.frame(sensor=sensor[[i]]$Sensor..,species=sensor[[i]]$species)
}

#join species info into the sensor
temp1 <- list()
for(i in 1:2){
	temp1[[i]] <- left_join(sapFlow[[i]],specDF[[i]],by="sensor")
}

#aggregate by species
nFlow <- list()
sFlow <- list()
sdFlow <- list()
sapFlowNA <- list()
for(i in 1:2){

	sapFlowNA[[i]] <- na.omit(temp1[[i]])
	nFlow[[i]] <- aggregate(sapFlowNA[[i]]$sapF,by=list(sapFlowNA[[i]]$hour,sapFlowNA[[i]]$doy,
													year=sapFlowNA[[i]]$year,species=sapFlowNA[[i]]$species),
							FUN="length")
	colnames(nFlow[[i]]) <- c("hour","doy","year","species","count")
	sFlow[[i]] <- aggregate(sapFlowNA[[i]]$sapF,by=list(sapFlowNA[[i]]$hour,sapFlowNA[[i]]$doy,
													year=sapFlowNA[[i]]$year,species=sapFlowNA[[i]]$species),
							FUN="mean")					
	colnames(sFlow[[i]]) <- c("hour","doy","year","species","sapF")
	sdFlow[[i]] <- aggregate(sapFlowNA[[i]]$sapF,by=list(sapFlowNA[[i]]$hour,sapFlowNA[[i]]$doy,
													year=sapFlowNA[[i]]$year,species=sapFlowNA[[i]]$species),
							FUN="sd")
	colnames(sdFlow[[i]]) <- c("hour","doy","year","species","SDsapF")						
	sFlow[[i]]$SDsapF <- sdFlow[[i]]$SDsapF
	sFlow[[i]]$count <- nFlow[[i]]$count
	sFlow[[i]][sFlow$count>=3,]
	
	
}

#filter for days where measurements aren't reliable
#remove days where the entire measurments are below zero
#get count of measurements in a day
#and remove days where it is only low flow measurements equal to zero
dayNflow <- list()
dayFlow <- list()
for(i in 1:2){
	dayNflow[[i]] <- aggregate(sFlow[[i]]$sapF,by=list( sFlow[[i]]$doy,sFlow[[i]]$year,sFlow[[i]]$species),
								FUN="length")
	colnames(dayNflow[[i]]) <- 	c("doy","year","species","nDay")						
	dayFlow[[i]] <- aggregate(sFlow[[i]]$sapF,by=list( sFlow[[i]]$doy,sFlow[[i]]$year,sFlow[[i]]$species),
								FUN="mean")	
	colnames(dayFlow[[i]]) <- 	c("doy","year","species","sapDay")
	dayFlow[[i]]$nDay <- dayNflow[[i]]$nDay
	#generate a list of days to keep at least half a day of measurement
	#and no all zero day
	dayFlow[[i]] <- dayFlow[[i]][dayFlow[[i]]$nDay>=28&dayFlow[[i]]$sapDay>0,]
	
}
#join back into sFlow
specFlow <- list()
for(i in 1:2){
	specFlow[[i]] <- inner_join(sFlow[[i]],dayFlow[[i]], by=c("species","doy","year"))
}
	
		
##################################
# calculate daily transpiration  #
##################################		
#calculate average daily T

#get the daily observation count by sensor
nFlowSens <- list()
nFlowSensA <- list()
for(i in 1:2){

	nFlowSens[[i]] <- aggregate(sapFlowNA[[i]]$sapF,by=list(doy=sapFlowNA[[i]]$doy,
													year=sapFlowNA[[i]]$year,sensor=sapFlowNA[[i]]$sensor
													,species=sapFlowNA[[i]]$species),
							FUN="length")
	nFlowSensA[[i]] <- nFlowSens[[i]][nFlowSens[[i]]$x==48,]						

}

#subset  sapflow
sensSF <- list()

for(i in 1:2){
	sensSF[[i]] <- inner_join(sapFlowNA[[i]],nFlowSensA[[i]], by=c("doy","year","sensor","species"))
}

#convert to total g m-2 per half hour
for(i in 1:2){
	sensSF[[i]]$sapHH <- sensSF[[i]]$sapF*(60*30)
}

#now sum up total daily transpiration
dailyT <- list()
for(i in 1:2){
	dailyT[[i]] <- aggregate(sensSF[[i]]$sapHH, by=list(doy=sensSF[[i]]$doy,
														year=sensSF[[i]]$year,
														sensor=sensSF[[i]]$sensor,
														species=sensSF[[i]]$species),FUN="sum")
	#remove daily flow where all t was low flow
	dailyT[[i]] <- dailyT[[i]][dailyT[[i]]$x>0,]
}
#aggregate by species
specTday <- list()
nspecT <- list()
SDspecT <- list()
for(i in 1:2){
	specTday[[i]] <- aggregate(dailyT[[i]]$x, by=list(dailyT[[i]]$doy,dailyT[[i]]$year,dailyT[[i]]$species),
											FUN="mean")
	colnames(specTday[[i]]) <- c("doy","year","species","T.g.m2.day")	
	nspecT[[i]] <-	aggregate(dailyT[[i]]$x, by=list(doy=dailyT[[i]]$doy,
														year=dailyT[[i]]$year,
														species=dailyT[[i]]$species),
											FUN="length")									
	specTday[[i]]$nT <- nspecT[[i]]$x
	SDspecT[[i]] <-	aggregate(dailyT[[i]]$x, by=list(doy=dailyT[[i]]$doy,
														year=dailyT[[i]]$year,
														species=dailyT[[i]]$species),
											FUN="sd")
	specTday[[i]]$sdT <- SDspecT[[i]]$x	
	specTday[[i]] <- specTday[[i]][specTday[[i]]$nT>=3,]
	#convert grams to L
	specTday[[i]]$L.m2.day <- specTday[[i]]$T.g.m2.day /1000
	specTday[[i]]$L.m2.daySD <- specTday[[i]]$sdT /1000
}


###################################################################################
###################################################################################
######## Part 3: calculate stomatal conductance by species                 ########   
###################################################################################
###################################################################################	

##################################
# organize met data for stomatal #
# conductance calculations       #
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


met <- list(metG,datLR)

##################################
# calculate stomatal conductance #
##################################		
#convert transpiration to kg m-2 -s
#note
for(i in 1:2){
	sapFlowNA[[i]]$sapFkg <- sapFlowNA[[i]]$sapF/1000
}

#join in canopy met data
for(i in 1:2){
	sapFlowNA[[i]] <- left_join(sapFlowNA[[i]],met[[i]],by=c("doy","hour","year"))
}

#calculate Kg coefficient from Montieth and Unsworth
Kg.coeff<-function(T){115.8+(.423*T)}


for(i in 1:2){
	sapFlowNA[[i]]$Kg.coff <- Kg.coeff(sapFlowNA[[i]]$temp)
}

#convert to gs
Gs.convert1<-function(Kg,El,D,P){((Kg*El)/D)*P}

for(i in 1:2){
	sapFlowNA[[i]]$gcalc <- Gs.convert1(sapFlowNA[[i]]$Kg.coff,
									sapFlowNA[[i]]$sapFkg,
									sapFlowNA[[i]]$D,
									sapFlowNA[[i]]$Pkpa.gap)
}

#change units to moles
unit.conv<-function(gs,T,P){gs*.446*(273/(T+273))*(P/101.3)}

for(i in 1:2){
	sapFlowNA[[i]]$gcm <- unit.conv(sapFlowNA[[i]]$gcalc,sapFlowNA[[i]]$temp,sapFlowNA[[i]]$Pkpa.gap)
}

#subset so that only taking gc during reliable times
#when its not raining and error in gc assumption is minimal due to higher vpd
gcSub <- list()
for(i in 1:2){
	gcSub[[i]] <- data.frame(sapFlowNA[[i]][,1:3],D=sapFlowNA[[i]]$D,Pr.mm=sapFlowNA[[i]]$Pr.mm,
								gcm=sapFlowNA[[i]]$gcm,species=sapFlowNA[[i]]$species)
									
	gcSub[[i]] <- na.omit(gcSub[[i]][gcSub[[i]]$D >= 0.6 & gcSub[[i]]$Pr.mm < 1,])
}
#calculate the average gc for species
gcSpec <- list()
gcSpecSD <- list()
gcSpecN <- list()
for(i in 1:2){
	gcSpec[[i]] <- aggregate(gcSub[[i]]$gcm, 
								by=list(gcSub[[i]]$hour,
										gcSub[[i]]$doy,
										gcSub[[i]]$year,
										gcSub[[i]]$species),
								FUN="mean")
	colnames(gcSpec[[i]]) <- c("hour","doy","year","species","gc.mol.m2.s")
	gcSpecSD[[i]] <- aggregate(gcSub[[i]]$gcm, 
								by=list(gcSub[[i]]$hour,
										gcSub[[i]]$doy,
										gcSub[[i]]$year,
										gcSub[[i]]$species),
								FUN="sd")
	gcSpecN[[i]] <- aggregate(gcSub[[i]]$gcm, 
								by=list(gcSub[[i]]$hour,
										gcSub[[i]]$doy,
										gcSub[[i]]$year,
										gcSub[[i]]$species),
								FUN="length")
	gcSpec[[i]]$gcSD <- gcSpecSD[[i]]$x
	gcSpec[[i]]$gcN <- gcSpecN[[i]]$x
	#remove cases where the average is calculated with less than 3
	gcSpec[[i]] <- gcSpec[[i]][gcSpec[[i]]$gcN>=3,]
}
		
rm(list=setdiff(ls(), c("sapFlow","sensor","specFlow","specTday","gcSpec")))