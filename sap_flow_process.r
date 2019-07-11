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
######################## outputs:                          ########################
######################## sensor (list) sensor info         ########################
######################## sapFlow (list) sapflow g m-2 s-1  ########################
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


#read in met data sensors for each site
datP <- read.csv("z:\\data_repo\\field_data\\viperData\\sensor\\airport\\airport.csv")

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

#aggregate sap flow across species
specDF <- list()
for(i in 1:2){
	specDF[[i]] <- data.frame(sensor=sensor[[i]]$Sensor..,species=sensor[[i]]$species)
}

#join species info into the sensor
temp1 <- list()
for(i in 1:2){
	temp1[[i]] <- join(sapFlow[[i]],specDF[[i]],by="sensor",type="left")
}

#aggregate by species
nFlow <- list()
sFlow <- list()
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
	sFlow[[i]]$count <- nFlow[[i]]$count
	sFlow[[i]][sFlow$count>=3,]
}

#filter for days where measurements aren't reliable
#remove days that are on precip days

for(i in 1:2){

}


specFlow <- sFlow


		
rm(list=setdiff(ls(), c("sapFlow","sensor","specFlow")))