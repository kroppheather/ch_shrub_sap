########################################################################################
########################  2016-2017 SIBERIA SHRUB SAPFLOW  #############################
########################  Low Density and German Site      #############################  
########################################################################################



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
library(plyr)
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
Kshapptemp2 <- list()
Kshapptemp3 <- list()
Kshapptemp4 <- list()
KshappM <- list()

for(i in 1:2){
	#omit negative ksh values
	for(j in 1:16){
	Kshapptemp2[[j]] <- data.frame(K=Kshapp[[i]][,j],Time[[i]],sensor=rep(j,length(Kshapp[[i]][j])))
	Kshapptemp3[[j]] <- Kshapptemp2[[j]][Kshapptemp2[[j]]$K>0,]
	}
	Kshapptemp4[[i]] <- ldply(Kshapptemp3,data.frame)
	KshappM[[i]] <- aggregate(Kshapptemp4[[i]]$K, by=list(Kshapptemp4[[i]]$doy,Kshapptemp4[[i]]$sensor),FUN="min")
	colnames(KshappM[[i]]) <- c("doy","sensor","K")
	
}

#organize back into matrix
KshAppt1 <- list()
KshAppt2 <- list()
KshApp <- list()

for(i in 1:2){

	for(j in 1:16){
		
		KshAppt1[[j]] <- join(Time[[i]],KshappM[[i]][KshappM[[i]]$sensor==j,],by="doy",type="left")
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
							ifelse(Qf[[i]][,j]<(0.2*Pin[[i]][,j])|Qf[[i]][,j]<0|dT[[i]][,j]<0.75,
									0,Qf[[i]][,j] / (dT[[i]][,j]*4.186)))
	}
	Flow[[i]] <- matrix(unlist(Flowtemp),byrow=FALSE,ncol=16)
}	

#calculate sap flow and normalize for leaf area
FlowLtemp <- list()
FlowL <- 	list()

for(i in 1:2){

	for(j in 1:16){
		FlowLtemp[[j]] <- Flow[[i]][,j]/sensor[[i]]$LA[j]
	}
	FlowL[[i]] <- matrix(unlist(FlowLtemp),byrow=FALSE,ncol=16)
}	


#add time information to sapflow data in g m-2 s-1
#german is first in list
sapFlow <- list()
for(i in 1:2){
	
	sapFlow[[i]] <- cbind(Time[[i]],FlowL[[i]])
}

rm(list=setdiff(ls(), c("sapFlow","sensor")))