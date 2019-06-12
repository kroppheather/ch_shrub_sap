###################################################################################
########################  2016-2017 SIBERIA SHRUB SAPFLOW  ########################
########################  Low Density and German Site      ########################
###################################################################################
######################## check for hysteresis in sap flow  ########################
###################################################################################
######################## inputs:                           ########################
######################## sensor (list) sensor info         ########################
######################## sapFlow (list) sapflow g m-2 s-1  ########################
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
datL <- datM[datM$site=="ld",]

#calculate vpd for each site



##################################
# calculate vapor pressure       #
##################################

#make a data.frame of days to plot hysteresis over for each site
days <- list()
for(i in 1:2){
	days[[i]] <- unique(data.frame(doy=sapFlow[[i]]$doy))
}

#make plots of the sensors on each day
#using a diverging palette
coli <- c(rev(brewer.pal(8,"Blues")),brewer.pal(8,"Oranges"))

for(i in 1:2){


}
