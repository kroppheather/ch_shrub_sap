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
datLD <- read.csv ("z:\\data_repo\\field_data\\viperData\\sensor\\campbell\\sapflow\\ld_f32_2016.csv")
ldLA <- read.csv("z:\\data_repo\\field_data\\viperData\\sapflow\\LD_la.csv")



#read in met data sensors for each site

