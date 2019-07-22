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

##################################
# set up directories             #
##################################

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

#make a plot of daily gc


