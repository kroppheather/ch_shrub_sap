library(plyr)
library(dplyr)
source("/Users/cpham/Downloads/ch_shrub_sap/sap_flow_process.r")

#added id for each site
gcSpec[[1]]$site <- rep(1, nrow(gcSpec[[1]]))
gcSpec[[2]]$site <- rep(2, nrow(gcSpec[[2]]))
gc_df <- rbind(gcSpec[[1]],  gcSpec[[2]])

#import datasets at the airport
datAir <- read.csv("/Volumes/data/data_repo/field_data/viperData/sensor/airport/airport.csv")

#get vpd at half hour rate
met[[1]]$site <- rep(1, nrow(met[[1]]))
met[[2]]$site <- rep(2, nrow(met[[2]]))

#create met_df, retaining only "site", "hour", doy", "year", temp", "D"
first_met <- met[[1]][,c("site", "hour", "doy", "year", "temp", "D")]
second_met <- met[[2]][,c("site", "hour", "doy", "year", "temp", "D")]
met_df <- rbind(first_met, second_met)

#match with gc_df by "site", "doy", "year", 
met_gc <- met_df%>% right_join(gc_df, by=c("doy","year","hour","site"))

#average daily air temperature for met_df
met_daily_avg <- ddply(met_df, .(doy, year), summarize, daily_avg_temp = mean(temp))
met_df_2 <- met_df%>% right_join(met_daily_avg, by=c("doy","year"))

#compare with Pr.mm values from datAir
met_df_3 <- datAir %>%select("doy", "year", "Pr.mm")%>% right_join(met_df_2, by=c("doy","year"))
met_df_3 <- met_df_3[,c("doy", "year","site","temp", "D", "daily_avg_temp", "Pr.mm")]


rm(list=setdiff(ls(), c("sapFlow","sensor","specFlow","specTday","gcSpec","met", "met_df_3","met_gc")))