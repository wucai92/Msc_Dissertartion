library(igraph)
require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(broom)
library(sqldf)
library(tidyr)


getwd()

train_flow <- read.csv(file="origin_destination_train_201904.csv",head=TRUE,sep=",")
bus_flow <- read.csv(file="origin_destination_bus_201904.csv",head=TRUE,sep=",")
file_stations <- read.csv(file="stations.csv",head=TRUE,sep=",")

head(train_flow, n=10)
head(file_stations, n=10)
head(bus_flow, n=10)


train_flow <-sqldf("SELECT train_flow.DAY_TYPE, train_flow.TIME_PER_HOUR, train_flow.ORIGIN_PT_CODE, train_flow.DESTINATION_PT_CODE, train_flow.TOTAL_TRIPS, file_stations.SUBZONE_C AS ORIGIN_SUB_C, file_stations.SUBZONE_N AS ORIGIN_SUB_N FROM train_flow LEFT JOIN file_stations ON train_flow.ORIGIN_PT_CODE = file_stations.Station_code")
train_flow <-sqldf("SELECT train_flow.DAY_TYPE, train_flow.TIME_PER_HOUR, train_flow.ORIGIN_PT_CODE, train_flow.DESTINATION_PT_CODE, train_flow.TOTAL_TRIPS, train_flow.ORIGIN_SUB_C, train_flow.ORIGIN_SUB_N, file_stations.SUBZONE_C AS DESTINATION_SUB_C, file_stations.SUBZONE_N AS DESTINATION_SUB_N FROM train_flow LEFT JOIN file_stations ON train_flow.DESTINATION_PT_CODE = file_stations.Station_code")
head(train_flow, n=10)

bus_flow <-sqldf("SELECT bus_flow.DAY_TYPE, bus_flow.TIME_PER_HOUR, bus_flow.ORIGIN_PT_CODE, bus_flow.DESTINATION_PT_CODE, bus_flow.TOTAL_TRIPS, file_stations.SUBZONE_C AS ORIGIN_SUB_C, file_stations.SUBZONE_N AS ORIGIN_SUB_N FROM bus_flow LEFT JOIN file_stations ON bus_flow.ORIGIN_PT_CODE = file_stations.Station_code")
bus_flow <-sqldf("SELECT bus_flow.DAY_TYPE, bus_flow.TIME_PER_HOUR, bus_flow.ORIGIN_PT_CODE, bus_flow.DESTINATION_PT_CODE, bus_flow.TOTAL_TRIPS, bus_flow.ORIGIN_SUB_C, bus_flow.ORIGIN_SUB_N, file_stations.SUBZONE_C AS DESTINATION_SUB_C, file_stations.SUBZONE_N AS DESTINATION_SUB_N FROM bus_flow LEFT JOIN file_stations ON bus_flow.DESTINATION_PT_CODE = file_stations.Station_code")
head(bus_flow, n=10)

total_flow <- rbind(train_flow, bus_flow)
total_flow %>% drop_na()