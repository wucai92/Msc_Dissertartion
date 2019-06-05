library(igraph)
require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(broom)
library(sqldf)
library(tidyr)
library(stringi)



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

write.csv(bus_flow, file = "bus_flow.csv")
write.csv(train_flow, file = "train_flow.csv")

sapply(bus_flow1, class)
sapply(train_flow, class)

bus_flow1 <- bus_flow %>% drop_na()
bus_flow1$ORIGIN_PT_CODE = as.character(bus_flow1$ORIGIN_PT_CODE)
bus_flow1$DESTINATION_PT_CODE = as.character(bus_flow1$DESTINATION_PT_CODE)
train_flow$ORIGIN_PT_CODE = as.character(train_flow$ORIGIN_PT_CODE)
train_flow$DESTINATION_PT_CODE = as.character(train_flow$DESTINATION_PT_CODE)

total_flow <- rbind(train_flow, bus_flow)
total_flow$ORIGIN_SUB_C <- stri_omit_empty(total_flow$ORIGIN_SUB_C, na_empty = FALSE)
total_flow$ORIGIN_SUB_N <- stri_omit_empty(total_flow$ORIGIN_SUB_N, na_empty = FALSE)
total_flow$DESTINATION_SUB_C <- stri_omit_empty(total_flow$DESTINATION_SUB_C, na_empty = FALSE)
total_flow$DESTINATION_SUB_N <- stri_omit_empty(total_flow$DESTINATION_SUB_N, na_empty = FALSE)
total_flow <- total_flow %>% drop_na()
write.csv(total_flow, file = "total_flow.csv")

subzone_wkd_all <- sqldf("select ORIGIN_SUB_C, ORIGIN_SUB_N, DESTINATION_SUB_C, DESTINATION_SUB_N, sum(TOTAL_TRIPS) as TOTAL_TRIPS from total_flow where DAY_TYPE = 'WEEKDAY' group by ORIGIN_SUB_C, DESTINATION_SUB_C")
head(subzone_wkd_all, n=10)
subzone_wkd_all <- subzone_wkd_all[30:31501, ]
subzone_wkd_all <- subzone_wkd_all[order(subzone_wkd_all$TOTAL_TRIPS),] 
head(subzone_wkd_all, n=10)
write.csv(subzone_wkd_all, file = "subzone_wkd_all.csv")

node1=subzone_wkd_all$ORIGIN_SUB_C
node2=subzone_wkd_all$DESTINATION_SUB_C
weight=subzone_wkd_all$TOTAL_TRIPS
df=data.frame(node1,node2,weight)
g_subzone=graph.data.frame(df,directed=TRUE)

plot(g_subzone,vertex.size=3,vertex.label.cex=.5,vertex.color="white")

s_subzone=simplify(g_subzone,remove.loops = T,edge.attr.comb = "min")
de <- degree(g_subzone, v = V(g_subzone), mode = "all",loops = TRUE, normalized = TRUE)
palette_edges=hsv(h=1-((E(s_subzone)$weight/max(E(s_subzone)$weight)*4/5)+1/5),s = 1,v=1)
palette=hsv(h=1-((de/max(de)*2/3)+1/3),s = 1,v=1)
plot(s_subzone,vertex.size=3, edge.arrow.mode = 0, edge.width=E(s_subzone)$weight/max(E(s_subzone)$weight)*30, vertex.label=NA, vertex.color=palette, vertex.size=(de/max(de))*15,edge.color=palette_edges)

c_infomap <- cluster_infomap(s_subzone, v.weights = NULL, nb.trials = 10, modularity = FALSE)
membership(c_infomap)
communities(c_infomap)