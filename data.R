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

time_flow <- sqldf("select TIME_PER_HOUR, sum(TOTAL_TRIPS) as TOTAL_TRIPS from total_flow where DAY_TYPE = 'WEEKDAY' group by TIME_PER_HOUR")
plot(time_flow,type = "o", col = "red", xlab = "TIME_PER_HOUR", ylab = "TOTAL_TRIPS",
     main = "Flow chart")
ggplot(time_flow, aes(x=TIME_PER_HOUR, y=TOTAL_TRIPS)) + geom_bar(stat = "identity")

subzone_wkd_all <- sqldf("select ORIGIN_SUB_C, ORIGIN_SUB_N, DESTINATION_SUB_C, DESTINATION_SUB_N, sum(TOTAL_TRIPS) as TOTAL_TRIPS from total_flow where DAY_TYPE = 'WEEKDAY' group by ORIGIN_SUB_C, DESTINATION_SUB_C")
head(subzone_wkd_all, n=10)
subzone_wkd_all <- subzone_wkd_all[30:31501, ]
subzone_wkd_all <- subzone_wkd_all[order(subzone_wkd_all$TOTAL_TRIPS),] 
head(subzone_wkd_all, n=10)
write.csv(subzone_wkd_all, file = "subzone_wkd_all.csv")

subzone_wkd_morning <- read.csv(file="subzone_wkd_morning.csv",head=TRUE,sep=",")
node1=subzone_wkd_morning$ORIGIN_SUB_C
node2=subzone_wkd_morning$DESTINATION_SUB_C
weight=subzone_wkd_morning$TOTAL_TRIPS
df=data.frame(node1,node2,weight)
g_subzone=graph.data.frame(df,directed=TRUE)

plot(g_subzone,vertex.size=3,vertex.label.cex=.5,vertex.color="white")

s_subzone=simplify(g_subzone,remove.loops = T,edge.attr.comb = "min")
de <- degree(g_subzone, v = V(g_subzone), mode = "all",loops = TRUE, normalized = TRUE)
palette_edges=hsv(h=1-((E(s_subzone)$weight/max(E(s_subzone)$weight)*4/5)+1/5),s = 1,v=1)
palette=hsv(h=1-((de/max(de)*2/3)+1/3),s = 1,v=1)
plot(s_subzone,vertex.size=3, edge.arrow.mode = 0, edge.width=E(s_subzone)$weight/max(E(s_subzone)$weight)*30, vertex.label=NA, vertex.color=palette, vertex.size=(de/max(de))*15,edge.color=palette_edges)

c_infomap <- cluster_infomap(s_subzone, v.weights = NULL, nb.trials = 10, modularity = FALSE)
info_membership <- cbind(V(s_subzone)$name,c_infomap$membership)
write.csv(info_membership, file = "info_membership_morning.csv")
communities(c_infomap)

completedata <- read.csv(file="completedata.csv",head=TRUE,sep=",")
node1=completedata$ORIGIN_SUB_C
node2=completedata$DESTINATION_SUB_C
weight=completedata$TOTAL_TRIPS
df1=data.frame(node1,node2,weight)
weight=completedata$prodsimest4_scenario
df2=data.frame(node1,node2,weight)
g_origin=graph.data.frame(df1,directed=TRUE)
g_after=graph.data.frame(df2,directed=TRUE)

infomap_origin <- cluster_infomap(g_origin, v.weights = NULL, nb.trials = 10, modularity = FALSE)
infomap_after <- cluster_infomap(g_after, v.weights = NULL, nb.trials = 10, modularity = FALSE)
membership <- cbind(V(g_origin)$name,infomap_origin$membership,infomap_after$membership)
membership_after <- cbind(V(g_after)$name,infomap_after$membership)
write.csv(membership_origin, file = "membership_origin.csv")
write.csv(membership, file = "membership.csv")

final <- read.csv(file="final_3.csv",head=TRUE,sep=",")
current <- sqldf("select memebershi as membership, sum(commerical) as commercial, sum(population) from final group by memebershi")
predicted <- sqldf("select membership, sum(floor) as commercial, sum(population) from final group by membership")
current <- current[2:13,]
predicted <- predicted[2:12,]
completedata <- sqldf("SELECT completedata.ORIGIN_SUB_C, completedata.DESTINATION_SUB_C, completedata.TOTAL_TRIPS AS CURRENT, completedata.prodsimest4_scenario AS PRESICTED, final.memebershi AS current_origin FROM completedata LEFT JOIN final ON completedata.ORIGIN_SUB_C = final.SUBZONE_C")
completedata <- sqldf("SELECT completedata.ORIGIN_SUB_C, completedata.DESTINATION_SUB_C, completedata.CURRENT, completedata.PRESICTED, current_origin, final.memebershi AS current_destination FROM completedata LEFT JOIN final ON completedata.DESTINATION_SUB_C = final.SUBZONE_C")
completedata <- sqldf("SELECT completedata.ORIGIN_SUB_C, completedata.DESTINATION_SUB_C, completedata.CURRENT, completedata.PRESICTED, completedata.current_origin, completedata.current_destination, final.membership AS predicted_origin FROM completedata LEFT JOIN final ON completedata.ORIGIN_SUB_C = final.SUBZONE_C")
completedata <- sqldf("SELECT completedata.ORIGIN_SUB_C, completedata.DESTINATION_SUB_C, completedata.CURRENT, completedata.PRESICTED, completedata.current_origin, completedata.current_destination, predicted_origin, final.membership AS predicted_destination FROM completedata LEFT JOIN final ON completedata.DESTINATION_SUB_C = final.SUBZONE_C")

current_nodality <- sqldf("select current_destination, sum(CURRENT) AS nodality from completedata group by current_destination")
current_centrality <- sqldf("select current_destination, sum(CURRENT) AS centrality from completedata where current_origin != current_destination group by current_destination")
current$nodality <- current_nodality$nodality
current$centrality <- current_centrality$centrality
predicted_nodality <- sqldf("select predicted_destination, sum(PRESICTED) AS nodality from completedata group by predicted_destination")
predicted_centrality <- sqldf("select predicted_destination, sum(PRESICTED) AS centrality from completedata where predicted_origin != predicted_destination group by predicted_destination")
predicted$nodality <- predicted_nodality$nodality
predicted$centrality <- predicted_centrality$centrality
write.csv(current, file = "current.csv")
write.csv(predicted, file = "predicted.csv")