library(maptools)
library(RColorBrewer)
library(classInt)
library(OpenStreetMap)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(MASS)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(leaflet)
library(methods)



subzone <- read_shape("shape/subzone.shp", as.sf = TRUE)
summary(subzone)
class(subzone)
subzoneSP <- as(subzone, "Spatial")
subzoneSP  <- subzoneSP[order(subzoneSP$SUBZONE_C),]
subzoneSP$ID <- seq.int(nrow(subzoneSP))
dist <- spDists(subzoneSP)
distPair <- melt(dist)
cdata <- read.csv("subzone_wkd_all.csv")
popspace <- read.csv("subzone.csv")
cdata$vi1_origpop <- popspace$population[match(cdata$ORIGIN_SUB_C, popspace$SUBZONE_C)]
cdata$vi2_origcom <- popspace$commerical[match(cdata$ORIGIN_SUB_C, popspace$SUBZONE_C)]
cdata$wj1_destpop <- popspace$population[match(cdata$DESTINATION_SUB_C, popspace$SUBZONE_C)]
cdata$wj2_destcom <- popspace$commerical[match(cdata$DESTINATION_SUB_C, popspace$SUBZONE_C)]
cdata$orig_id <- subzoneSP$ID[match(cdata$ORIGIN_SUB_C, subzoneSP$SUBZONE_C)]
cdata$dest_id <- subzoneSP$ID[match(cdata$DESTINATION_SUB_C, subzoneSP$SUBZONE_C)]
cdata <- arrange(cdata, ORIGIN_SUB_C, DESTINATION_SUB_C)
require(dplyr)
cdata <- left_join(cdata, distPair, by = c("orig_id" = "Var1", "dest_id" = "Var2")) 
cdata$dist <- distPair$value[match(interaction(cdata$orig_id, distPair$Var1), interaction(cdata$dest_id, distPair$Var2))]

cdata$TotalNoIntra <- ifelse(cdata$ORIGIN_SUB_C == cdata$DESTINATION_SUB_C,0,cdata$TOTAL_TRIPS)
cdata$offset <- ifelse(cdata$ORIGIN_SUB_C == cdata$DESTINATION_SUB_C,0.0000000001,1)
cdata <- dplyr::select(cdata, ORIGIN_SUB_C, DESTINATION_SUB_C, TOTAL_TRIPS, everything())

cdatasub <- cdata[cdata$ORIGIN_SUB_C!=cdata$DESTINATION_SUB_C,]

cdatamat <- dcast(cdata, ORIGIN_SUB_C ~ DESTINATION_SUB_C, sum, value.var = "TOTAL_TRIPS", margins=c("ORIGIN_SUB_C", "DESTINATION_SUB_C"))
cdatamat


plot1 <- qplot(cdata$value, cdata$TOTAL_TRIPS)
#and now the model fit...
plot1 + stat_function(fun=function(x)x^-2, geom="line", aes(colour="^-2"))

plot2 <- qplot(cdata$vi1_origpop, cdata$TOTAL_TRIPS)
plot2 + stat_function(fun=function(x)x^1.2, geom="line", aes(colour="^1.2"))

plot3 <- qplot(cdata$wj2_destcom, cdata$TOTAL_TRIPS)
plot3 + stat_function(fun=function(x)x^1, geom="line", aes(colour="^1"))

qplot(cdata$TOTAL_TRIPS) + geom_histogram()
qplot(log(value+1), log(TOTAL_TRIPS+1), data=cdatasub) + geom_smooth(method = lm)

uncosim <- glm(TOTAL_TRIPS ~ log(vi1_origpop+1)+log(wj2_destcom+1)+log(value), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
summary(uncosim)

#first asign the parameter values from the model to the appropriate variables
k <- uncosim$coefficients[1]
mu <- uncosim$coefficients[2]
alpha <- uncosim$coefficients[3]
beta <- -uncosim$coefficients[4]

#now plug everything back into the Equation 6 model... (be careful with the positive and negative signing of the parameters as the beta parameter may not have been saved as negative so will need to force negative)
cdatasub$unconstrainedEst2 <- exp(k+(mu*log(cdatasub$vi1_origpop+1))+(alpha*log(cdatasub$wj2_destcom+1))-(beta*log(cdatasub$value)))


#and of course, being R, there is an even easier way of doing this...
cdatasub$fitted <- fitted(uncosim)

cdatasub$unconstrainedEst2 <- round(cdatasub$unconstrainedEst2,0)
sum(cdatasub$unconstrainedEst2)

cdatasubmat2 <- dcast(cdatasub, ORIGIN_SUB_C ~ DESTINATION_SUB_C, sum, value.var = "unconstrainedEst2", margins=c("ORIGIN_SUB_C", "DESTINATION_SUB_C"))
cdatasubmat2

CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}
CalcRSquared(cdatasub$value,cdatasub$unconstrainedEst2)
CalcRMSE <- function(observed,estimated){
  res <- (observed - estimated)^2
  RMSE <- round(sqrt(mean(res)),3)
  RMSE
}
CalcRMSE(cdatasub$value,cdatasub$unconstrainedEst2)
CalcRMSE(cdatasub$value,cdatasub$unconstrainedEst2)

uncosim <- glm(TOTAL_TRIPS ~ log(vi1_origpop+1)+log(wj2_destcom+1)+log(value), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
summary(uncosim)