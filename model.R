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
library(stringr)



subzone <- read_shape("shape/subzone.shp", as.sf = TRUE)
summary(subzone)
class(subzone)
subzoneSP <- as(subzone, "Spatial")
subzoneSP  <- subzoneSP[order(subzoneSP$SUBZONE_C),]
subzoneSP$ID <- seq.int(nrow(subzoneSP))
dist <- spDists(subzoneSP)
distPair <- melt(dist)
cdata <- read.csv("subzone_wkd_morning.csv")
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
CalcRSquared(cdatasub$TotalNoIntra,cdatasub$unconstrainedEst2)
CalcRMSE <- function(observed,estimated){
  res <- (observed - estimated)^2
  RMSE <- round(sqrt(mean(res)),3)
  RMSE
}
CalcRMSE(cdatasub$value,cdatasub$unconstrainedEst2)
CalcRMSE(cdatasub$value,cdatasub$unconstrainedEst2)

prodSim <- glm(TOTAL_TRIPS ~ ORIGIN_SUB_C + log(wj2_destcom+1)+log(value)-1, na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
#let's have a look at it's summary...
summary(prodSim)

O_i <- cdatasub %>% group_by(ORIGIN_SUB_C) %>% summarise(O_i = sum(TOTAL_TRIPS))
cdatasub$O_i <- O_i$O_i[match(cdatasub$ORIGIN_SUB_C,O_i$ORIGIN_SUB_C)]
D_j <- cdatasub %>% group_by(DESTINATION_SUB_C) %>% summarise(D_j = sum(TOTAL_TRIPS))
cdatasub$D_j <- D_j$D_j[match(cdatasub$DESTINATION_SUB_C,D_j$DESTINATION_SUB_C)]
prodSim_out <- tidy(prodSim)
prodSim_out

# or you can just pull out the coefficients and put them into an object
coefs <- as.data.frame(prodSim$coefficients)
#then once you have done this, you can join them back into the dataframe using a regular expression to match the bits of the identifier that you need - *note, this bit of code below took me about 2 hours to figure out!*
cdatasub$mu_i <- coefs$`prodSim$coefficients`[match(cdatasub$ORIGIN_SUB_C,sub(".*ORIGIN_SUB_C","", rownames(coefs)))]
#now, where we have missing values for our reference mu_i variable, fill those with 1s
head(cdatasub)

mu_i <- prodSim$coefficients[1:303]
alpha <- prodSim$coefficients[304]
beta <- prodSim$coefficients[305]

cdatasub$prodsimest1 <- exp((cdatasub$mu_i)+(alpha*log(cdatasub$wj2_destcom+1))+(beta*log(cdatasub$value)))

cdatasub$prodsimFitted <- fitted(prodSim)

head(cdatasub)

cdatasub$prodsimFitted <- round(fitted(prodSim),0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat3 <- dcast(cdatasub, ORIGIN_SUB_C ~ DESTINATION_SUB_C, sum, value.var = "prodsimFitted", margins=c("Orig", "Dest"))
cdatasubmat3
CalcRSquared(cdatasub$TOTAL_TRIPS,cdatasub$prodsimFitted)

popspace2 <- read.csv("newsubzone.csv")
cdatasub$wj3_destscomScenario <- popspace2$floor[match(cdatasub$DESTINATION_SUB_C, popspace2$SUBZONE_C)]
cdatasub$prodsimest2 <- exp((cdatasub$mu_i)+(alpha*log(cdatasub$wj3_destscomScenario+1))+(beta*log(cdatasub$value)))
cdatasub$prodsimest2 <- round(cdatasub$prodsimest2,0)

wj2_alpha <- (cdatasub$wj2_destcom+1)^alpha
value_beta <- (cdatasub$value+1)^beta
cdatasub$Ai1 <- wj2_alpha*value_beta
A_i <- cdatasub %>% group_by(ORIGIN_SUB_C) %>% summarise(A_i = sum(Ai1))
A_i[,2] <- 1/A_i[,2]
#and write the A_i values back into the data frame
cdatasub$A_i <- A_i$A_i[match(cdatasub$ORIGIN_SUB_C,A_i$ORIGIN_SUB_C)]
cdatasub$prodsimest3 <- cdatasub$A_i*cdatasub$O_i*wj2_alpha*value_beta

wj3_alpha <- (cdatasub$wj3_destscomScenario+1)^alpha
#calculate the first stage of the Ai values
cdatasub$Ai1 <- wj3_alpha*dist_beta
#now do the sum over all js bit
A_i <- cdatasub %>% group_by(ORIGIN_SUB_C) %>% summarise(A_i = sum(Ai1))
#now divide in to 1
A_i[,2] <- 1/A_i[,2]
#and write the A_i values back into the data frame
cdatasub$A_i <- A_i$A_i[match(cdatasub$ORIGIN_SUB_C,A_i$ORIGIN_SUB_C)]
cdatasub$prodsimest4_scenario <- cdatasub$A_i*cdatasub$O_i*wj3_alpha*value_beta
write.csv(cdatasub, file = "completedata.csv")