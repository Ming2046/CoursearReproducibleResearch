setwd("~/Desktop/OneDrive/R_Project/Coursera/ReproducibleResearch/Week4")

library(tidyverse)
library(grid)
library(gridExtra)

NOAA <- read_csv("repdata-data-StormData.csv")

names(NOAA)

### event type EVTYPE, Variabe No.8

head(NOAA,6)

stormData <- NOAA[,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG",
                     "PROPDMGEXP","CROPDMG","CROPDMGEXP")]

Health <- stormData %>% group_by(EVTYPE) %>% summarise(Injury = sum(INJURIES), Fatality =sum(FATALITIES))


#### results for the most harmful with respect to population health
HealthInjury <- Health [order (Health$Injury, decreasing = T), -3]

HealthFatality <- Health [order (Health$Fatality, decreasing = T), -2]

### data processig for the EconomicDamange (ED)


class(stormData$PROPDMGEXP)

stormData$PROPDMGEXP <- as.factor(stormData$PROPDMGEXP)

stormData$CROPDMGEXP <- as.factor(stormData$CROPDMGEXP)

ED <- stormData[,-c(2,3)]

levels(ED$PROPDMGEXP)

## factor function only call applicable factor levels. 

## if only table function, all levels will be shown, even certain levels are already changed or removed

### https://www.stat.berkeley.edu/classes/s133/factors.html

table(factor(ED$PROPDMGEXP))

sum(is.na(ED$PROPDMGEXP))

ED$Prop.DMG[ED$PROPDMGEXP %in% c("-","?","+")] <- 0

ED$Prop.DMG[ED$PROPDMGEXP %in% c("h","H")] <- 100

ED$Prop.DMG[ED$PROPDMGEXP == "0"] <- 1

ED$Prop.DMG[ED$PROPDMGEXP == ""] <- 0

ED$Prop.DMG[ED$PROPDMGEXP == "1"] <- 10

ED$Prop.DMG[ED$PROPDMGEXP == "2"] <- 100

ED$Prop.DMG[ED$PROPDMGEXP == "3"] <- 1000

ED$Prop.DMG[ED$PROPDMGEXP == "4"] <- 1*10^4

ED$Prop.DMG[ED$PROPDMGEXP == "5"] <- 1*10^5

ED$Prop.DMG[ED$PROPDMGEXP == "6"] <- 1*10^6

ED$Prop.DMG[ED$PROPDMGEXP == "7"] <- 1* 10^7

ED$Prop.DMG[ED$PROPDMGEXP == "8"] <- 1* 10^8

ED$Prop.DMG[ED$PROPDMGEXP == "K"] <- 1000

ED$Prop.DMG[ED$PROPDMGEXP == "B"] <- 1*10^9

ED$Prop.DMG[ED$PROPDMGEXP %in% c("m","M")] <- 1*10^6

sum(is.na(ED$Prop.DMG))

levels(ED$CROPDMGEXP)

table(factor(ED$CROPDMGEXP))

ED$Crop.DMG[ED$CROPDMGEXP == "?"] <- 0

ED$Crop.DMG[ED$CROPDMGEXP == ""] <- 0

ED$Crop.DMG[ED$CROPDMGEXP == "0"] <- 0

ED$Crop.DMG[ED$CROPDMGEXP == "2"] <- 100

ED$Crop.DMG[ED$CROPDMGEXP == "B"] <- 1*10^9

ED$Crop.DMG[ED$CROPDMGEXP %in% c("k","K")] <- 1000

ED$Crop.DMG[ED$CROPDMGEXP %in% c("m","M")] <- 1*10^6

Property <- ED[, c(1,2,6)]     

Crop <- ED[,c(1,4,7)]

Property$DMG.Value <- Property$PROPDMG*Property$Prop.DMG

Crop$DMG.Value <- Crop$CROPDMG*Crop$Crop.DMG

PropertyDMD <- Property %>% group_by(EVTYPE) %>% 
    summarise(Damage = sum(DMG.Value)) %>% 
    arrange(desc(Damage))

PropertyDMDTop5 <- PropertyDMD [c(1:5),]

PropertyDMDTop5$Damage <- PropertyDMDTop5$Damage/10^9

names(PropertyDMDTop5) <- c("Events","Billions")

CropDMD <- Crop %>% group_by(EVTYPE) %>% 
    summarise(Damage = sum(DMG.Value)) %>% 
    arrange(desc(Damage))

CropDMDTop5 <- CropDMD[c(1:5),]

CropDMDTop5$Damage <- CropDMDTop5$Damage/10^9

names(CropDMDTop5) <- c("Events","Billions")

g1 <- ggplot(PropertyDMDTop5,aes(x=Events, y = Billions)) + geom_col()+
    labs(title = "Property Damage (Top 5)")

g1


g2 <- ggplot(CropDMDTop5,aes(x=Events, y = Billions)) + geom_col()+
    labs(title = " Crop Damage (Top 5")

g2

grid.arrange(g1,g2,nrow = 2)
