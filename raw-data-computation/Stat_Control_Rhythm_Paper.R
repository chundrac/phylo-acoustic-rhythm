## Necessary Packages
library (sjPlot)
library (ggplot2)
library (doBy)
library (GGRidge)
library (ggridges)
library (extrafont)
library (showtext)
library (ggpubr)
library (RColorBrewer)



## Load Rhythm Data

filename <- paste ("C:/Users/piette/Desktop/Rhythm_Study/Data_Rhythm_Final.csv")

Rhythm <- read.csv(filename, sep=";", dec=",", header=TRUE)



## Get Median Rhythn

Median.R <- summaryBy(Frequency ~ Common_Name*Weight*Environement*Group*Group_Simple*Group_All*Order*Mastication*Congregatory*Log_Group_Size, data = Rhythm, 
                      FUN = list(median))

colnames(Median.R) <- c("Common_Name","Weight","Environment","Group","Group_Simple","Group_All","Order","Mastication","Congregatory", "Group_Size", "Rhythm")


## Get Median Log Rhythn

Median.R.Log <- summaryBy(Log_Frequency ~ Common_Name*Log_Weight*Environement*Group*Group_Simple*Group_All*Order*Mastication*Congregatory*Log_Group_Size, data = Rhythm, 
                          FUN = list(median))

colnames(Median.R.Log) <- c("Common_Name","Weight","Environment","Group","Group_Simple","Group_All","Order","Mastication","Congregatory", "Group_Size", "Rhythm")

## Get Median DF

DF <- Rhythm[!(is.na(Rhythm$Log_Dominant_Frequency)),]
DF <- DF[!(is.na(DF$Log_Weight)),]

Median.DF <- summaryBy(Log_Dominant_Frequency ~ Common_Name*Log_Weight*Environement*Group*Group_Simple*Group_All*Order*Mastication*Congregatory*Log_Group_Size, data = DF, 
                       FUN = list(median))

colnames(Median.DF) <- c("Common_Name","Weight","Environment","Group","Group_Simple","Group_All","Order","Mastication","Congregatory", "Group_Size", "DF")

## Merged Median files

Median.DF.small <- subset (Median.DF, select=c("Common_Name", "DF"))

Median.Merge    <- merge(Median.R.Log, Median.DF.small, by=c("Common_Name")) 


## LOAD SNR 

filename <- paste("C:/Users/piette/Desktop/Rhythm_Study/SNR/master_snr_data.csv")

SNR      <- read.csv(filename, sep=";", dec=",", header=TRUE)

## LOAD Length
filename <- paste("C:/Users/piette/Desktop/Rhythm_Study/Length/master_length_data.csv")

Length   <- read.csv(filename, sep=";", dec=",", header=TRUE)

## LOAD Context
filename <- paste("C:/Users/piette/Desktop/Rhythm_Study/Context/context.csv")

Context  <- read.csv(filename, sep=";", dec=",", header=TRUE)

## Load Method
filename <- paste("C:/Users/piette/Desktop/Rhythm_Study/Rhythm_Alternative_Methods.csv")

Method   <- read.csv(filename, sep=";", dec=",", header=TRUE)

## Anova Method


aov.method <- aov(Rhythm~Type*Species, data=Method)
summary(aov.method)



## Anova Context*Species

aov.context <- aov(Rhythm~Species + Context, data=Context)
summary(aov.context)
TukeyHSD(aov.context, conf.level=.95)

Dog <- subset(Context, Species=="Dog")
Baboon <- subset(Context, Species=="Baboon")
Curlew <- subset(Context, Species=="Curlew")

aov.context.Baboon <- aov(Rhythm~Context, data=Baboon)
summary(aov.context.Baboon)
TukeyHSD(aov.context.Baboon, conf.level=.95)

aov.context.Dog <- aov(Rhythm~Context, data=Dog)
summary(aov.context.Dog)
TukeyHSD(aov.context.Dog, conf.level=.95)

aov.context.Curlew <- aov(Rhythm~Context, data=Curlew)
summary(aov.context.Curlew)
TukeyHSD(aov.context.Curlew, conf.level=.95)

## LM Rhythm~SNR*Lenght

lm.SNR <- lm(Frequency~SNR + Length, data=Rhythm)
summary(lm.SNR)


## Get beak size data

filename <-  paste ("C:/Users/piette/Desktop/Rhythm_Study/Avonet_Rhythm.csv")

avonet <- read.csv(filename, sep=";", dec=",", header=TRUE)
avonet <- as.data.frame(avonet)

beak_lm <- lm(Median~Beak_Length*(Beak_Width*Beak_Depth), data=avonet)
summary(beak_lm)

## Get Vocal Repertoire Data

library(tidyverse)

# Step 1: Load data from Excel sheets
filename <- paste("C:/Users/piette/Desktop/Rhythm_Study/Animal_Vocal_Repertoire.csv")
Vocal_Repertoire <- read.csv(filename, sep=";", dec=",", header=TRUE)

# Step 2: Merge the datasets based on the common species column
merged_data <- merge(Vocal_Repertoire, Median.R, by = "Common_Name", all = TRUE)



plot(Rhythm~Vocal_Repertoire, data=merged_data)
     
lm_voc_repertoire <- lm(Rhythm~Vocal_Repertoire, data=merged_data)
summary(lm_voc_repertoire)
