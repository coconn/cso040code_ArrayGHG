# figure-fluxes-ghgarraydrought.R
# 
# process the GHG flux data from the eosAnalyze program
# make GHG flux figures
# do statistical work on fluxes
# there's no moisture/O2/precip stuff in this script
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# see also (update before making this figure if needed!): 
# Process-arraysensorsdf-Sensor-Excel-Data-Rcode.R is where arraysensorsdf.csv gets built
# figure-precipO2moisture-ghgarraydrought.R - where the moisture/O2/precip stuff is

# output products:
# figures in folder /DroughtMSFigures/
# tables in folder /DroughtMSTables/



########################################################################
# GET READY TO BRING IN DATA

library(ggplot2)
library(gridExtra)
library(scales)
library(dplyr)
library(nlme)      ## for lme()
library(multcomp)  ## for multiple comparison stuff
#library(data.table)
#library(chron)
library(lubridate)
#library(lattice)
#library(reshape2)
#options(java.parameters = "-Xmx5000m") # make sure there's sufficient memory to open excel file
library(xlsx)
library(strucchange) # piecewise regression

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/DroughtMSFigures/"
pathGHGdata = "~/Documents/GITHUB/NOT REPOS/cso040code_ArrayGHG_LargeFiles/Chamber-data-large-files/eosAnalyzeACProcessed/"
pathrainfalldata = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Rainfall-data-Ryan/"
pathsavetab = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/DroughtMSTables/"
pathsoildata = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Soil-data/"


########################################################################
# BRING IN GHG DATA, MAKE DATAFRAME

# load csv
arrayGHGdf <- read.csv(paste(pathGHGdata, "ArrayGHG_master_chamber_data-deadbandcombined.csv", sep = ""), stringsAsFactors=FALSE)

# nice names
newnames <- c("DateTime","Chamber","MeasurementDuration_s","MeanCO2_ppm","MeanCH4_ppm","MeanN2O_ppm","MeanNH3_ppm","MeanH2O_percent","ChemDetect_0-1","CavPressure_kPa","CavTemperature_K","WaterContent_fraction","ChamberTemperature_K","ChamberPressure_kPa","FluxCO2_L_umolm2s","FluxCO2_E_umolm2s","FluxCH4_L_nmolm2s","FluxCH4_E_nmolm2s","FluxN2O_L_nmolm2s","FluxN2O_E_nmolm2s","FluxNH3_L_umolm2s","FluxNH3_E_umolm2s","e_FluxCO2_L_umolm2s","e_FluxCO2_E_umolm2s","e_FluxCH4_L_nmolm2s","e_FluxCH4_E_nmolm2s","e_FluxN2O_L_nmolm2s","e_FluxN2O_E_nmolm2s","e_FluxNH3_L_umolm2s","e_FluxNH3_E_umolm2s","AUXVoltage1_V","AUXVoltage2_V","AUXVoltage3_V","AUXCurrent1_mA","AUXCurrent2_mA","Metadata","Deadband","System","DataSource")
names(arrayGHGdf) <- newnames

# make dates nice
arrayGHGdf$DateTime2 <- mdy_hm(arrayGHGdf$DateTime)

# rip out date only
arrayGHGdf$Date <- substr(arrayGHGdf$DateTime2,1,11)
arrayGHGdf$Date <- ymd(arrayGHGdf$Date)

# assign topographic location

# Silver system assign
arrayGHGdf$Topo <- NA
arrayGHGdf$Topo[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="1" | arrayGHGdf$Chamber=="4" | arrayGHGdf$Chamber=="7")] <- "Ridge"
arrayGHGdf$Topo[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="2" | arrayGHGdf$Chamber=="5" | arrayGHGdf$Chamber=="8")] <- "Slope"
arrayGHGdf$Topo[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="3" | arrayGHGdf$Chamber=="6" | arrayGHGdf$Chamber=="9")] <- "Valley"

# Mayes system assign
arrayGHGdf$Topo[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="1" | arrayGHGdf$Chamber=="5" | arrayGHGdf$Chamber=="9")] <- "Ridge"
arrayGHGdf$Topo[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="2" | arrayGHGdf$Chamber=="3" | arrayGHGdf$Chamber=="6" | arrayGHGdf$Chamber=="7" | arrayGHGdf$Chamber=="10" | arrayGHGdf$Chamber=="11")] <- "Slope"
arrayGHGdf$Topo[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="4" | arrayGHGdf$Chamber=="8" | arrayGHGdf$Chamber=="12")] <- "Valley"

# what about the low-slope vs. upper-slope distinction?

# Silver system assign
arrayGHGdf$Topo4Cats <- NA
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="1" | arrayGHGdf$Chamber=="4" | arrayGHGdf$Chamber=="7")] <- "Ridge"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="5")] <- "Upper Slope"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="2" | arrayGHGdf$Chamber=="8")] <- "Low Slope"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Silver" & (arrayGHGdf$Chamber=="3" | arrayGHGdf$Chamber=="6" | arrayGHGdf$Chamber=="9")] <- "Valley"

# Mayes system assign
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="1" | arrayGHGdf$Chamber=="5" | arrayGHGdf$Chamber=="9")] <- "Ridge"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="2" | arrayGHGdf$Chamber=="6" | arrayGHGdf$Chamber=="10")] <- "Upper Slope"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="3" | arrayGHGdf$Chamber=="7" | arrayGHGdf$Chamber=="11")] <- "Low Slope"
arrayGHGdf$Topo4Cats[arrayGHGdf$System=="Mayes" & (arrayGHGdf$Chamber=="4" | arrayGHGdf$Chamber=="8" | arrayGHGdf$Chamber=="12")] <- "Valley"

# make factors where needed
arrayGHGdf$Chamber <- as.factor(arrayGHGdf$Chamber)
arrayGHGdf$Topo <- as.factor(arrayGHGdf$Topo)
arrayGHGdf$Topo4Cats <- as.factor(arrayGHGdf$Topo4Cats)
# str(arrayGHGdf)

# For the analyses below, keep arrayGHGdf as only the good deadband fluxes
arrayGHGdf_all <- arrayGHGdf
arrayGHGdf <- subset(arrayGHGdf_all, arrayGHGdf_all$Deadband=="Good_180")


########################################################################
# QUALITY CONTROL FOR GHG DATA

# how many observations did we start with?
gasobvsstart <- dim(arrayGHGdf)[1]

## inappropriate fluxes

# take out any fluxes associated with negative CO2 fluxes (troubleshoot these later)
# throw out all gases if CO2 is bad
arrayGHGdf <- subset(arrayGHGdf, arrayGHGdf$FluxCO2_E_umolm2s>=0)
arrayGHGdf <- subset(arrayGHGdf, arrayGHGdf$FluxCO2_L_umolm2s>=0)
# take out all gases if CO2 fluxes over abs(20) nmol/m2/s
arrayGHGdf <- subset(arrayGHGdf, arrayGHGdf$FluxCO2_E_umolm2s<20)
arrayGHGdf <- subset(arrayGHGdf, arrayGHGdf$FluxCO2_L_umolm2s<20)

# take out any N2O fluxes over abs(100) nmol/m2/s
arrayGHGdf$FluxN2O_E_nmolm2s[abs(arrayGHGdf$FluxN2O_E_nmolm2s) > 100] <- NA
arrayGHGdf$FluxN2O_L_nmolm2s[abs(arrayGHGdf$FluxN2O_L_nmolm2s) > 100] <- NA

# take out any CH4 fluxes over abs(1000) nmol/m2/s
arrayGHGdf$FluxCH4_E_nmolm2s[abs(arrayGHGdf$FluxCH4_E_nmolm2s) > 1000] <- NA
arrayGHGdf$FluxCH4_L_nmolm2s[abs(arrayGHGdf$FluxCH4_L_nmolm2s) > 1000] <- NA

## deployment period too long or short

# take out places where the deployment period of the chamber was too long or too short
toolong <- 661
tooshort <- 539
# print info about removed 
tmp <- arrayGHGdf$MeasurementDuration_s
tmp2 <- tmp[tmp < toolong]
tmp3 <- tmp2[tmp2 > tooshort]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where chamber duration length was shorter than ", 
    toolong, " and longer than ", tooshort, " seconds; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers deployed too long/short
arrayGHGdf <- subset(arrayGHGdf, MeasurementDuration_s > tooshort & MeasurementDuration_s < toolong)

## cavity pressure too high or low

# take out places where the cavity pressure wasn't in an ok range
toobig <- mean(arrayGHGdf$CavPressure_kPa) + 4*sd(arrayGHGdf$CavPressure_kPa)
toosmall <- mean(arrayGHGdf$CavPressure_kPa) - 4*sd(arrayGHGdf$CavPressure_kPa)
# print info about removed 
tmp <- arrayGHGdf$CavPressure_kPa
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where chamber cavity pressure was smaller than ", 
    toobig, " and larger than ", toosmall, " kPs; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, CavPressure_kPa > toosmall & CavPressure_kPa < toobig)

## cavity temperature too high or low

# take out places where the cavity temp wasn't in an ok range
toobig <- mean(arrayGHGdf$CavTemperature_K) + 4*sd(arrayGHGdf$CavTemperature_K)
toosmall <- mean(arrayGHGdf$CavTemperature_K) - 4*sd(arrayGHGdf$CavTemperature_K)
# print info about removed 
tmp <- arrayGHGdf$CavTemperature_K
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where chamber cavity temperature was lower than ", 
    toobig, " and higher than ", toosmall, " K; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, CavTemperature_K > toosmall & CavTemperature_K < toobig)


###### Nick thinks that the chamber pressure and temperature aren't a good thing to throw out data for (maybe screen and report, but don't dump the data)

## chamber pressure too high or low

# # take out places where the chamber pressure wasn't in an ok range
# toobig <- mean(arrayGHGdf$ChamberPressure_kPa) + 4*sd(arrayGHGdf$ChamberPressure_kPa)
# toosmall <- mean(arrayGHGdf$ChamberPressure_kPa) - 4*sd(arrayGHGdf$ChamberPressure_kPa)
# # print info about removed 
# tmp <- arrayGHGdf$ChamberPressure_kPa
# tmp2 <- tmp[tmp < toobig]
# tmp3 <- tmp2[tmp2 > toosmall]
# durationok <- length(tmp3)/length(tmp)
# cat("You have removed flux measurements where chamber cavity pressure was smaller than ", 
#     toobig, " and larger than ", toosmall, " kPs; this left ", durationok*100, 
#     "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")
# 
# # take out chambers with cavity pressure too extreme
# arrayGHGdf <- subset(arrayGHGdf, ChamberPressure_kPa > toosmall & ChamberPressure_kPa < toobig)
# 
# ## chamber temperature too high or low
# 
# # take out places where the cavity temp wasn't in an ok range
# toobig <- mean(arrayGHGdf$ChamberTemperature_K) + 4*sd(arrayGHGdf$ChamberTemperature_K)
# toosmall <- mean(arrayGHGdf$ChamberTemperature_K) - 4*sd(arrayGHGdf$ChamberTemperature_K)
# # print info about removed 
# tmp <- arrayGHGdf$ChamberTemperature_K
# tmp2 <- tmp[tmp < toobig]
# tmp3 <- tmp2[tmp2 > toosmall]
# durationok <- length(tmp3)/length(tmp)
# cat("You have removed flux measurements where chamber cavity temperature was lower than ", 
#     toobig, " and higher than ", toosmall, " K; this left ", durationok*100, 
#     "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")
# 
# # take out chambers with cavity pressure too extreme
# arrayGHGdf <- subset(arrayGHGdf, ChamberTemperature_K > toosmall & ChamberTemperature_K < toobig)




#### MAYBE SWITCH THIS TO A CUT OFF AND NOT A MEAN +/- STD OPTION

## H2O percent too high or low

# take out places where the cavity temp wasn't in an ok range
toobig <- mean(arrayGHGdf$MeanH2O_percent) + 4*sd(arrayGHGdf$MeanH2O_percent)
toosmall <- mean(arrayGHGdf$MeanH2O_percent) - 4*sd(arrayGHGdf$MeanH2O_percent)
# print info about removed 
tmp <- arrayGHGdf$MeanH2O_percent
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where mean percent H2O was lower than ", 
    toobig, "% and higher than ", toosmall, "%; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, MeanH2O_percent > toosmall & MeanH2O_percent < toobig)

## water content fraction too high or low

# take out places where the cavity temp wasn't in an ok range
toobig <- mean(arrayGHGdf$WaterContent_fraction) + 4*sd(arrayGHGdf$WaterContent_fraction)
toosmall <- mean(arrayGHGdf$WaterContent_fraction) - 4*sd(arrayGHGdf$WaterContent_fraction)
# print info about removed 
tmp <- arrayGHGdf$WaterContent_fraction
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where mean percent H2O was lower than ", 
    toobig, "% and higher than ", toosmall, "%; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, WaterContent_fraction > toosmall & WaterContent_fraction < toobig)

#write.csv(arrayGHGdf, file="sampledataforPicarro.csv")

## instances where the mean ppm of a gas is negative or too high

# CO2 mean ppm is too high (indicated potential problem with chamber opening/closing)
toobig <- mean(arrayGHGdf$MeanCO2_ppm) + 4*sd(arrayGHGdf$MeanCO2_ppm)
toosmall <- mean(arrayGHGdf$MeanCO2_ppm) - 4*sd(arrayGHGdf$MeanCO2_ppm)
# print info about removed 
tmp <- arrayGHGdf$MeanCO2_ppm
tmp2 <- tmp[tmp < toobig]
tmp3 <- tmp2[tmp2 > toosmall]
durationok <- length(tmp3)/length(tmp)
cat("You have removed flux measurements where mean ppm CO2 was lower than ", 
    toobig, " and higher than ", toosmall, "; this left ", durationok*100, 
    "% of the samples remaining (", length(tmp3), " of ", length(tmp), ").", sep="")

# take out chambers with cavity pressure too extreme
arrayGHGdf <- subset(arrayGHGdf, MeanCO2_ppm > toosmall & MeanCO2_ppm < toobig)

# remove instances with negative ppms
# print info about removed 
tmp <- arrayGHGdf$MeanCH4_ppm
negCO2 <- sum(arrayGHGdf$MeanCO2_ppm < 0)
negCH4 <- sum(arrayGHGdf$MeanCH4_ppm < 0)
negN2O <- sum(arrayGHGdf$MeanN2O_ppm < 0)
cat("There were ", negCO2, ", ", negCH4, ", and ", negN2O,
    " observations for CO2, CH4, and N2O respectively where the mean ppm over the ",
    "observation period was negative. ", (negCO2/length(tmp))*100, "%, ",
    (negCH4/length(tmp))*100, "%, and ", (negN2O/length(tmp))*100, "% of the ",
    "observations for CO2, CH4, and N2O respectively will be removed.", sep="")

# cut fluxes for the gas that had the negative reading

# take out any CO2 fluxes with negative ppm values
arrayGHGdf$FluxCO2_E_umolm2s[arrayGHGdf$MeanCO2_ppm < 0] <- NA
arrayGHGdf$FluxCO2_L_umolm2s[arrayGHGdf$MeanCO2_ppm < 0] <- NA

# take out any N2O fluxes with negative ppm values
arrayGHGdf$FluxN2O_E_nmolm2s[arrayGHGdf$MeanN2O_ppm < 0] <- NA
arrayGHGdf$FluxN2O_L_nmolm2s[arrayGHGdf$MeanN2O_ppm < 0] <- NA

# take out any CH4 fluxes with negative ppm values
arrayGHGdf$FluxCH4_E_nmolm2s[arrayGHGdf$MeanCH4_ppm < 0] <- NA
arrayGHGdf$FluxCH4_L_nmolm2s[arrayGHGdf$MeanCH4_ppm < 0] <- NA


## what did we end up with?
gasobvsend <- dim(arrayGHGdf)[1]
remainingobvs <- gasobvsend/gasobvsstart
cat("You began the data quality process with ", gasobvsstart, " observations for each gas. ",
    "You are left with ", gasobvsend, " for ", remainingobvs*100, "% remaining. ",
    ((gasobvsstart-gasobvsend)/gasobvsstart)*100, "% of the observations were removed.", 
    sep="")

# what about for each gas?
naCO2 <- sum(is.na(arrayGHGdf$FluxCO2_L_umolm2s) & is.na(arrayGHGdf$FluxCO2_E_umolm2s))
naCH4 <- sum(is.na(arrayGHGdf$FluxCH4_L_nmolm2s) & is.na(arrayGHGdf$FluxCH4_E_nmolm2s))
naN2O <- sum(is.na(arrayGHGdf$FluxN2O_L_nmolm2s) & is.na(arrayGHGdf$FluxN2O_E_nmolm2s))
cat("Of those ", gasobvsend, " final observations, ",
    naCO2, " fluxes are NA for both the linear and exponential flux estimates of CO2. ",
    "For CH4 and N2O respectively, that situation is true for ", naCH4, " and ", naN2O,
    " of the observations.  That leaves you with ", gasobvsend-naCO2,
    " CO2 flux observations, ", gasobvsend-naCH4, " for CH4, and ", 
    gasobvsend-naN2O, " for N2O.", sep="")


########################################################################
# CHEM DETECT AND OTHER DATA QUALITY GRAPHS

# see evernote "Nick and Chance chemdetect mtg" for info about steps to take

# does meanN2Oppb drift over time or is it event based? should be ~320 +/- 30 ppb
N2Odrift1 <- ggplot(arrayGHGdf, aes(x=DateTime2, y=MeanN2O_ppm, color=Topo)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"))

# save figures
png(file = paste(pathsavefigs, "drift_N2Oppm.png", sep=""),width=14,height=7,units="in",res=150)
N2Odrift1
dev.off()

# chance could put in the average chemdetect value and not only the binary chemdetect value
chemdetect1 <- ggplot(arrayGHGdf, aes(x=DateTime2, y=`ChemDetect_0-1`, color=Topo)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"))

png(file = paste(pathsavefigs, "drift_chemdetect.png", sep=""),width=14,height=7,units="in",res=150)
chemdetect1
dev.off()

# chem detect diagnostic figures to send out to Eosense, Picarro
# these were done early in the process of trying to figure out the issues, and I haven't updated the figures since then
# don't always re-do these
makechemdetectfigs <- 2.5
if(makechemdetectfigs < 2) {
  
  ## what about chem_detect_0-1?
  
  sum(arrayGHGdf$`ChemDetect_0-1`)
  length(arrayGHGdf$`ChemDetect_0-1`)
  2471/9462
  # [1] 0.2611499
  # unfortunately, if chemdetect ends up being something we should cut, that's 25% of the samples... so for now I'm not going to cut it
  
  # diagnoses on chemdetect
  
  chemdetect0 <- subset(arrayGHGdf, `ChemDetect_0-1`==0)
  chemdetect1 <- subset(arrayGHGdf, `ChemDetect_0-1`==1)
  
  summary(chemdetect0$FluxCO2_L_umolm2s)
  summary(chemdetect1$FluxCO2_L_umolm2s)
  
  summary(chemdetect0$FluxCO2_E_umolm2s)
  summary(chemdetect1$FluxCO2_E_umolm2s)
  
  summary(chemdetect0$FluxCH4_L_nmolm2s)
  summary(chemdetect1$FluxCH4_L_nmolm2s)
  
  summary(chemdetect0$FluxCH4_E_nmolm2s)
  summary(chemdetect1$FluxCH4_E_nmolm2s)
  
  summary(chemdetect0$FluxN2O_L_nmolm2s)
  summary(chemdetect1$FluxN2O_L_nmolm2s)
  
  summary(chemdetect0$FluxN2O_E_nmolm2s)
  summary(chemdetect1$FluxN2O_E_nmolm2s)
  
  # graphs for chemdetect
  
  CDgraph1 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxCO2_L_umolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph2 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxCO2_E_umolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph3 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxN2O_L_nmolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph4 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxN2O_E_nmolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph5 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxCH4_L_nmolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  CDgraph6 <- ggplot(data=arrayGHGdf, aes(arrayGHGdf$FluxCH4_E_nmolm2s)) + geom_histogram() + facet_grid( ~ `ChemDetect_0-1`)
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses.png", sep=""),width=20,height=15,units="in",res=400)
  grid.arrange(CDgraph1, CDgraph2, CDgraph3, CDgraph4, CDgraph5, CDgraph6, nrow = 3, ncol = 2)
  dev.off()
  
  # more graphs for chemdetect
  
  # 1) Group the ChemDetect = 1 by chamber and see if its one specific chamber that is throwing the signals.
  # 2) Group the ChemDetect = 1 by date and see if its a specific date range (or several specific date ranges) that we need to be worrying about.
  
  CDgraph7 <- ggplot(arrayGHGdf, aes(x=Date, y=`ChemDetect_0-1`)) + geom_point() + facet_wrap(~Chamber, nrow=3)
  
  CDgraph8 <- ggplot(arrayGHGdf, aes(x=Date, y=`ChemDetect_0-1`)) + geom_point() + facet_wrap(~Chamber, nrow=9)
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses2a.png", sep=""),width=12,height=6,units="in",res=400)
  CDgraph7
  dev.off()
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses2b.png", sep=""),width=3,height=10,units="in",res=400)
  CDgraph8
  dev.off()
  
  # 3) A scatter plot of ChemDetect vs. each gas species average concentration - Water, Ammonia, CH4, N2O,CO2 (which I assume you’ve output as part of your data file).
  
  CDgraph9a <- ggplot(arrayGHGdf, aes(x=MeanCO2_ppm, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9b <- ggplot(arrayGHGdf, aes(x=MeanCH4_ppm, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9c <- ggplot(arrayGHGdf, aes(x=MeanN2O_ppm, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9d <- ggplot(arrayGHGdf, aes(x=MeanNH3_ppm, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9e <- ggplot(arrayGHGdf, aes(x=MeanH2O_percent, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph9f <- ggplot(arrayGHGdf, aes(x=MeasurementDuration_s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="right")
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses3.png", sep=""),width=12,height=6,units="in",res=400)
  grid.arrange(CDgraph9a, CDgraph9b, CDgraph9c, CDgraph9d, CDgraph9e, CDgraph9f, nrow = 2, ncol = 3)
  dev.off()
  
  # 4) A scatter plot of ChemDetect vs. the Error values for the linear fluxes for each gas
  
  CDgraph10a <- ggplot(arrayGHGdf, aes(x=e_FluxCO2_L_umolm2s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph10b <- ggplot(arrayGHGdf, aes(x=e_FluxCH4_L_nmolm2s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph10c <- ggplot(arrayGHGdf, aes(x=e_FluxN2O_L_nmolm2s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="none")
  CDgraph10d <- ggplot(arrayGHGdf, aes(x=e_FluxNH3_L_umolm2s, y=`ChemDetect_0-1`, color=Chamber)) + geom_point() + theme(legend.position="right")
  
  png(file = paste(pathsavefigs, "chemdetectdiagnoses4.png", sep=""),width=8,height=5,units="in",res=400)
  grid.arrange(CDgraph10a, CDgraph10b, CDgraph10c, CDgraph10d, nrow = 2, ncol = 2)
  dev.off()
  
}

## for now, remove the data that had chemdetect running more than some proportion of the time
# actually, let's only remove the N2O data from those time periods

# take out places where chemdetect is above the threshold
chemdetectcutoff <- 0.15
# print info about removed
tmp <- arrayGHGdf$`ChemDetect_0-1`
tmp2 <- tmp[tmp <= chemdetectcutoff]
durationok <- length(tmp2)/length(tmp)
cat("You have removed flux measurements where ChemDetect_0-1 was flagged more than the threshold proportion (",chemdetectcutoff, "); this left ", durationok*100, "% of the samples remaining (", length(tmp2), " of ", length(tmp), ").", sep="")

# take out N2O fluxes where chemdetect is above the threshold
arrayGHGdf$FluxN2O_E_nmolm2s[arrayGHGdf$`ChemDetect_0-1` > chemdetectcutoff] <- NA
arrayGHGdf$FluxN2O_L_nmolm2s[arrayGHGdf$`ChemDetect_0-1` > chemdetectcutoff] <- NA
cat("Removing the N2O fluxes from the bad ChemDetect periods only.", sep="")
# if taking out all fluxes:
# arrayGHGdf <- subset(arrayGHGdf, `ChemDetect_0-1` <= chemdetectcutoff)


########################################################################
# EXPONENTIAL VS. LINEAR FLUX ESTIMATE DECISION

## which flux gets preferred, linear or exponential, for each observation and gas?

# define ratio of lin flux:lin error and expon flux:expon error for each gas
arrayGHGdf$CO2errratio_L <- (arrayGHGdf$e_FluxCO2_L_umolm2s)/(arrayGHGdf$FluxCO2_L_umolm2s)
arrayGHGdf$CO2errratio_E <- (arrayGHGdf$e_FluxCO2_E_umolm2s)/(arrayGHGdf$FluxCO2_E_umolm2s)
arrayGHGdf$CH4errratio_L <- (arrayGHGdf$e_FluxCH4_L_nmolm2s)/(arrayGHGdf$FluxCH4_L_nmolm2s)
arrayGHGdf$CH4errratio_E <- (arrayGHGdf$e_FluxCH4_E_nmolm2s)/(arrayGHGdf$FluxCH4_E_nmolm2s)
arrayGHGdf$N2Oerrratio_L <- (arrayGHGdf$e_FluxN2O_L_nmolm2s)/(arrayGHGdf$FluxN2O_L_nmolm2s)
arrayGHGdf$e_FluxN2O_E_nmolm2s <- as.numeric(arrayGHGdf$e_FluxN2O_E_nmolm2s)
arrayGHGdf$N2Oerrratio_E <- (arrayGHGdf$e_FluxN2O_E_nmolm2s)/(arrayGHGdf$FluxN2O_E_nmolm2s)

# assign the flux with the better ratio to a "flux to use" column, another column that says whether that was the linear or the exponential flux, and another column with the error ratio

# CO2 fluxuse and fluxusetype
arrayGHGdf <- transform(arrayGHGdf, CO2_umolm2s_fluxuse = ifelse(is.na(FluxCO2_L_umolm2s), as.numeric(FluxCO2_E_umolm2s), ifelse(is.na(FluxCO2_E_umolm2s), as.numeric(FluxCO2_L_umolm2s), ifelse(CO2errratio_L>CO2errratio_E, as.numeric(FluxCO2_E_umolm2s), as.numeric(FluxCO2_L_umolm2s)))))

arrayGHGdf <- transform(arrayGHGdf, CO2_umolm2s_fluxusetype = ifelse(is.na(FluxCO2_L_umolm2s), as.character("exponential"), ifelse(is.na(FluxCO2_E_umolm2s), as.character("linear"), ifelse(CO2errratio_L>CO2errratio_E, as.character("exponential"), as.character("linear")))))

arrayGHGdf <- transform(arrayGHGdf, CO2_umolm2s_fluxuseerror = ifelse(is.na(FluxCO2_L_umolm2s), as.numeric(CO2errratio_E), ifelse(is.na(FluxCO2_E_umolm2s), as.numeric(CO2errratio_L), ifelse(CO2errratio_L>CO2errratio_E, as.numeric(CO2errratio_E), as.numeric(CO2errratio_L)))))

# print info about what fluxes made the final dataset
linsum <- sum(arrayGHGdf$CO2_umolm2s_fluxusetype=="linear")
expsum <- sum(arrayGHGdf$CO2_umolm2s_fluxusetype=="exponential")
totalnum <- length(arrayGHGdf$CO2_umolm2s_fluxusetype)
cat("Of the ", totalnum, " final CO2 fluxes, ",
    linsum, " fluxes were best estimated with a linear fit and ", expsum,
    " were best estimated with an exponential fit. The mean",
    " error ratio of the fluxes used was ", 
    mean(is.finite(abs(arrayGHGdf$CO2_umolm2s_fluxuseerror))), ".", sep="")

# what is a reasonable cut off for error?
tmp <- sort(arrayGHGdf$CO2_umolm2s_fluxuseerror, decreasing = T)
tmp[1:50]

# if error is over a certain ratio, cut that flux
# moreover, if the CO2 error is bad, then cut all the gases
errorcutoff <- 0.10
# print info about removed
tmp <- arrayGHGdf$CO2_umolm2s_fluxuseerror
tmp2 <- tmp[tmp <= errorcutoff]
durationok <- length(tmp2)/length(tmp)
cat("You have removed flux measurements where the CO2 error ratio was above the threshold proportion (",errorcutoff, "); this left ", durationok*100, "% of the samples remaining (", length(tmp2), " of ", length(tmp), ").", sep="")
# take out all fluxes above the threshold
arrayGHGdf <- subset(arrayGHGdf, CO2_umolm2s_fluxuseerror <= errorcutoff)

# if you want to see how the error is distributed:
# ggplot(arrayGHGdf, aes(x=DateTime2, y=CO2_umolm2s_fluxuse, color=CO2_umolm2s_fluxuseerror)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"))

# CH4 fluxuse and fluxusetype
arrayGHGdf <- transform(arrayGHGdf, CH4_nmolm2s_fluxuse = ifelse(is.na(FluxCH4_L_nmolm2s), as.numeric(FluxCH4_E_nmolm2s), ifelse(is.na(FluxCH4_E_nmolm2s), as.numeric(FluxCH4_L_nmolm2s), ifelse(CH4errratio_L>CH4errratio_E, as.numeric(FluxCH4_E_nmolm2s), as.numeric(FluxCH4_L_nmolm2s)))))

arrayGHGdf <- transform(arrayGHGdf, CH4_nmolm2s_fluxusetype = ifelse(is.na(FluxCH4_L_nmolm2s), as.character("exponential"), ifelse(is.na(FluxCH4_E_nmolm2s), as.character("linear"), ifelse(CH4errratio_L>CH4errratio_E, as.character("exponential"), as.character("linear")))))

arrayGHGdf <- transform(arrayGHGdf, CH4_nmolm2s_fluxuseerror = ifelse(is.na(FluxCH4_L_nmolm2s), as.numeric(CH4errratio_E), ifelse(is.na(FluxCH4_E_nmolm2s), as.numeric(CH4errratio_L), ifelse(CH4errratio_L>CH4errratio_E, as.numeric(CH4errratio_E), as.numeric(CH4errratio_L)))))

# print info about what fluxes made the final dataset
linsum <- sum(arrayGHGdf$CH4_nmolm2s_fluxusetype=="linear")
expsum <- sum(arrayGHGdf$CH4_nmolm2s_fluxusetype=="exponential")
totalnum <- length(arrayGHGdf$CH4_nmolm2s_fluxusetype)
cat("Of the ", totalnum, " final CH4 fluxes, ",
    linsum, " fluxes were best estimated with a linear fit and ", expsum,
    " were best estimated with an exponential fit. The mean",
    " error ratio of the fluxes used was ", 
    mean(is.finite(abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror))), ".", sep="")

# what is a reasonable cut off for error?
tmp <- sort(abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror), decreasing = T)
tmp[1000:1050]
ggplot(arrayGHGdf, aes(x=DateTime2, y=CH4_nmolm2s_fluxuseerror, color=CH4_nmolm2s_fluxuse)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d")) + ylim(-100000, 1000)

ggplot(arrayGHGdf, aes(x=CH4_nmolm2s_fluxuse, y=abs(CH4_nmolm2s_fluxuseerror), color=Topo)) + geom_point() + theme_bw() + ylim(-10,200) + xlim(-30,50)

ggplot(arrayGHGdf, aes(x=DateTime2, y=CH4_nmolm2s_fluxuse, color=abs(CH4_nmolm2s_fluxuseerror))) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d")) + scale_colour_gradient(limits=c(0, 100), low="red", high="black")

# if error is over a certain ratio, cut that flux
# don't cut the other gases just because CH4 is off
errorcutoff <- 50000
# print info about removed
tmp <- abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror)
tmp2 <- tmp[tmp <= errorcutoff]
durationok <- length(tmp2)/length(tmp)
cat("You have removed flux measurements where the CH4 error ratio was above the threshold proportion (",errorcutoff, "); this left ", durationok*100, "% of the samples remaining (", length(tmp2), " of ", length(tmp), ").", sep="")
# take out any CH4 fluxes with error above the threshold
arrayGHGdf$CH4_nmolm2s_fluxuse[abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror) > errorcutoff] <- NA
arrayGHGdf$CH4_nmolm2s_fluxusetype[abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror) > errorcutoff] <- NA

# N2O fluxuse and fluxusetype
arrayGHGdf <- transform(arrayGHGdf, N2O_nmolm2s_fluxuse = ifelse(is.na(FluxN2O_L_nmolm2s), as.numeric(FluxN2O_E_nmolm2s), ifelse(is.na(FluxN2O_E_nmolm2s), as.numeric(FluxN2O_L_nmolm2s), ifelse(N2Oerrratio_L>N2Oerrratio_E, as.numeric(FluxN2O_E_nmolm2s), as.numeric(FluxN2O_L_nmolm2s)))))

arrayGHGdf <- transform(arrayGHGdf, N2O_nmolm2s_fluxusetype = ifelse(is.na(FluxN2O_L_nmolm2s), as.character("exponential"), ifelse(is.na(FluxN2O_E_nmolm2s), as.character("linear"), ifelse(N2Oerrratio_L>N2Oerrratio_E, as.character("exponential"), as.character("linear")))))

arrayGHGdf <- transform(arrayGHGdf, N2O_nmolm2s_fluxuseerror = ifelse(is.na(FluxN2O_L_nmolm2s), as.numeric(N2Oerrratio_E), ifelse(is.na(FluxN2O_E_nmolm2s), as.numeric(N2Oerrratio_L), ifelse(N2Oerrratio_L>N2Oerrratio_E, as.numeric(N2Oerrratio_E), as.numeric(N2Oerrratio_L)))))

# print info about what fluxes made the final dataset
linsum <- sum(arrayGHGdf$N2O_nmolm2s_fluxusetype=="linear")
expsum <- sum(arrayGHGdf$N2O_nmolm2s_fluxusetype=="exponential")
totalnum <- length(arrayGHGdf$N2O_nmolm2s_fluxusetype)
cat("Of the ", totalnum, " final N2O fluxes, ",
    linsum, " fluxes were best estimated with a linear fit and ", expsum,
    " were best estimated with an exponential fit. The mean",
    " error ratio of the fluxes used was ", 
    mean(is.finite(abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror))), ".", sep="")

# what is a reasonable cut off for error?
tmp <- sort(abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror), decreasing = T)
tmp[1:50]
ggplot(arrayGHGdf, aes(x=DateTime2, y=N2O_nmolm2s_fluxuseerror, color=N2O_nmolm2s_fluxuse)) + geom_point() + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d")) + ylim(-50000, 1000)

# if error is over a certain ratio, cut that flux
# don't cut the other gases just because N2O is off
errorcutoff <- 100
# print info about removed
tmp <- abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror)
tmp2 <- tmp[tmp <= errorcutoff]
durationok <- length(tmp2)/length(tmp)
cat("You have removed flux measurements where the N2O error ratio was above the threshold proportion (",errorcutoff, "); this left ", durationok*100, "% of the samples remaining (", length(tmp2), " of ", length(tmp), ").", sep="")
# take out any CH4 fluxes with error above the threshold
arrayGHGdf$N2O_nmolm2s_fluxuse[abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror) > errorcutoff] <- NA
arrayGHGdf$N2O_nmolm2s_fluxusetype[abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror) > errorcutoff] <- NA

# mean(arrayGHGdf$CO2errratio_L, na.rm=T)
# mean(arrayGHGdf$CO2errratio_E, na.rm=T)
# mean(arrayGHGdf$CH4errratio_L, na.rm=T)
# mean(arrayGHGdf$CH4errratio_E, na.rm=T)
# mean(arrayGHGdf$N2Oerrratio_L, na.rm=T)
# mean(is.finite(arrayGHGdf$N2Oerrratio_E), na.rm=T)

# these are the average error values before any high error ratio fluxes got thrown out
mean(abs(arrayGHGdf$CO2_umolm2s_fluxuseerror), na.rm=T)
mean(abs(arrayGHGdf$CH4_nmolm2s_fluxuseerror), na.rm=T)
mean(abs(arrayGHGdf$N2O_nmolm2s_fluxuseerror), na.rm=T)


########################################################################
# SUMMARY STATS: CO2, CH4, N2O ACROSS TRANSECTS AT EACH DATE

## look at time series figures to make sure that nothing is weird
# recall that if you cut the chemdetect=1 samples, there are arge chunks of data missing
# do the box plot figures (for each drought period) after this section

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE CO2
summarytab3atmp <- summarySE(data=arrayGHGdf, measurevar="CO2_umolm2s_fluxuse", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)

summarytab3btmp <- summarySE(data=arrayGHGdf, measurevar="CO2_umolm2s_fluxuse", c("Date", "Topo"), na.rm=TRUE, renameallcols=TRUE)

# summarySE CH4
summarytab4atmp <- summarySE(data=arrayGHGdf, measurevar="CH4_nmolm2s_fluxuse", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)

summarytab4btmp <- summarySE(data=arrayGHGdf, measurevar="CH4_nmolm2s_fluxuse", c("Date", "Topo"), na.rm=TRUE, renameallcols=TRUE)

# summarySE N2O
summarytab5atmp <- summarySE(data=arrayGHGdf, measurevar="N2O_nmolm2s_fluxuse", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)

summarytab5btmp <- summarySE(data=arrayGHGdf, measurevar="N2O_nmolm2s_fluxuse", c("Date", "Topo"), na.rm=TRUE, renameallcols=TRUE)

# convert summary table dates
#summarytab3a$Date <- ymd_hms(summarytab3atmp$Date)
#summarytab2$Date <- ymd_hms(summarytab2$Date)

# take out weird NA lines
#summarytab1 <- subset(summarytab1tmp,summarytab1tmp$N>0.5)
#summarytab2 <- subset(summarytab2tmp,summarytab2tmp$N>0.5)


########################################################################
# CO2, CH4, N2O TIME SERIES

#topocolorsGHG <- c("blue2","darkmagenta","firebrick2")
#topocolorsGHG <- rev(colorRampPalette(c('red','blue'), space = "Lab")(3))
topocolorsGHG <- rev(colorRampPalette(c('red','orange','purple','blue','dark blue'), space = "Lab")(3))

topobreaks <- c("1","2","3","4","5","6","7")
topolabs <- c("1 (Ridge)","2","3","4","5","6","7 (Valley)")

# CO2 by date (mean and se)
p3 <- ggplot(summarytab3atmp, aes(x=Date, y=meanCO2_umolm2s_fluxuse, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanCO2_umolm2s_fluxuse-seCO2_umolm2s_fluxuse, ymax=meanCO2_umolm2s_fluxuse+seCO2_umolm2s_fluxuse), alpha=0.5) + ylab("CO2 Flux (umol/m^2/s),\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line()

# CH4 by date (mean and se)
p4 <- ggplot(summarytab4atmp, aes(x=Date, y=meanCH4_nmolm2s_fluxuse, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanCH4_nmolm2s_fluxuse-seCH4_nmolm2s_fluxuse, ymax=meanCH4_nmolm2s_fluxuse+seCH4_nmolm2s_fluxuse), alpha=0.5) + ylab("CH4 Flux (nmol/m^2/s),\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) + ylim(-10,70) #+ geom_line()

# N2O by date (mean and se)
p5 <- ggplot(summarytab5atmp, aes(x=Date, y=meanN2O_nmolm2s_fluxuse, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanN2O_nmolm2s_fluxuse-seN2O_nmolm2s_fluxuse, ymax=meanN2O_nmolm2s_fluxuse+seN2O_nmolm2s_fluxuse), alpha=0.5) + ylab("N2O Flux (nmol/m^2/s),\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line()

# save figures
png(file = paste(pathsavefigs, "time_series_CO2.png", sep=""),width=10,height=7,units="in",res=400)
p3
dev.off()

# save figures
png(file = paste(pathsavefigs, "time_series_CH4.png", sep=""),width=10,height=7,units="in",res=400)
p4
dev.off()

# save figures
png(file = paste(pathsavefigs, "time_series_N2O.png", sep=""),width=10,height=7,units="in",res=400)
p5
dev.off()


########################################################################
# COMBINE TIME SERIES FIGS INTO ONE FIGURE

# # save figures
# png(file = paste(pathsavefigs, "time_series_allpanels.png", sep=""),width=10,height=25,units="in",res=400)
# grid.arrange(p0, p1, p2, p3, p4, p5, nrow = 6, ncol = 1)
# dev.off()
# 
# # try another orientation
# png(file = paste(pathsavefigs, "time_series_allpanels_v2.png", sep=""),width=20,height=15,units="in",res=400)
# # move the legends around so the new orientation isn't wonky
# grid.arrange(p0, p3 + theme(legend.position="none"), p2 + theme(legend.position="none"), p4 + theme(legend.position="none"), p1 + theme(legend.position="bottom"), p5 + theme(legend.position="bottom"), nrow = 3, ncol = 2)
# dev.off()

# only the gas panels
png(file = paste(pathsavefigs, "time_series_gaspanels.png", sep=""),width=8,height=12,units="in",res=400)
grid.arrange(p3, p4, p5, nrow = 3, ncol = 1)
dev.off()


########################################################################
# CO2, CH4, N2O BAR GRAPHS BY DROUGHT TIME PERIOD

# define the four time periods
# pre-drought: before 2015-04-20
# drought: 2015-04-21 to 2015-08-20, 16 weeks
# drought recovery: 2015-08-21 to 2015-11-02, 10 weeks
# post-drought: after 2015-11-03
#### note that Christine manually updated these below based on the piecewise regression results

# assign topographic location
# have to run these in this order
arrayGHGdf$DroughtTimePds <- "Post-Drought"
arrayGHGdf$DroughtTimePds[arrayGHGdf$Date < "2015-11-24"] <- "Drought Recovery"
arrayGHGdf$DroughtTimePds[arrayGHGdf$Date < "2015-08-24"] <- "Drought"
arrayGHGdf$DroughtTimePds[arrayGHGdf$Date < "2015-04-25"] <- "Pre-Drought"

# make DroughtTimePds a factor
arrayGHGdf$DroughtTimePds <- as.factor(arrayGHGdf$DroughtTimePds)
# order the levels so the plot is temporally appropriate
print(levels(arrayGHGdf$DroughtTimePds))
arrayGHGdf$DroughtTimePds <- factor(arrayGHGdf$DroughtTimePds,levels(arrayGHGdf$DroughtTimePds)[c(4,1,2,3)])

# save final GHG flux csv
fname = paste(pathsavetab, "arrayGHGdf_processed.csv" ,sep="")
write.csv(arrayGHGdf, file = fname, na = "")


# get means, std, ste for each period

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE sets - by drought period and topographic location for each gas, using fluxuse variables
summarytabDroughtTimePdsA1 <- summarySE(data=arrayGHGdf, measurevar="CO2_umolm2s_fluxuse", c("DroughtTimePds", "Topo"), na.rm=TRUE, renameallcols=TRUE)
summarytabDroughtTimePdsA2 <- summarySE(data=arrayGHGdf, measurevar="CO2_umolm2s_fluxuse", c("DroughtTimePds"), na.rm=TRUE, renameallcols=TRUE)

summarytabDroughtTimePdsB1 <- summarySE(data=arrayGHGdf, measurevar="CH4_nmolm2s_fluxuse", c("DroughtTimePds", "Topo"), na.rm=TRUE, renameallcols=TRUE)
summarytabDroughtTimePdsB2 <- summarySE(data=arrayGHGdf, measurevar="CH4_nmolm2s_fluxuse", c("DroughtTimePds"), na.rm=TRUE, renameallcols=TRUE)

summarytabDroughtTimePdsC1 <- summarySE(data=arrayGHGdf, measurevar="N2O_nmolm2s_fluxuse", c("DroughtTimePds", "Topo"), na.rm=TRUE, renameallcols=TRUE)
summarytabDroughtTimePdsC2 <- summarySE(data=arrayGHGdf, measurevar="N2O_nmolm2s_fluxuse", c("DroughtTimePds"), na.rm=TRUE, renameallcols=TRUE)


########################################################################
# TWO-WAY REPEATED MEASURES ANOVA OF FLUXES BY DROUGHT PERIOD, TOPO: CO2

# read these on lme() for two-way repeated measures
# http://www.researchgate.net/post/Has_anyone_performed_linear_mixed_model_with_repeated_measures
# http://www.jason-french.com/tutorials/repeatedmeasures.html

##### do I have to do interaction terms for these tests?  Because they're all significant.  Then what happens?

# Topo and DroughtTimePds are both already factors
# str(arrayGHGdf)

## CO2 fluxes

# lme can't handle columns with any NAs
arrayGHGdf_noNA <- subset(arrayGHGdf, !is.na(CO2_umolm2s_fluxuse))

# should log transform based on the diagnostic plots of doing this without transformation

# aov version of the two-way test
aov_twoway_droughttopo = aov(log(CO2_umolm2s_fluxuse) ~ DroughtTimePds + Topo + Error(Date/(DroughtTimePds + Topo)), data=arrayGHGdf_noNA)
# lme version of the one-way test
lme_twoway_droughttopo = lme(log(CO2_umolm2s_fluxuse) ~ DroughtTimePds + Topo, data=arrayGHGdf_noNA, random = ~1|Date/Topo/DroughtTimePds)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_twoway_droughttopo)
anova(lme_twoway_droughttopo)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/CO2_umolm2s_fluxuse_twowayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway_droughttopo,col=arrayGHGdf_noNA$DroughtTimePds, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "ANOVAdiagnostics/CO2_umolm2s_fluxuse_twowayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_twoway_droughttopo),sqrt(abs(resid(lme_twoway_droughttopo))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "ANOVAdiagnostics/CO2_umolm2s_fluxuse_twowayANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_twoway_droughttopo,col=arrayGHGdf_noNA$DroughtTimePds, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/CO2_umolm2s_fluxuse_twowayANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway_droughttopo,DroughtTimePds~resid(.))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/CO2_umolm2s_fluxuse_twowayANOVAdiagnostics_5.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway_droughttopo,Topo~resid(.))
dev.off()

# save anova info

# post hoc test
posthoc_glht_1 <- summary(glht(lme_twoway_droughttopo, linfct=mcp(DroughtTimePds = "Tukey")), test = adjusted(type = "bonferroni"))
posthoc_glht_2 <- summary(glht(lme_twoway_droughttopo, linfct=mcp(Topo = "Tukey")), test = adjusted(type = "bonferroni"))

# save posthoc test as a nice table later
x <- posthoc_glht_1
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_1 <- mtests

# save posthoc test as a nice table later
x <- posthoc_glht_2
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_2 <- mtests

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_twoway_droughttopo), file=paste(path.expand(pathsavetab), "stats-tables/CO2_twowayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests_1, file=paste(path.expand(pathsavetab), "stats-tables/CO2_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #1", append=TRUE)
write.xlsx(mtests_2, file=paste(path.expand(pathsavetab), "stats-tables/CO2_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #2", append=TRUE)

# set significance stars for graphs
cor_stars <- numeric(length=2)
# cycle through to set number of stars
for (i in 1:2) {
  
  if(anova(lme_twoway_droughttopo)[1+i,4] < 0.001){
    cor_stars[i] <- "***"
  } else if(anova(lme_twoway_droughttopo)[1+i,4] < 0.01){
    cor_stars[i] <- "**"
  } else if(anova(lme_twoway_droughttopo)[1+i,4] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_CO2twoway <- cor_stars


########################################################################
# TWO-WAY REPEATED MEASURES ANOVA OF FLUXES BY DROUGHT PERIOD, TOPO: CH4

## CH4 fluxes

# lme can't handle columns with any NAs
arrayGHGdf_noNA <- subset(arrayGHGdf, !is.na(CH4_nmolm2s_fluxuse))

# should NOT log transform based on the diagnostic plots of doing this without transformation
# the diagnostic plots are crappy, but the diagnostic plots were equally crappy when I log transformed

# aov version of the two-way test
aov_twoway_droughttopo = aov(CH4_nmolm2s_fluxuse ~ DroughtTimePds + Topo + Error(Date/(DroughtTimePds + Topo)), data=arrayGHGdf_noNA)
# lme version of the one-way test
lme_twoway_droughttopo = lme(log(CH4_nmolm2s_fluxuse) ~ DroughtTimePds + Topo, data=arrayGHGdf_noNA, random = ~1|Date/Topo/DroughtTimePds)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_twoway_droughttopo)
anova(lme_twoway_droughttopo)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/CH4_nmolm2s_fluxuse_twowayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway_droughttopo,col=arrayGHGdf_noNA$DroughtTimePds, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "ANOVAdiagnostics/CH4_nmolm2s_fluxuse_twowayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_twoway_droughttopo),sqrt(abs(resid(lme_twoway_droughttopo))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "ANOVAdiagnostics/CH4_nmolm2s_fluxuse_twowayANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_twoway_droughttopo,col=arrayGHGdf_noNA$DroughtTimePds, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/CH4_nmolm2s_fluxuse_twowayANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway_droughttopo,DroughtTimePds~resid(.))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/CH4_nmolm2s_fluxuse_twowayANOVAdiagnostics_5.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway_droughttopo,Topo~resid(.))
dev.off()

# save anova info

# post hoc test
posthoc_glht_1 <- summary(glht(lme_twoway_droughttopo, linfct=mcp(DroughtTimePds = "Tukey")), test = adjusted(type = "bonferroni"))
posthoc_glht_2 <- summary(glht(lme_twoway_droughttopo, linfct=mcp(Topo = "Tukey")), test = adjusted(type = "bonferroni"))

# save posthoc test as a nice table later
x <- posthoc_glht_1
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_1 <- mtests

# save posthoc test as a nice table later
x <- posthoc_glht_2
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_2 <- mtests

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_twoway_droughttopo), file=paste(path.expand(pathsavetab), "stats-tables/CH4_twowayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests_1, file=paste(path.expand(pathsavetab), "stats-tables/CH4_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #1", append=TRUE)
write.xlsx(mtests_2, file=paste(path.expand(pathsavetab), "stats-tables/CH4_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #2", append=TRUE)

# set significance stars for graphs
cor_stars <- numeric(length=2)
# cycle through to set number of stars
for (i in 1:2) {
  
  if(anova(lme_twoway_droughttopo)[1+i,4] < 0.001){
    cor_stars[i] <- "***"
  } else if(anova(lme_twoway_droughttopo)[1+i,4] < 0.01){
    cor_stars[i] <- "**"
  } else if(anova(lme_twoway_droughttopo)[1+i,4] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_CH4twoway <- cor_stars


########################################################################
# TWO-WAY REPEATED MEASURES ANOVA OF FLUXES BY DROUGHT PERIOD, TOPO: N2O

## N2O fluxes

# lme can't handle columns with any NAs
arrayGHGdf_noNA <- subset(arrayGHGdf, !is.na(N2O_nmolm2s_fluxuse))

# should log transform based on the diagnostic plots of doing this without transformation

# aov version of the two-way test
aov_twoway_droughttopo = aov(log(abs(N2O_nmolm2s_fluxuse)) ~ DroughtTimePds + Topo + Error(Date/(DroughtTimePds + Topo)), data=arrayGHGdf_noNA)
# lme version of the one-way test
lme_twoway_droughttopo = lme(log(abs(N2O_nmolm2s_fluxuse)) ~ DroughtTimePds + Topo, data=arrayGHGdf_noNA, random = ~1|Date/Topo/DroughtTimePds)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_twoway_droughttopo)
anova(lme_twoway_droughttopo)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/N2O_nmolm2s_fluxuse_twowayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway_droughttopo,col=arrayGHGdf_noNA$DroughtTimePds, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "ANOVAdiagnostics/N2O_nmolm2s_fluxuse_twowayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(lme_twoway_droughttopo),sqrt(abs(resid(lme_twoway_droughttopo))))
dev.off()

# qq plot
png(file = paste(pathsavetab, "ANOVAdiagnostics/N2O_nmolm2s_fluxuse_twowayANOVAdiagnostics_3.png", sep=""),width=6,height=6,units="in",res=150)
qqnorm(lme_twoway_droughttopo,col=arrayGHGdf_noNA$DroughtTimePds, main="QQ Plot", abline=c(0,1))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/N2O_nmolm2s_fluxuse_twowayANOVAdiagnostics_4.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway_droughttopo,DroughtTimePds~resid(.))
dev.off()

# boxplot of residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/N2O_nmolm2s_fluxuse_twowayANOVAdiagnostics_5.png", sep=""),width=6,height=6,units="in",res=150)
plot(lme_twoway_droughttopo,Topo~resid(.))
dev.off()

# save anova info

# post hoc test
posthoc_glht_1 <- summary(glht(lme_twoway_droughttopo, linfct=mcp(DroughtTimePds = "Tukey")), test = adjusted(type = "bonferroni"))
posthoc_glht_2 <- summary(glht(lme_twoway_droughttopo, linfct=mcp(Topo = "Tukey")), test = adjusted(type = "bonferroni"))

# save posthoc test as a nice table later
x <- posthoc_glht_1
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_1 <- mtests

# save posthoc test as a nice table later
x <- posthoc_glht_2
pq<-summary(x)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",                                                                       ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
mtests_2 <- mtests

# save tables as single excel doc
# ignore saving the aov() since the formatting was a mess and we're ignoring it anyways
# lme() anova output
write.xlsx(anova(lme_twoway_droughttopo), file=paste(path.expand(pathsavetab), "stats-tables/N2O_twowayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests_1, file=paste(path.expand(pathsavetab), "stats-tables/N2O_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #1", append=TRUE)
write.xlsx(mtests_2, file=paste(path.expand(pathsavetab), "stats-tables/N2O_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #2", append=TRUE)

# set significance stars for graphs
cor_stars <- numeric(length=2)
# cycle through to set number of stars
for (i in 1:2) {
  
  if(anova(lme_twoway_droughttopo)[1+i,4] < 0.001){
    cor_stars[i] <- "***"
  } else if(anova(lme_twoway_droughttopo)[1+i,4] < 0.01){
    cor_stars[i] <- "**"
  } else if(anova(lme_twoway_droughttopo)[1+i,4] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_N2Otwoway <- cor_stars


########################################################################
# GHG BAR PLOT BY DROUGHT TIME PERIOD

# font size for stats annotations
rhosize=3

# bar graphs; bar for each DroughtTimePds

# CO2
p6a <- ggplot(summarytabDroughtTimePdsA2, aes(x=DroughtTimePds, y=meanCO2_umolm2s_fluxuse)) + geom_bar(stat = "identity") + xlab("Drought Time Period") + ylab("CO2 Flux (umol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanCO2_umolm2s_fluxuse-seCO2_umolm2s_fluxuse, ymax=meanCO2_umolm2s_fluxuse+seCO2_umolm2s_fluxuse), width = 0.65)

# CH4
p6b <- ggplot(summarytabDroughtTimePdsB2, aes(x=DroughtTimePds, y=meanCH4_nmolm2s_fluxuse)) + geom_bar(stat = "identity") + xlab("Drought Time Period") + ylab("CH4 Flux (nmol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanCH4_nmolm2s_fluxuse-seCH4_nmolm2s_fluxuse, ymax=meanCH4_nmolm2s_fluxuse+seCH4_nmolm2s_fluxuse), width = 0.65)

# N2O
p6c <- ggplot(summarytabDroughtTimePdsC2, aes(x=DroughtTimePds, y=meanN2O_nmolm2s_fluxuse)) + geom_bar(stat = "identity") + xlab("Drought Time Period") + ylab("N2O Flux (nmol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanN2O_nmolm2s_fluxuse-seN2O_nmolm2s_fluxuse, ymax=meanN2O_nmolm2s_fluxuse+seN2O_nmolm2s_fluxuse), width = 0.65)

# bar graphs; bar for each DroughtTimePds and each topo location

# CO2
p7a <- ggplot(summarytabDroughtTimePdsA1, aes(x=DroughtTimePds, y=meanCO2_umolm2s_fluxuse, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("CO2 Flux (umol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanCO2_umolm2s_fluxuse-seCO2_umolm2s_fluxuse, ymax=meanCO2_umolm2s_fluxuse+seCO2_umolm2s_fluxuse), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_CO2twoway[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_CO2twoway[2]), size=rhosize)

ptmp<-ggplot(summarytabDroughtTimePdsA1, aes(x=DroughtTimePds, y=meanCO2_umolm2s_fluxuse, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("CO2 Flux (umol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanCO2_umolm2s_fluxuse-seCO2_umolm2s_fluxuse, ymax=meanCO2_umolm2s_fluxuse+seCO2_umolm2s_fluxuse), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_CO2twoway[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_CO2twoway[2]), size=rhosize)

### how do I add the N values to each bar?
ptmp + annotate("text", x = seq(0, 4, length.out = 12), y = 0.5, label = summarytabDroughtTimePdsA1$N[1:12], size=rhosize-1)

summarytabDroughtTimePdsA1








# CH4
p7b <- ggplot(summarytabDroughtTimePdsB1, aes(x=DroughtTimePds, y=meanCH4_nmolm2s_fluxuse, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("CH4 Flux (nmol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanCH4_nmolm2s_fluxuse-seCH4_nmolm2s_fluxuse, ymax=meanCH4_nmolm2s_fluxuse+seCH4_nmolm2s_fluxuse), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_CH4twoway[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_CH4twoway[2]), size=rhosize)

# N2O
p7c <- ggplot(summarytabDroughtTimePdsC1, aes(x=DroughtTimePds, y=meanN2O_nmolm2s_fluxuse, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("N2O Flux (nmol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanN2O_nmolm2s_fluxuse-seN2O_nmolm2s_fluxuse, ymax=meanN2O_nmolm2s_fluxuse+seN2O_nmolm2s_fluxuse), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_N2Otwoway[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_N2Otwoway[2]), size=rhosize)






###### THINGS TO DO

# using the base data:
# chamber might increase by 10-20 ppb in N2O over the 10 minutes, so we should expect good fluxes to have a meanN2Oppb of 320 +/- 30 ppb -> those data are associated with good concentration readings
# does the histogram of the meanN2Oppb drift over time?  indication that something was wonky
# if that wasn't the case, the instrument is probably behaving acceptably, but the negative N2O concentrations were likely event based

# aside on what Scott did: had to correct the N2O line, since that can drift if the analyzer reads a weird concentration for a long time (like way too high or something)

# another option is that the deadbands are a problem from when I analyzed the data - if there was a large spike in the data and the tubing length is seen as too short, then the deadband will get in the wrong place
# should probably just re-do all these measurements
# set it correctly on one chamber that is a really good one, and that should be good across the chambers, time periods, etc.

# also think about cutting everything where the deployment time was only +/- 60 seconds, not several minutes, like I have now
# also you can cut fluxes where the mean CO2 is too large, since that could mean that the chamber was stuck closed




## save figures

# save figures without topo categories
png(file = paste(pathsavefigs, "drought_periods_notopo_CO2.png", sep=""),width=6,height=4,units="in",res=400)
p6a
dev.off()

png(file = paste(pathsavefigs, "drought_periods_notopo_CH4.png", sep=""),width=6,height=4,units="in",res=400)
p6b
dev.off()

png(file = paste(pathsavefigs, "drought_periods_notopo_N2O.png", sep=""),width=6,height=4,units="in",res=400)
p6c
dev.off()

# stacked into one figure
png(file = paste(pathsavefigs, "drought_periods_notopo_allpanels.png", sep=""),width=5,height=12,units="in",res=400)
grid.arrange(p6a, p6b, p6c, nrow = 3, ncol = 1)
dev.off()

# save figures with topo categories
png(file = paste(pathsavefigs, "drought_periods_topo_CO2.png", sep=""),width=7,height=4,units="in",res=400)
p7a
dev.off()

png(file = paste(pathsavefigs, "drought_periods_topo_CH4.png", sep=""),width=7,height=4,units="in",res=400)
p7b
dev.off()

png(file = paste(pathsavefigs, "drought_periods_topo_N2O.png", sep=""),width=7,height=4,units="in",res=400)
p7c
dev.off()

# stacked into one figure
png(file = paste(pathsavefigs, "drought_periods_topo_allpanels.png", sep=""),width=7,height=12,units="in",res=400)
grid.arrange(p7a, p7b, p7c, nrow = 3, ncol = 1)
dev.off()

## save csvs of the drought period summaries

# summary csvs without topo categories
fname = paste(pathsavetab, "drought_periods_summarytable_notopo_CO2.csv" ,sep="")
write.csv(summarytabDroughtTimePdsA2, file = fname, na = "")

fname = paste(pathsavetab, "drought_periods_summarytable_notopo_CH4.csv" ,sep="")
write.csv(summarytabDroughtTimePdsB2, file = fname, na = "")

fname = paste(pathsavetab, "drought_periods_summarytable_notopo_N2O.csv" ,sep="")
write.csv(summarytabDroughtTimePdsC2, file = fname, na = "")

# summary csvs with topo categories
fname = paste(pathsavetab, "drought_periods_summarytable_topo_CO2.csv" ,sep="")
write.csv(summarytabDroughtTimePdsA1, file = fname, na = "")

fname = paste(pathsavetab, "drought_periods_summarytable_topo_CH4.csv" ,sep="")
write.csv(summarytabDroughtTimePdsB1, file = fname, na = "")

fname = paste(pathsavetab, "drought_periods_summarytable_topo_N2O.csv" ,sep="")
write.csv(summarytabDroughtTimePdsC1, file = fname, na = "")


#############################################################################
# MAYES VS SILVER SYSTEMS

# pull out only the dates after which both systems are running
arrayGHGdf_systems <- subset(arrayGHGdf, arrayGHGdf$DateTime2 > "2016-03-01 09:17:00 UTC")

# fluxes over time

# summary stats
# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE CO2
summarytab3atmpsys <- summarySE(data=arrayGHGdf_systems, measurevar="CO2_umolm2s_fluxuse", c("Date", "Chamber", "Topo", "System"), na.rm=TRUE, renameallcols=TRUE)

summarytab3btmpsys <- summarySE(data=arrayGHGdf, measurevar="CO2_umolm2s_fluxuse", c("Date", "Topo", "System"), na.rm=TRUE, renameallcols=TRUE)

# summarySE CH4
summarytab4atmpsys <- summarySE(data=arrayGHGdf, measurevar="CH4_nmolm2s_fluxuse", c("Date", "Chamber", "Topo", "System"), na.rm=TRUE, renameallcols=TRUE)

summarytab4btmpsys <- summarySE(data=arrayGHGdf, measurevar="CH4_nmolm2s_fluxuse", c("Date", "Topo", "System"), na.rm=TRUE, renameallcols=TRUE)

# summarySE N2O
summarytab5atmpsys <- summarySE(data=arrayGHGdf, measurevar="N2O_nmolm2s_fluxuse", c("Date", "Chamber", "Topo", "System"), na.rm=TRUE, renameallcols=TRUE)

summarytab5btmpsys <- summarySE(data=arrayGHGdf, measurevar="N2O_nmolm2s_fluxuse", c("Date", "Topo", "System"), na.rm=TRUE, renameallcols=TRUE)

# CO2 by date (mean and se)
p3sys <- ggplot(summarytab3atmpsys, aes(x=Date, y=meanCO2_umolm2s_fluxuse, color=Topo)) + geom_point() + facet_wrap( ~ System, nrow=2) + geom_errorbar(aes(ymin=meanCO2_umolm2s_fluxuse-seCO2_umolm2s_fluxuse, ymax=meanCO2_umolm2s_fluxuse+seCO2_umolm2s_fluxuse), alpha=0.5) + ylab("CO2 Flux (umol/m^2/s),\n(Mean +/- Standard Error)") + theme_bw() + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25')))

# CH4 by date (mean and se)
p4sys <- ggplot(summarytab4atmpsys, aes(x=Date, y=meanCH4_nmolm2s_fluxuse, color=Topo)) + geom_point() + facet_wrap( ~ System, nrow=2) + geom_errorbar(aes(ymin=meanCH4_nmolm2s_fluxuse-seCH4_nmolm2s_fluxuse, ymax=meanCH4_nmolm2s_fluxuse+seCH4_nmolm2s_fluxuse), alpha=0.5) + ylab("CH4 Flux (nmol/m^2/s),\n(Mean +/- Standard Error)") + theme_bw() + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) + ylim(-10,70) #+ geom_line()+ theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) 

# N2O by date (mean and se)
p5sys <- ggplot(summarytab5atmpsys, aes(x=Date, y=meanN2O_nmolm2s_fluxuse, color=Topo)) + geom_point() + facet_wrap( ~ System, nrow=2) + geom_errorbar(aes(ymin=meanN2O_nmolm2s_fluxuse-seN2O_nmolm2s_fluxuse, ymax=meanN2O_nmolm2s_fluxuse+seN2O_nmolm2s_fluxuse), alpha=0.5) + ylab("N2O Flux (nmol/m^2/s),\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line()

# # save figures
# png(file = paste(pathsavefigs, "time_series_CO2.png", sep=""),width=10,height=7,units="in",res=400)
# p3
# dev.off()
# 
# # save figures
# png(file = paste(pathsavefigs, "time_series_CH4.png", sep=""),width=10,height=7,units="in",res=400)
# p4
# dev.off()
# 
# # save figures
# png(file = paste(pathsavefigs, "time_series_N2O.png", sep=""),width=10,height=7,units="in",res=400)
# p5
# dev.off()








# bar graphs; bar for each DroughtTimePds and each topo location

# CO2
p7a <- ggplot(summarytabDroughtTimePdsA1, aes(x=DroughtTimePds, y=meanCO2_umolm2s_fluxuse, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("CO2 Flux (umol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanCO2_umolm2s_fluxuse-seCO2_umolm2s_fluxuse, ymax=meanCO2_umolm2s_fluxuse+seCO2_umolm2s_fluxuse), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_CO2twoway[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_CO2twoway[2]), size=rhosize)







# CH4
p7b <- ggplot(summarytabDroughtTimePdsB1, aes(x=DroughtTimePds, y=meanCH4_nmolm2s_fluxuse, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("CH4 Flux (nmol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanCH4_nmolm2s_fluxuse-seCH4_nmolm2s_fluxuse, ymax=meanCH4_nmolm2s_fluxuse+seCH4_nmolm2s_fluxuse), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_CH4twoway[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_CH4twoway[2]), size=rhosize)

# N2O
p7c <- ggplot(summarytabDroughtTimePdsC1, aes(x=DroughtTimePds, y=meanN2O_nmolm2s_fluxuse, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("N2O Flux (nmol/m^2/s)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanN2O_nmolm2s_fluxuse-seN2O_nmolm2s_fluxuse, ymax=meanN2O_nmolm2s_fluxuse+seN2O_nmolm2s_fluxuse), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_N2Otwoway[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_N2Otwoway[2]), size=rhosize)






# to do on plots:
# print n over each boxplot
# print a, b, c to indicate post-hoc results of which groups are different

# done
# anova on DroughtTimePds groups
# run stats and print info in corner of plots
# run stats and put asterisks if applicable
# save csvs of the anova results







# 
# #############################################################################
# # FIGURE WITH SIMPLE STATS TO LOOK AT THE LINEAR O2-MOISTURE RELATIONSHIPS
# 
# # what are the pearson's correlation coefficients between soil moisture and O2 overall and for each topolocation?
# # default cor.test method is "pearson"
# 
# # All data
# coralldata <- cor.test(arraysensorsdf$SoilMoisture, arraysensorsdf$O2)
# 
# # All data by topolocation
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==1)
# cor1all <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==2)
# cor2all <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==3)
# cor3all <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==4)
# cor4all <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==5)
# cor5all <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==6)
# cor6all <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==7)
# cor7all <- cor.test(tmp$SoilMoisture, tmp$O2)
# 
# # Pre-drought
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==1 & arraysensorsdf$Drought=="Pre-drought")
# cor1pre <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==2 & arraysensorsdf$Drought=="Pre-drought")
# cor2pre <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==3 & arraysensorsdf$Drought=="Pre-drought")
# cor3pre <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==4 & arraysensorsdf$Drought=="Pre-drought")
# cor4pre <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==5 & arraysensorsdf$Drought=="Pre-drought")
# cor5pre <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==6 & arraysensorsdf$Drought=="Pre-drought")
# cor6pre <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==7 & arraysensorsdf$Drought=="Pre-drought")
# cor7pre <- cor.test(tmp$SoilMoisture, tmp$O2)
# 
# # Post-drought
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==1 & arraysensorsdf$Drought=="Post-drought")
# cor1post <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==2 & arraysensorsdf$Drought=="Post-drought")
# cor2post <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==3 & arraysensorsdf$Drought=="Post-drought")
# cor3post <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==4 & arraysensorsdf$Drought=="Post-drought")
# cor4post <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==5 & arraysensorsdf$Drought=="Post-drought")
# cor5post <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==6 & arraysensorsdf$Drought=="Post-drought")
# cor6post <- cor.test(tmp$SoilMoisture, tmp$O2)
# tmp <- subset(arraysensorsdf, arraysensorsdf$TopoLocation==7 & arraysensorsdf$Drought=="Post-drought")
# cor7post <- cor.test(tmp$SoilMoisture, tmp$O2)
# 
# # get statistical significance stars
# cor_stars_all <- numeric(length=7)
# # cycle through to set number of stars
# for (i in 1:7 ) {
#       
#       corpval <- paste("cor",i,"all$p.value",sep="")
#       
#       if(eval(parse(text=corpval)) < 0.001){
#             cor_stars_all[i] <- "***"
#       } else if(eval(parse(text=corpval)) < 0.01){
#             cor_stars_all[i] <- "**"
#       } else if(eval(parse(text=corpval)) < 0.05){
#             cor_stars_all[i] <- "*"
#       } else {
#             cor_stars_all[i] <- " "
#       }
#       
# }
# 
# cor_stars_pre <- numeric(length=7)
# # cycle through to set number of stars
# for (i in 1:7 ) {
#       
#       corpval <- paste("cor",i,"pre$p.value",sep="")
#       
#       if(eval(parse(text=corpval)) < 0.001){
#             cor_stars_pre[i] <- "***"
#       } else if(eval(parse(text=corpval)) < 0.01){
#             cor_stars_pre[i] <- "**"
#       } else if(eval(parse(text=corpval)) < 0.05){
#             cor_stars_pre[i] <- "*"
#       } else {
#             cor_stars_pre[i] <- " "
#       }
#       
# }
# 
# cor_stars_post <- numeric(length=7)
# # cycle through to set number of stars
# for (i in 1:7 ) {
#       
#       corpval <- paste("cor",i,"post$p.value",sep="")
#       
#       if(eval(parse(text=corpval)) < 0.001){
#             cor_stars_post[i] <- "***"
#       } else if(eval(parse(text=corpval)) < 0.01){
#             cor_stars_post[i] <- "**"
#       } else if(eval(parse(text=corpval)) < 0.05){
#             cor_stars_post[i] <- "*"
#       } else {
#             cor_stars_post[i] <- " "
#       }
#       
# }
# 
# # make lists so these will print correctly on the ggplots
# l1all <- list(cor = round(cor1all$estimate,4), star = cor_stars_all[1])
# l1pre <- list(cor = round(cor1pre$estimate,4), star = cor_stars_pre[1])
# l1post <- list(cor = round(cor1post$estimate,4), star = cor_stars_post[1])
# 
# l2all <- list(cor = round(cor1all$estimate,4), star = cor_stars_all[2])
# l2pre <- list(cor = round(cor2pre$estimate,4), star = cor_stars_pre[2])
# l2post <- list(cor = round(cor2post$estimate,4), star = cor_stars_post[2])
# 
# l3all <- list(cor = round(cor1all$estimate,4), star = cor_stars_all[3])
# l3pre <- list(cor = round(cor3pre$estimate,4), star = cor_stars_pre[3])
# l3post <- list(cor = round(cor3post$estimate,4), star = cor_stars_post[3])
# 
# l4all <- list(cor = round(cor1all$estimate,4), star = cor_stars_all[4])
# l4pre <- list(cor = round(cor4pre$estimate,4), star = cor_stars_pre[4])
# l4post <- list(cor = round(cor4post$estimate,4), star = cor_stars_post[4])
# 
# l5all <- list(cor = round(cor1all$estimate,4), star = cor_stars_all[5])
# l5pre <- list(cor = round(cor5pre$estimate,4), star = cor_stars_pre[5])
# l5post <- list(cor = round(cor5post$estimate,4), star = cor_stars_post[5])
# 
# l6all <- list(cor = round(cor1all$estimate,4), star = cor_stars_all[6])
# l6pre <- list(cor = round(cor6pre$estimate,4), star = cor_stars_pre[6])
# l6post <- list(cor = round(cor6post$estimate,4), star = cor_stars_post[6])
# 
# l7all <- list(cor = round(cor1all$estimate,4), star = cor_stars_all[7])
# l7pre <- list(cor = round(cor7pre$estimate,4), star = cor_stars_pre[7])
# l7post <- list(cor = round(cor7post$estimate,4), star = cor_stars_post[7])
# 
# # do the plot with all the data
# 
# 
# 
# 
# 
# # do the pre-drought plot
# tmp <- subset(arraysensorsdf, arraysensorsdf$Drought=="Pre-drought")
# eq_1 <- substitute(italic(r)[1] == cor~star,l1pre); eqstr_1 <- as.character(as.expression(eq_1))
# eq_2 <- substitute(italic(r)[2] == cor~star,l2pre); eqstr_2 <- as.character(as.expression(eq_2))
# eq_3 <- substitute(italic(r)[3] == cor~star,l3pre); eqstr_3 <- as.character(as.expression(eq_3))
# eq_4 <- substitute(italic(r)[4] == cor~star,l4pre); eqstr_4 <- as.character(as.expression(eq_4))
# eq_5 <- substitute(italic(r)[5] == cor~star,l5pre); eqstr_5 <- as.character(as.expression(eq_5))
# eq_6 <- substitute(italic(r)[6] == cor~star,l6pre); eqstr_6 <- as.character(as.expression(eq_6))
# eq_7 <- substitute(italic(r)[7] == cor~star,l7pre); eqstr_7 <- as.character(as.expression(eq_7))
# 
# # font size
# rhosize <- 4
# 
# # clarify pearson's
# eq2 <- substitute(italic(Below):~~italic(Pearson)~~italic(corr.)~~italic(coefficients))
# eqstr2 <- as.character(as.expression(eq2))
# 
# # base plot, no corr text
# p <- ggplot(tmp, aes(x=SoilMoisture, y=O2, color=TopoLocation)) + geom_point(shape=1, alpha=0.5) + xlab("Soil Moisture (Fraction)") + ylab("Soil O2 (Fraction)") + theme_bw() + theme(legend.position="none") + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.06, vjust=-16.6, label = eqstr, parse = TRUE, size = rhosize) + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.055, vjust=-14.8, label = eqstr2, parse = TRUE, size = rhosize) + geom_smooth(size = 1, method="lm") + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.055, vjust=1.7, label = "Pre-drought", parse = TRUE, size = rhosize+2) + geom_smooth(size = 1, method="lm")
# 
# # add the correlation text
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.12, vjust=-10, label = eqstr_1, parse = TRUE, size=rhosize) 
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.12, vjust=-8.5, label = eqstr_2, parse = TRUE, size=rhosize)
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.12, vjust=-7, label = eqstr_3, parse = TRUE, size=rhosize) 
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.12, vjust=-5.5, label = eqstr_4, parse = TRUE, size=rhosize)
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.12, vjust=-4, label = eqstr_5, parse = TRUE, size=rhosize) 
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.12, vjust=-2.5, label = eqstr_6, parse = TRUE, size=rhosize)
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.12, vjust=-1, label = eqstr_7, parse = TRUE, size=rhosize) 
# 
# 
# 
# 
# 
# # note that pearson's r reflects the non-linearity and direction of a linear relationship, but not the slope of that relationship (https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient)
# 
# # lm_eqn
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/lm_eqn.r")
# # p1 = p + annotate(geom="text", x = 25, y = 300, label = lm_eqn(lm(y ~ x, df)), parse = TRUE)
# 
# # lm(y~x)
# m <- lm(tmp$O2[tmp$TopoLocation==7]~tmp$SoilMoisture[tmp$TopoLocation==7], tmp)
# # upgraded figure
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.62, vjust=-1, label = lm_eqn(m), parse = TRUE, size=rhosize)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # so we're also going to put the linear regression slope on the graph
# p <- ggplot(tmp, aes(x=SoilMoisture, y=O2, color=TopoLocation)) + geom_point(shape=1, alpha=0.5) + xlab("Soil Moisture (Fraction)") + ylab("Soil O2 (Fraction)") + theme_bw() + theme(legend.position="none") + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.06, vjust=-14.8, label = eqstr, parse = TRUE, size = rhosize) + geom_smooth(size = 1, method="lm")
# 
# 
# 
# 
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-1.2, label = eqstr_1, parse = TRUE, size=rhosize) + geom_smooth(size = 1, method="lm")
# 
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-1.2, label = eqstr_1, parse = TRUE, size=rhosize) + geom_smooth(size = 1, method="lm")
# 
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-1.2, label = eqstr_1, parse = TRUE, size=rhosize) + geom_smooth(size = 1, method="lm")
# 
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-1.2, label = eqstr_1, parse = TRUE, size=rhosize) + geom_smooth(size = 1, method="lm")
# 
# p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-1.2, label = eqstr_1, parse = TRUE, size=rhosize) + geom_smooth(size = 1, method="lm")
# 
# 
# 
# 
# p5labelsfull <- p5labels + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-1.2, label = eqstr_1, parse = TRUE, size=rhosize) + geom_smooth(size = 1, method="lm")
# 
# 
# p5 <- p3 + geom_smooth(size = 1, method="lm") + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3)
# 
# 
# 
# 
# plot1 <- ggplot(dat, aes(x=wC, y=-EmittedC, color=SimulationFactor)) + geom_point(shape=1) + geom_smooth(size=0.75) + theme_bw() + ylab("Tg Carbon Emitted") + xlab("Carbon Storage Priority Level") + theme(legend.position="none") + ylim(1.25e10,3.53e10) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.2, colour = factorcolors[1], label = eqstr_a, parse = TRUE, size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.4, colour = factorcolors[2], label = eqstr_b, parse = TRUE, size=rhosize) + scale_colour_manual(values = factorcolors)
# 
# 
# eq_b <- substitute(ρ[prot] == cor~star,l1b); eqstr_b <- as.character(as.expression(eq_b))
# plot1 <- ggplot(dat, aes(x=wC, y=-EmittedC, color=SimulationFactor)) + geom_point(shape=1) + geom_smooth(size=0.75) + theme_bw() + ylab("Tg Carbon Emitted") + xlab("Carbon Storage Priority Level") + theme(legend.position="none") + ylim(1.25e10,3.53e10) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.2, colour = factorcolors[1], label = eqstr_a, parse = TRUE, size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.4, colour = factorcolors[2], label = eqstr_b, parse = TRUE, size=rhosize) + scale_colour_manual(values = factorcolors)
# 
# 
# 
# # do the post-drought plot
# 
# 
# 
# # save via grid.arrange
# # must save this way, because ggsave only works with ggplot items and it doesn't understand that gB, etc. are based on ggplot items (since they are currently grobs, the units of grid.arrange)
# png(file = "AmazonTOs_simulationscatters.png",width=10,height=10,units="in",res=300) # tiff(file=...) also an option
# grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=3)
# dev.off()
# 
# 
# 
# 
# # now begin the plots
# 
# eq_a <- substitute(ρ[all] == cor~star,l1a); eqstr_a <- as.character(as.expression(eq_a))
# eq_b <- substitute(ρ[prot] == cor~star,l1b); eqstr_b <- as.character(as.expression(eq_b))
# plot1 <- ggplot(dat, aes(x=wC, y=-EmittedC, color=SimulationFactor)) + geom_point(shape=1) + geom_smooth(size=0.75) + theme_bw() + ylab("Tg Carbon Emitted") + xlab("Carbon Storage Priority Level") + theme(legend.position="none") + ylim(1.25e10,3.53e10) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.2, colour = factorcolors[1], label = eqstr_a, parse = TRUE, size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.4, colour = factorcolors[2], label = eqstr_b, parse = TRUE, size=rhosize) + scale_colour_manual(values = factorcolors)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # put these onto the figure
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # linear fit figure
# eq <- substitute(italic(Trend)~~italic(lines):~~italic(linear)~~italic(fit))
# eqstr <- as.character(as.expression(eq))
# p5 <- p3 + geom_smooth(size = 1, method="lm") + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ########################################################################
# # NOTES, TO DO AND TESTING
# 
# 
# 
# 


# # cumulative rainfall graphs (individually saved not overlayed)

# p0b <- ggplot(rainfallstudypd_365) + geom_line(aes(x = Date, y = acc_sum)) + ylab("El Verde Cumulative Rainfall \n(mm)") + theme_bw()
# 
# # save figure
# png(file = paste(pathsavefigs, "cumulativerain_rainfallstudypd_365.png", sep=""),width=5,height=5,units="in",res=150)
# p0b
# dev.off()
# cumulative rainfall (reference period)
# p0d <- ggplot(summarytabraincomp, aes(x=MonthDay2, y=acc_sum)) + geom_ribbon(aes(ymin=acc_sum-sdRainfall_mm, ymax=acc_sum+sdRainfall_mm), alpha=0.5) + geom_line() + ylab("El Verde Cumulative Rainfall (mm) \n2004-2013 Mean") + theme_bw() + scale_x_datetime(labels = date_format("%b-%d"), name="")
# 
# # save figure
# png(file = paste(pathsavefigs, "cumulativerain_rainfallstudypd_reference.png", sep=""),width=5,height=5,units="in",res=150)
# p0d
# dev.off()
# 
# # save a version with a large error band to explain to whendee
# p0e <- ggplot(summarytabraincomp, aes(x=MonthDay2, y=acc_sum)) + geom_ribbon(aes(ymin=acc_sum-700, ymax=acc_sum+700), alpha=0.5) + geom_line() + ylab("El Verde Cumulative Rainfall (mm) \n2004-2013 Mean") + theme_bw() + scale_x_datetime(labels = date_format("%b-%d"), name="")
# 
# png(file = paste(pathsavefigs, "cumulativerain_rainfallstudypd_reference_errortoobig.png", sep=""),width=5,height=5,units="in",res=150)
# p0e
# dev.off()




# ########################################################################
# # SUMMARY STATS: CO2, CH4, N2O ACROSS TRANSECTS AT EACH DATE
# 
# # summarySE using plyr
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
# 
# # summarySE CO2
# summarytab3atmp <- summarySE(data=arrayGHGdf, measurevar="CO2_umolm2s_fluxuse", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)
# 
# summarytab3btmp <- summarySE(data=arrayGHGdf, measurevar="CO2_umolm2s_fluxuse", c("Date", "Topo"), na.rm=TRUE, renameallcols=TRUE)
# 
# # summarySE CH4
# summarytab4atmp <- summarySE(data=arrayGHGdf, measurevar="CH4_nmolm2s_fluxuse", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)
# 
# summarytab4btmp <- summarySE(data=arrayGHGdf, measurevar="CH4_nmolm2s_fluxuse", c("Date", "Topo"), na.rm=TRUE, renameallcols=TRUE)
# 
# # summarySE N2O
# summarytab5atmp <- summarySE(data=arrayGHGdf, measurevar="N2O_nmolm2s_fluxuse", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)
# 
# summarytab5btmp <- summarySE(data=arrayGHGdf, measurevar="N2O_nmolm2s_fluxuse", c("Date", "Topo"), na.rm=TRUE, renameallcols=TRUE)
# 
# # convert summary table dates
# #summarytab3a$Date <- ymd_hms(summarytab3atmp$Date)
# #summarytab2$Date <- ymd_hms(summarytab2$Date)
# 
# # take out weird NA lines
# #summarytab1 <- subset(summarytab1tmp,summarytab1tmp$N>0.5)
# #summarytab2 <- subset(summarytab2tmp,summarytab2tmp$N>0.5)
# 
# 
# ########################################################################
# # CO2, CH4, N2O TIME SERIES
# 
# #topocolorsGHG <- c("blue2","darkmagenta","firebrick2")
# #topocolorsGHG <- rev(colorRampPalette(c('red','blue'), space = "Lab")(3))
# topocolorsGHG <- rev(colorRampPalette(c('red','orange','purple','blue','dark blue'), space = "Lab")(3))
# 
# topobreaks <- c("1","2","3","4","5","6","7")
# topolabs <- c("1 (Ridge)","2","3","4","5","6","7 (Valley)")
# 
# # CO2 by date (mean and se)
# p3 <- ggplot(summarytab3atmp, aes(x=Date, y=meanCO2_umolm2s_fluxuse, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanCO2_umolm2s_fluxuse-seCO2_umolm2s_fluxuse, ymax=meanCO2_umolm2s_fluxuse+seCO2_umolm2s_fluxuse), alpha=0.5) + ylab("CO2 Flux (umol/m^2/s), Exp. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line()
# 
# # CH4 by date (mean and se)
# p4 <- ggplot(summarytab4atmp, aes(x=Date, y=meanFluxCH4_E_nmolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxCH4_E_nmolm2s-seFluxCH4_E_nmolm2s, ymax=meanFluxCH4_E_nmolm2s+seFluxCH4_E_nmolm2s), alpha=0.5) + ylab("CH4 Flux (nmol/m^2/s), Exp. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line()
# 
# # N2O by date (mean and se)
# p5 <- ggplot(summarytab5atmp, aes(x=Date, y=meanFluxN2O_E_nmolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxN2O_E_nmolm2s-seFluxN2O_E_nmolm2s, ymax=meanFluxN2O_E_nmolm2s+seFluxN2O_E_nmolm2s), alpha=0.5) + ylab("N2O Flux (nmol/m^2/s), Exp. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line()
# 
# # save figures
# png(file = paste(pathsavefigs, "time_series_CO2.png", sep=""),width=10,height=7,units="in",res=400)
# p3
# dev.off()
# 
# # save figures
# png(file = paste(pathsavefigs, "time_series_CH4.png", sep=""),width=10,height=7,units="in",res=400)
# p4
# dev.off()
# 
# # save figures
# png(file = paste(pathsavefigs, "time_series_N2O.png", sep=""),width=10,height=7,units="in",res=400)
# p5
# dev.off()
# 
# # save figures
# png(file = paste(pathsavefigs, "time_series_allpanels.png", sep=""),width=10,height=25,units="in",res=400)
# grid.arrange(p0, p1, p2, p3, p4, p5, nrow = 6, ncol = 1)
# dev.off()
# 
# 
# # CO2 by date (mean and se)
# p3b <- ggplot(summarytab3ctmp, aes(x=Date, y=meanFluxCO2_L_umolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxCO2_L_umolm2s-seFluxCO2_L_umolm2s, ymax=meanFluxCO2_L_umolm2s+seFluxCO2_L_umolm2s), alpha=0.5) + ylab("CO2 Flux (umol/m^2/s), Lin. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line()
# 
# # CH4 by date (mean and se)
# p4b <- ggplot(summarytab4ctmp, aes(x=Date, y=meanFluxCH4_L_nmolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxCH4_L_nmolm2s-seFluxCH4_L_nmolm2s, ymax=meanFluxCH4_L_nmolm2s+seFluxCH4_L_nmolm2s), alpha=0.5) + ylab("CH4 Flux (nmol/m^2/s), Lin. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line()
# 
# # N2O by date (mean and se)
# p5b <- ggplot(summarytab5ctmp, aes(x=Date, y=meanFluxN2O_L_nmolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxN2O_L_nmolm2s-seFluxN2O_L_nmolm2s, ymax=meanFluxN2O_L_nmolm2s+seFluxN2O_L_nmolm2s), alpha=0.5) + ylab("N2O Flux (nmol/m^2/s), Lin. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ geom_line()
# 






# ########################################################################
# # FUNCTION FOR SCATTERPLOT GGPLOTS
# 
# # applies to all the graphs
# factorcolors <- c("#00BA38","#619CFF","#F8766D") # c("#1f78b4","#33a02c") # c("#619CFF","#F8766D")
# rhosize <- 4
# 
# # function for correlation df for reporting
# corfun<-function(x, y) {corr=(cor.test(x, y))}
# 
# # function for printing the nice labels
# cococodepath = "~/Documents/GITHUB/RPersonalFunctionsChristine/"
# source(paste(cococodepath, "cor_stars_info.R", sep=""))
# 
# oconnelldiss_tanuro_scatterplots <- function(corrdf,colnames,xlab,ylab,corinfo) {
#   
#   plotdf <- corrdf[,colnames]
#   
#   p <- ggplot(plotdf, aes(x=plotdf[,3], y=plotdf[,2], color=plotdf[,1]), environment = environment()) 
#   p <- p + geom_point(shape=1) 
#   p <- p + scale_colour_manual(values = factorcolors) 
#   p <- p + geom_smooth(size=0.75, method = "lm", se=FALSE) 
#   p <- p + geom_smooth(size=0.75, method = "lm", se=FALSE, fullrange=TRUE, lty = 2) 
#   p <- p + theme_bw() 
#   p <- p + ylab(ylabel) 
#   p <- p + xlab(xlabel) 
#   p <- p + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.2, label = corinfo$infotmp[1], parse = T) 
#   p <- p + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.4, label = corinfo$infotmp[2], parse = T) 
#   p + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=3.6, label = corinfo$infotmp[3], parse = T) + theme(legend.position="none")
#   
# }
# 
# oconnelldiss_tanuro_scatterplots_legendbottom <- function(corrdf,colnames,xlab,ylab,corinfo) {
#   
#   plotdf <- corrdf[,colnames]
#   
#   p <- ggplot(plotdf, aes(x=plotdf[,3], y=plotdf[,2], color=plotdf[,1]), environment = environment()) 
#   p <- p + geom_point(shape=1) 
#   p <- p + scale_colour_manual(values = factorcolors) 
#   p <- p + geom_smooth(size=0.75, method = "lm", se=FALSE) 
#   p <- p + geom_smooth(size=0.75, method = "lm", se=FALSE, fullrange=TRUE, lty = 2) 
#   p <- p + theme_bw() 
#   p <- p + ylab(ylabel) 
#   p <- p + xlab(xlabel) 
#   p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.05, vjust=-3.6, label = corinfo$infotmp[1], parse = T) 
#   p <- p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.05, vjust=-2.4, label = corinfo$infotmp[2], parse = T) 
#   p + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.05, vjust=-1.2, label = corinfo$infotmp[3], parse = T) + theme(legend.position="none")
#   
# }
# 
# 
# ########################################################################
# # BUILD SCATTERPLOTS
# 
# ## N2O
# ylabel <- "Flux N2O: ngN / cm^2 / h"
# 
# colnames <- c("LUtype","LinearFluxN2O","SoilMoisPercent")
# xlabel <- "% Soil Moisture"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(SoilMoisPercent,LinearFluxN2O)$estimate,
#                  pval=corfun(SoilMoisPercent,LinearFluxN2O)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(SoilMoisPercent,LinearFluxN2O,unique(as.character(LUtype))))
# p1n <- oconnelldiss_tanuro_scatterplots(corrdf=corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# colnames <- c("LUtype","LinearFluxN2O","NO3_N_mgNg")
# xlabel <- "NO3-N mgN/g"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_mgNg,LinearFluxN2O)$estimate,
#                  pval=corfun(NO3_N_mgNg,LinearFluxN2O)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NO3_N_mgNg,LinearFluxN2O,unique(as.character(LUtype))))
# p2n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# colnames <- c("LUtype","LinearFluxN2O","NH4_N_mgNg")
# xlabel <- "NH4-N mgN/g"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_N_mgNg,LinearFluxN2O)$estimate,
#                  pval=corfun(NH4_N_mgNg,LinearFluxN2O)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NH4_N_mgNg,LinearFluxN2O,unique(as.character(LUtype))))
# p3n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# colnames <- c("LUtype","LinearFluxN2O","NO3_N_NH4_N_mgNg")
# xlabel <- "NO3-N + NH4-N mgN/g"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_NH4_N_mgNg,LinearFluxN2O)$estimate,
#                  pval=corfun(NO3_N_NH4_N_mgNg,LinearFluxN2O)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NO3_N_NH4_N_mgNg,LinearFluxN2O,unique(as.character(LUtype))))
# p4n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# 
# colnames <- c("LUtype","LinearFluxN2O","NO3_netnitr_perDay_AreaBasis")
# xlabel <- "Net nitrificationrate (NO3), area basis, mg N m-2 day-1"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxN2O)$estimate,
#                  pval=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxN2O)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NO3_netnitr_perDay_AreaBasis,LinearFluxN2O,unique(as.character(LUtype))))
# p5n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# 
# colnames <- c("LUtype","LinearFluxN2O","NH4_netamm_FinalMinusInitial_perDay_AreaBasis")
# xlabel <- "Net ammonificationrate (NH4), area basis, mg N m-2 day-1"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O)$estimate,
#                  pval=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O,unique(as.character(LUtype))))
# p6n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# 
# colnames <- c("LUtype","LinearFluxN2O","NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis")
# xlabel <- "Net mineralizationrate area basis (NO3 + NH4), mg N m-2 day-1"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O)$estimate,
#                  pval=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxN2O,unique(as.character(LUtype))))
# p7n <- oconnelldiss_tanuro_scatterplots(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# 
# 
# ## CO2
# ylabel <- "Flux CO2: ugC / cm^2 / h"
# 
# colnames <- c("LUtype","LinearFluxCO2","SoilMoisPercent")
# xlabel <- "% Soil Moisture"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(SoilMoisPercent,LinearFluxCO2)$estimate,
#                  pval=corfun(SoilMoisPercent,LinearFluxCO2)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(SoilMoisPercent,LinearFluxCO2,unique(as.character(LUtype))))
# p1co <- oconnelldiss_tanuro_scatterplots(corrdf=corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# colnames <- c("LUtype","LinearFluxCO2","NO3_N_mgNg")
# xlabel <- "NO3-N mgN/g"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_mgNg,LinearFluxCO2)$estimate,
#                  pval=corfun(NO3_N_mgNg,LinearFluxCO2)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NO3_N_mgNg,LinearFluxCO2,unique(as.character(LUtype))))
# p2co <- oconnelldiss_tanuro_scatterplots_legendbottom(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# colnames <- c("LUtype","LinearFluxCO2","NH4_N_mgNg")
# xlabel <- "NH4-N mgN/g"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_N_mgNg,LinearFluxCO2)$estimate,
#                  pval=corfun(NH4_N_mgNg,LinearFluxCO2)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NH4_N_mgNg,LinearFluxCO2,unique(as.character(LUtype))))
# p3co <- oconnelldiss_tanuro_scatterplots_legendbottom(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# colnames <- c("LUtype","LinearFluxCO2","NO3_N_NH4_N_mgNg")
# xlabel <- "NO3-N + NH4-N mgN/g"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_N_NH4_N_mgNg,LinearFluxCO2)$estimate,
#                  pval=corfun(NO3_N_NH4_N_mgNg,LinearFluxCO2)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NO3_N_NH4_N_mgNg,LinearFluxCO2,unique(as.character(LUtype))))
# p4co <- oconnelldiss_tanuro_scatterplots_legendbottom(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# 
# colnames <- c("LUtype","LinearFluxCO2","NO3_netnitr_perDay_AreaBasis")
# xlabel <- "Net nitrificationrate (NO3), area basis, mg N m-2 day-1"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxCO2)$estimate,
#                  pval=corfun(NO3_netnitr_perDay_AreaBasis,LinearFluxCO2)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NO3_netnitr_perDay_AreaBasis,LinearFluxCO2,unique(as.character(LUtype))))
# p5co <- oconnelldiss_tanuro_scatterplots_legendbottom(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# 
# colnames <- c("LUtype","LinearFluxCO2","NH4_netamm_FinalMinusInitial_perDay_AreaBasis")
# xlabel <- "Net ammonificationrate (NH4), area basis, mg N m-2 day-1"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2)$estimate,
#                  pval=corfun(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NH4_netamm_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2,unique(as.character(LUtype))))
# p6co <- oconnelldiss_tanuro_scatterplots_legendbottom(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# 
# colnames <- c("LUtype","LinearFluxCO2","NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis")
# xlabel <- "Net mineralizationrate area basis (NO3 + NH4), mg N m-2 day-1"
# corinfo <- ddply(corrdf, .(LUtype), summarise, est=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2)$estimate,
#                  pval=corfun(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2)$p.value,
#                  n=length(LUtype), infotmp=cor_stars_info(NO3_NH4_netmin_FinalMinusInitial_perDay_AreaBasis,LinearFluxCO2,unique(as.character(LUtype))))
# p7co <- oconnelldiss_tanuro_scatterplots_legendbottom(corrdf,colnames=colnames,xlab=xlabel,ylab=ylabel,corinfo=corinfo)
# 
# 




