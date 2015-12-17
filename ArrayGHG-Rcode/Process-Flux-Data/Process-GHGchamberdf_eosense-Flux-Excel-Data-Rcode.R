# Process-GHGchamberdf_eosense-Flux-Excel-Data-Rcode.R
# 
# processing the csv files that are produced by the Eosense chamber analysis software
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# output products:


########################################################################
# GET READY TO BRING IN DATA

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(data.table)
library(chron)
library(lubridate)
library(lattice)
library(reshape2)
#options(java.parameters = "-Xmx5000m") # make sure there's sufficient memory to open excel file
#library(xlsx)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/FluxFigures/"


########################################################################
# BRING IN DATA, MAKE DATAFRAME

# only do this if you haven't already made a csv
alreadybuiltcsv <- "y"

if(alreadybuiltcsv=="n") {
      
      # then build the csv from the excel file from Leilei
      print("building csv from the csv file that gets built by the eosense software") 
      
      # where excel file is
      pathfile = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Chamber-data/Processed data from others/From Chance/"
      
      # bring in excel data
      data2 <- read.csv(paste(pathfile,"all_chamber_data.csv",sep=""), stringsAsFactors=FALSE)
      
      # rename cols
      newnames <- c("DateTime","ChamberNumber","MeasurementDuration_s",
                    "MeanCO2_ppm","MeanCH4_ppm","MeanN2O_ppm","MeanNH3_ppm",
                    "MeanH2O_percent","ChemDetect_0to1","CavPressure_kPa","CavTemperature_K",
                    "WaterContent_fraction","ChamberTemperature_K","ChamberPressure_kPa",
                    "FluxCO2L_umol_per_m2_per_s","FluxCO2E_umol_per_m2_per_s","FluxCH4L_nmol_per_m2_per_s",
                    "FluxCH4E_nmol_per_m2_per_s","FluxN2OL_nmol_per_m2_per_s",
                    "FluxN2OE_nmol_per_m2_per_s","FluxNH3L_umol_per_m2_per_s",
                    "FluxNH3E_umol_per_m2_per_s","e_FluxCO2L_umol_per_m2_per_s",
                    "e_FluxCO2E_umol_per_m2_per_s","e_FluxCH4L_nmol_per_m2_per_s",
                    "e_FluxCH4E_nmol_per_m2_per_s","e_FluxN2OL_nmol_per_m2_per_s",
                    "e_FluxN2OE_nmol_per_m2_per_s","e_FluxNH3L_umol_per_m2_per_s",
                    "e_FluxNH3E_umol_per_m2_per_s")
      names(data2) <- newnames
      
      # fix dates
      data2$DateTime2 <- ymd_hms(data2$DateTime)
      
      # add useful post- and pre-drought column
      data2$Drought <- -9999
      droughtstartdate <- mdy("4/18/2015")
      data2$Drought[data2$DateTime2 <= droughtstartdate] <- "Pre-drought"
      data2$Drought[data2$DateTime2 > droughtstartdate] <- "Post-drought"
      
      # add TopoLocation column
      # clarify which chambers are on which part of the slope
      # Chamber 1,4 and 7 stay on the ridge, 2, 5 and 8 are on the slope, while 3, 6 and 9 are on the valley
      data2$TopoLocation <- -9999
      data2$TopoLocation[data2$ChamberNumber == 1 | data2$ChamberNumber == 4 | data2$ChamberNumber == 7] <- "Ridge"
      data2$TopoLocation[data2$ChamberNumber == 2 | data2$ChamberNumber == 5 | data2$ChamberNumber == 8] <- "Slope"
      data2$TopoLocation[data2$ChamberNumber == 3 | data2$ChamberNumber == 6 | data2$ChamberNumber == 9] <- "Valley"
      
      # save as csv
      GHGchamberdf_eosense_precleaned <- data2
      write.csv(GHGchamberdf_eosense_precleaned, file=paste(pathsavefiles, "GHGchamberdf_eosense_precleaned.csv", sep = ""), row.names=FALSE)  
      
} else {
      
      # if the csv has been built previously, then just load it
      print("csv built previously; loading it now")
      
      # load csv
      GHGchamberdf_eosense_precleaned <- read.csv(paste(pathsavefiles, "GHGchamberdf_eosense_precleaned.csv", sep = ""), stringsAsFactors=FALSE)
      
      # fix dates
      GHGchamberdf_eosense_precleaned$DateTime2 <- ymd_hms(GHGchamberdf_eosense_precleaned$DateTime)
      
}


# make factors where needed
GHGchamberdf_eosense_precleaned$Drought <- as.factor(GHGchamberdf_eosense_precleaned$Drought)
GHGchamberdf_eosense_precleaned$ChamberNumber <- as.factor(GHGchamberdf_eosense_precleaned$ChamberNumber)
GHGchamberdf_eosense_precleaned$TopoLocation <- as.factor(GHGchamberdf_eosense_precleaned$TopoLocation)

# check dates
str(GHGchamberdf_eosense_precleaned$DateTime2)


########################################################################
# ELIMINATE UNACCEPTABLE DATA

# how many chambers have negative CO2 values?

# E estimate is negative
tmp <- GHGchamberdf_eosense_precleaned$FluxCO2E_umol_per_m2_per_s[GHGchamberdf_eosense_precleaned$FluxCO2E_umol_per_m2_per_s < 0]
# E extimate is negative and L estimate is negative
tmp2 <- GHGchamberdf_eosense_precleaned$FluxCO2E_umol_per_m2_per_s[GHGchamberdf_eosense_precleaned$FluxCO2E_umol_per_m2_per_s < 0 & GHGchamberdf_eosense_precleaned$FluxCO2L_umol_per_m2_per_s < 0]
# only L estimate is negative
tmp3 <- GHGchamberdf_eosense_precleaned$FluxCO2E_umol_per_m2_per_s[GHGchamberdf_eosense_precleaned$FluxCO2L_umol_per_m2_per_s < 0]

# check to see if these line up
length(tmp)
length(tmp2)
length(tmp3)
# length(tmp) = 53, others are 52, so not too far off.  Let's err on the E side and wipe all of the neg E estimate fluxes for CO2

# any chamber where CO2 flux is negative for the E estimate, throw out all the points from that chamber
badCO2truefalse <- GHGchamberdf_eosense_precleaned$FluxCO2E_umol_per_m2_per_s < 0
badCO2rows <- which(badCO2truefalse, arr.ind = TRUE)

# what columns to set to NA?
badCO2cols <- match(c("FluxCO2L_umol_per_m2_per_s","FluxCO2E_umol_per_m2_per_s","FluxCH4L_nmol_per_m2_per_s",
                      "FluxCH4E_nmol_per_m2_per_s","FluxN2OL_nmol_per_m2_per_s",
                      "FluxN2OE_nmol_per_m2_per_s","FluxNH3L_umol_per_m2_per_s",
                      "FluxNH3E_umol_per_m2_per_s","e_FluxCO2L_umol_per_m2_per_s",
                      "e_FluxCO2E_umol_per_m2_per_s","e_FluxCH4L_nmol_per_m2_per_s",
                      "e_FluxCH4E_nmol_per_m2_per_s","e_FluxN2OL_nmol_per_m2_per_s",
                      "e_FluxN2OE_nmol_per_m2_per_s","e_FluxNH3L_umol_per_m2_per_s",
                      "e_FluxNH3E_umol_per_m2_per_s"),names(GHGchamberdf_eosense_precleaned))
# set badCO2 rows to NA in badCO2cols
GHGchamberdf_eosense_precleaned[badCO2rows,badCO2cols] <- NA 

# reorder cols so they're easier to read when opening in excel
GHGchamberdf_eosense_precleaned2 <- GHGchamberdf_eosense_precleaned[c(1:2,33,3:32)]

# save error per flux ratio columns

# ratio of CO2 flux to error
GHGchamberdf_eosense_precleaned2$CO2Eerrorperflux <- abs(GHGchamberdf_eosense_precleaned2$e_FluxCO2E_umol_per_m2_per_s/GHGchamberdf_eosense_precleaned2$FluxCO2E_umol_per_m2_per_s)

GHGchamberdf_eosense_precleaned2$CO2Lerrorperflux <- abs(GHGchamberdf_eosense_precleaned2$e_FluxCO2L_umol_per_m2_per_s/GHGchamberdf_eosense_precleaned2$FluxCO2L_umol_per_m2_per_s)

# ratio of N2O flux to error
GHGchamberdf_eosense_precleaned2$N2OEerrorperflux <- abs(GHGchamberdf_eosense_precleaned2$e_FluxN2OE_nmol_per_m2_per_s/GHGchamberdf_eosense_precleaned2$FluxN2OE_nmol_per_m2_per_s)

GHGchamberdf_eosense_precleaned2$N2OLerrorperflux <- abs(GHGchamberdf_eosense_precleaned2$e_FluxN2OL_nmol_per_m2_per_s/GHGchamberdf_eosense_precleaned2$FluxN2OL_nmol_per_m2_per_s)

# ratio of CH4 flux to error
GHGchamberdf_eosense_precleaned2$CH4Eerrorperflux <- abs(GHGchamberdf_eosense_precleaned2$e_FluxCH4E_nmol_per_m2_per_s/GHGchamberdf_eosense_precleaned2$FluxCH4E_nmol_per_m2_per_s)

GHGchamberdf_eosense_precleaned2$CH4Lerrorperflux <- abs(GHGchamberdf_eosense_precleaned2$e_FluxCH4L_nmol_per_m2_per_s/GHGchamberdf_eosense_precleaned2$FluxCH4L_nmol_per_m2_per_s)

# save as csv
GHGchamberdf_eosense <- GHGchamberdf_eosense_precleaned2
write.csv(GHGchamberdf_eosense, file=paste(pathsavefiles, "GHGchamberdf_eosense.csv", sep = ""), row.names=FALSE)  



### TO DO
# print some diagnostics

# that weird ridiculously high N2O flux - Whendee says keep it in
# right now choosing to not eliminate any CH4 fluxes

# if error for that gas is above a certain ratio to the flux itself

# if abiotic factors about the chamber are a problem
# pressure
# temperature
# MeanH2O_percent

# print and save a bunch of information about the diagnostics of these data tests



########################################################################
# NOTES, TO DO AND TESTING

##### bring in pre-drought (Feb and March data) - have this in the processed Leilei version (linear algorithm flux solve only), but get the raw data and then bring it through the same workflow



