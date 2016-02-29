# Process-arraysensorsdf-Sensor-Data-Rcode.R
# 
# processing raw data files from the PR array sensors
#
# code heavily indebted to: pitTDR_CR1000_PL_02042014.R
# AUTHORS: Paulo Brando, Paul Lefebvre, Marcia Macedo
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# output products:
# 


########################################################################
# GET READY TO BRING IN DATA

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(chron)
library(lubridate)
library(lattice)
library(reshape2)
options(java.parameters = "-Xmx5000m")
library(xlsx)

# define standard error function
ste <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# where to save outputs
#pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"
pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan-practice/Getting things together for Whendee/"
pathsavefigs = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/SensorFigures/"
sensordatapath = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan/2016-02/"
calibrationdatapath = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan-practice/"


########################################################################
# BRING IN NEW DATA SHEETS

# what are the new temperature/moisture/O2 csv files to bring in?
CR1000_EIsoilT1 <- read.csv(paste(sensordatapath,"CR1000_EIsoilT1June2015.csv",sep=""), stringsAsFactors=FALSE)
CR1000_EIsoilT2 <- read.csv(paste(sensordatapath,"CR1000_EIsoilT2June2015.csv",sep=""), stringsAsFactors=FALSE)
CR1000_oxygenEV <- read.csv(paste(sensordatapath,"CR1000_oxygenEVJune2015.csv",sep=""), stringsAsFactors=FALSE)


########################################################################
# INFO ABOUT THE DATA

## temperature and moisture info is in the same files:
# T1 .csv files is through sensor 19 and T2 is the sensors from 20-35
# vol water content columns are labeled as: VWC_Avg, VWC_2_Avg, VWC_3_Avg, etc. in row 2
# VWC is in units of m^3/m^3
# temperature columns are labeled as: T_Avg, T_2_Avg, T_3_Avg, etc. also in row 2
# VWC is in units of Deg C
# we want the columns that say "Avg" in row 4, not those that say "Smp" in row 4

## oxygen info is in its own file:
# in excel, the relevant columns for O2 data are
# AI-AX = sensor 1-16, FG-FV = sensor 17-32, JE-JG = sensor 33-35
# see the calibration curve stuff in the hourly tab in the excel file


########################################################################
# VWC: HOURLY --> DAILY

## get T1 data

# where is the VWC data?
vwccols <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39)
dataend <- dim(CR1000_EIsoilT1)[1]
vwcrows <- c(4:dataend)

# what are the variable names?
vwcnamesT1 <- c("TIMESTAMP","Sensor01","Sensor02","Sensor03","Sensor04","Sensor05","Sensor06","Sensor07","Sensor08",
                "Sensor09","Sensor10","Sensor11","Sensor12","Sensor13","Sensor14","Sensor15","Sensor16",
                "Sensor17","Sensor18","Sensor19")
#vwcnamesT1 <- as.character(CR1000_EIsoilT1[1,vwccols])

# extract the data and give it useful col names
vwchourlyT1 <- CR1000_EIsoilT1[vwcrows, vwccols]
names(vwchourlyT1) <- vwcnamesT1

## get T2 data

# where is the VWC data?
vwccols <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)
dataend <- dim(CR1000_EIsoilT2)[1]
vwcrows <- c(4:dataend)

# what are the variable names?
vwcnamesT2 <- c("TIMESTAMP","Sensor20","Sensor21","Sensor22","Sensor23","Sensor24",
                "Sensor25","Sensor26","Sensor27","Sensor28","Sensor29","Sensor30",
                "Sensor31","Sensor32","Sensor33","Sensor34","Sensor35")
#vwcnamesT2 <- as.character(CR1000_EIsoilT1[1,vwccols])

# extract the data and give it useful col names
vwchourlyT2 <- CR1000_EIsoilT2[vwcrows, vwccols]
names(vwchourlyT2) <- vwcnamesT2

## bind T1 and T2 in wide format, make useful columns

vwchourly <- full_join(vwchourlyT1, vwchourlyT2, by="TIMESTAMP")
# vwchourly <- cbind(vwchourlyT1, vwchourlyT2[,2:17])

# replace 7999 values with NA
vwchourly[vwchourly==7999] <- NA

# make separate columns for date and time
vwchourly$TIMESTAMP2 <- parse_date_time(vwchourly$TIMESTAMP, orders="mdy hm") # as date
# get hour
vwchourly$Hour <- gsub( ".* ", "", vwchourly$TIMESTAMP) # as character
vwchourly$Hour2 <- hour(vwchourly$TIMESTAMP2) # as int
# get date
vwchourly$Date <- gsub( " .*$", "", vwchourly$TIMESTAMP) # as character
vwchourly$Date2 <- mdy(vwchourly$Date) # as date

# get into percent instead of fraction

## convert to long format

vwchourlylong <- gather(vwchourly, SensorID, VWC, Sensor01:Sensor35)
vwchourlylong$SensorID <- as.character(vwchourlylong$SensorID)
vwchourlylong$VWC <- as.numeric(vwchourlylong$VWC)

## get daily mean, sd, max, min
vwcdailylong <- ddply(vwchourlylong,.(Date2, SensorID),
             summarize,
             avgVWC=mean(VWC, na.rm = TRUE),
             sdVWC=sd(VWC, na.rm = TRUE),
             seVWC=ste(VWC),
             maxVWC=max(VWC, na.rm = TRUE),
             minVWC=min(VWC, na.rm = TRUE)) # na.rm=T already in the function definition for ste()


########################################################################
# TEMP: HOURLY --> DAILY

## get T1 data

# where is the VWC data?
tempcols <- c(1,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40)
dataend <- dim(CR1000_EIsoilT1)[1]
temprows <- c(4:dataend)

# what are the variable names?
tempnamesT1 <- c("TIMESTAMP","Sensor01","Sensor02","Sensor03","Sensor04","Sensor05","Sensor06","Sensor07","Sensor08",
                "Sensor09","Sensor10","Sensor11","Sensor12","Sensor13","Sensor14","Sensor15","Sensor16",
                "Sensor17","Sensor18","Sensor19")
#vwcnamesT1 <- as.character(CR1000_EIsoilT1[1,vwccols])

# extract the data and give it useful col names
temphourlyT1 <- CR1000_EIsoilT1[temprows, tempcols]
names(temphourlyT1) <- tempnamesT1

## get T2 data

# where is the VWC data?
tempcols <- c(1,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34)
#vwccols  <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)
dataend <- dim(CR1000_EIsoilT2)[1]
temprows <- c(4:dataend)

# what are the variable names?
tempnamesT2 <- c("TIMESTAMP","Sensor20","Sensor21","Sensor22","Sensor23","Sensor24",
                "Sensor25","Sensor26","Sensor27","Sensor28","Sensor29","Sensor30",
                "Sensor31","Sensor32","Sensor33","Sensor34","Sensor35")
#vwcnamesT2 <- as.character(CR1000_EIsoilT1[1,vwccols])

# extract the data and give it useful col names
temphourlyT2 <- CR1000_EIsoilT2[temprows, tempcols]
names(temphourlyT2) <- tempnamesT2

## bind T1 and T2 in wide format, make useful columns

temphourly <- full_join(temphourlyT1, temphourlyT2, by="TIMESTAMP")
# vwchourly <- cbind(vwchourlyT1, vwchourlyT2[,2:17])

# replace 7999 values with NA
temphourly[temphourly==7999] <- NA

# make separate columns for date and time
temphourly$TIMESTAMP2 <- parse_date_time(temphourly$TIMESTAMP, orders="mdy hm") # as date
# get hour
temphourly$Hour <- gsub( ".* ", "", temphourly$TIMESTAMP) # as character
temphourly$Hour2 <- hour(temphourly$TIMESTAMP2) # as int
# get date
temphourly$Date <- gsub( " .*$", "", temphourly$TIMESTAMP) # as character
temphourly$Date2 <- mdy(temphourly$Date) # as date

## convert to long format

temphourlylong <- gather(temphourly, SensorID, Temp, Sensor01:Sensor35)
temphourlylong$SensorID <- as.character(temphourlylong$SensorID)
temphourlylong$Temp <- as.numeric(temphourlylong$Temp)

## get daily mean, sd, max, min
tempdailylong <- ddply(temphourlylong,.(Date2, SensorID),
                      summarize,
                      avgTemp=mean(Temp, na.rm = TRUE),
                      sdTemp=sd(Temp, na.rm = TRUE),
                      seTemp=ste(Temp),
                      smaxTemp=max(Temp, na.rm = TRUE),
                      minTemp=min(Temp, na.rm = TRUE)) # na.rm=T already in the function definition for ste()


########################################################################
# O2: HOURLY --> DAILY

# get O2 datalogger data
# AI-AX = sensor 1-16 (col 35-50), FG-FV = sensor 17-32 (col 163-178), JE-JG = sensor 33-35 (col 265-267)
# note that this data is all in mV until we convert it later

# where is the O2 data?
O2cols <- c(1,35:50,163:178,265:267)
dataend <- dim(CR1000_oxygenEV)[1]
O2rows <- c(4:dataend)

# what are the variable names?
O2names <- c("TIMESTAMP","Sensor01","Sensor02","Sensor03","Sensor04","Sensor05","Sensor06","Sensor07","Sensor08",
                 "Sensor09","Sensor10","Sensor11","Sensor12","Sensor13","Sensor14","Sensor15","Sensor16",
                 "Sensor17","Sensor18","Sensor19","Sensor20","Sensor21","Sensor22","Sensor23","Sensor24",
                 "Sensor25","Sensor26","Sensor27","Sensor28","Sensor29","Sensor30",
                 "Sensor31","Sensor32","Sensor33","Sensor34","Sensor35")
#O2names <- as.character(CR1000_oxygenEV[1,O2cols])

# extract the data and give it useful col names
O2hourly <- CR1000_oxygenEV[O2rows, O2cols]
names(O2hourly) <- O2names

## make useful columns

# replace 7999 values with NA
O2hourly[O2hourly==7999] <- NA

# make separate columns for date and time
O2hourly$TIMESTAMP2 <- parse_date_time(O2hourly$TIMESTAMP, orders="mdy hm") # as date
# get hour
O2hourly$Hour <- gsub( ".* ", "", O2hourly$TIMESTAMP) # as character
O2hourly$Hour2 <- hour(O2hourly$TIMESTAMP2) # as int
# get date
O2hourly$Date <- gsub( " .*$", "", O2hourly$TIMESTAMP) # as character
O2hourly$Date2 <- mdy(O2hourly$Date) # as date

## convert to long format

O2hourlylong <- gather(O2hourly, SensorID, O2mV, Sensor01:Sensor35)
O2hourlylong$SensorID <- as.character(O2hourlylong$SensorID)
O2hourlylong$O2mV <- as.numeric(O2hourlylong$O2mV)

## convert from mV to percent using the calibration curves for each sensor

# bring in calibration curve info
O2cal <- read.csv(paste(calibrationdatapath,"O2sensorscalibration.csv",sep=""), stringsAsFactors=FALSE)
# join to O2hourlylong
O2hourlylong <- full_join(O2hourlylong, O2cal)
# solve for percent O2
O2hourlylong$O2pct <- ((O2hourlylong$O2mV * O2hourlylong$Slope) - O2hourlylong$Intercept) * 100

## get daily mean, sd, max, min
O2dailylong <- ddply(O2hourlylong,.(Date2, SensorID),
                       summarize,
                       avgO2pct=mean(O2pct, na.rm = TRUE),
                       sdO2pct=sd(O2pct, na.rm = TRUE),
                       seO2pct=ste(O2pct),
                       smaxO2pct=max(O2pct, na.rm = TRUE),
                       minO2pct=min(O2pct, na.rm = TRUE)) # na.rm=T already in the function definition for ste()


##### Q for Ryan... why are the daily averages off between the hourly tab and the daily summary tab?



########################################################################
# VWC + TEMP + O2 --> ONE DF

# add O2 dataframe into this once you've defined it

fulldaily1 <- full_join(tempdailylong, vwcdailylong)
fulldaily2 <- full_join(fulldaily1, O2dailylong)

# put transect info in as a column near the front for easy reading
definetransects <- read.csv(paste(calibrationdatapath,"definetransects.csv",sep=""), stringsAsFactors=FALSE)
fulldaily3 <- full_join(fulldaily2, definetransects)
# dim(fulldaily3)[2] is where TransectID is; move to early column
endcol <- dim(fulldaily3)[2]
secondlast <- endcol-1
fulldaily <- fulldaily3[,c(1:2,endcol,3:secondlast)]

# add TopoLocation
defineTopoLocation <- read.csv(paste(calibrationdatapath,"defineTopoLocation.csv",sep=""), stringsAsFactors=FALSE)
fulldaily4 <- full_join(fulldaily, defineTopoLocation)
# dim(fulldaily4)[2] is where TopoLocation is; move to early column
endcol <- dim(fulldaily4)[2]
secondlast <- endcol-1
fulldaily <- fulldaily4[,c(1:2,endcol,3:secondlast)]
fulldaily$TopoLocation <- as.factor(fulldaily$TopoLocation)


########################################################################
# DEFINE WHAT GETS ADDED TO THE RUNNING SPREADSHEET

# hourly wide for each
ht(vwchourly)
ht(temphourly)
ht(O2hourly)

# daily wide for each
# go from long to wide using spread() from tidyr
# only do this for the average daily values, not the sd, se, min or max

vwcdailylongavg <- vwcdailylong[,c(1:3)]
vwcdailywideavg <- spread(vwcdailylongavg, SensorID, avgVWC)

tempdailylongavg <- tempdailylong[,c(1:3)]
tempdailywideavg <- spread(tempdailylongavg, SensorID, avgTemp)

O2dailylongavg <- O2dailylong[,c(1:3)]
O2dailywideavg <- spread(O2dailylongavg, SensorID, avgO2pct)

head(tempdailywideavg)
head(vwcdailywideavg)
head(O2dailywideavg)

# daily long combined dataset
head(fulldaily)


########################################################################
# SAVE AS CSVS

# hourly wide for each

# make ok to write to CSV
vwchourly$TIMESTAMP2 <- as.character(vwchourly$TIMESTAMP2)
vwchourly$Date2 <- as.character(vwchourly$Date2)
temphourly$TIMESTAMP2 <- as.character(temphourly$TIMESTAMP2)
temphourly$Date2 <- as.character(temphourly$Date2)
O2hourly$TIMESTAMP2 <- as.character(O2hourly$TIMESTAMP2)
O2hourly$Date2 <- as.character(O2hourly$Date2)

# write to csv
write.csv(vwchourly, file=paste(pathsavefiles, "vwchourly.csv", sep = ""), row.names=FALSE)
write.csv(temphourly, file=paste(pathsavefiles, "temphourly.csv", sep = ""), row.names=FALSE)
write.csv(O2hourly, file=paste(pathsavefiles, "O2hourly.csv", sep = ""), row.names=FALSE)

# daily wide for each

# make ok to write to CSV
vwcdailywideavg$Date2 <- as.character(vwcdailywideavg$Date2)
tempdailywideavg$Date2 <- as.character(tempdailywideavg$Date2)
O2dailywideavg$Date2 <- as.character(O2dailywideavg$Date2)

# write to csv
write.csv(vwcdailywideavg, file=paste(pathsavefiles, "vwcdailywideavg.csv", sep = ""), row.names=FALSE)
write.csv(tempdailywideavg, file=paste(pathsavefiles, "tempdailywideavg.csv", sep = ""), row.names=FALSE)
write.csv(O2dailywideavg, file=paste(pathsavefiles, "O2dailywideavg.csv", sep = ""), row.names=FALSE)

# daily long combined dataset

# make ok to write to CSV
fulldaily$Date2 <- as.character(fulldaily$Date2)

# write to csv
write.csv(fulldaily, file=paste(pathsavefiles, "fulldaily.csv", sep = ""), row.names=FALSE)


##### FOR NOW, CHRISTINE IS JUST PASTING THESE INTO THE APPROPRIATE PLACE ON THE RUNNING EXCEL SHEET










########################################################################
# FIRST TIME RUNNING THE ABOVE - PREP RUNNING FILES

# only do this if you haven't already gotten the running data files working well
needtobuildrunningfiles <- "y"

if(alreadybuiltcsv=="y") {
      
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
      
      # if the running data files already work well, load them
      print("running sensor data files were built previously; loading them now")
      
      # load csvs
      GHGchamberdf_eosense_precleaned <- read.csv(paste(pathsavefiles, "GHGchamberdf_eosense_precleaned.csv", sep = ""), stringsAsFactors=FALSE)
      
      
      
      # fix dates
      GHGchamberdf_eosense_precleaned$DateTime2 <- ymd_hms(GHGchamberdf_eosense_precleaned$DateTime)
      
}











########################################################################
# ADD TO RUNNING CSV FILES AND SAVE SUMMARY FIGURES






########################################################################
# BRING IN RUNNING DATA SHEETS

# bring in the file where the running results are kept
runningmaster2 <- read.xlsx(paste(sensordatapath,"daily sensor data running master.xlsx",sep=""),"data to stat gaps included")
# in case it got brought in with extra variables, weirdly
runningmaster <- runningmaster2[,1:8]








library(XLConnect)
#Create file
wb <- loadWorkbook("file2.xlsx", create = TRUE)

# Create a worksheet called 'cars'
createSheet(wb, name = "cars")

#write data cars to sheet
writeWorksheet(wb, cars, sheet = "cars")

# Merge the cells A2:A3 and A4:A5 on the worksheet created above
mergeCells(wb, sheet = "cars", reference = c("A2:A3","A4:A5"))

# Save workbook
saveWorkbook(wb)





########################################################################
# ADD TO RUNNING EXCEL FILE









library(XLConnect)
#Create file
wb <- loadWorkbook("file2.xlsx", create = TRUE)

# Create a worksheet called 'cars'
createSheet(wb, name = "cars")

#write data cars to sheet
writeWorksheet(wb, cars, sheet = "cars")

# Merge the cells A2:A3 and A4:A5 on the worksheet created above
mergeCells(wb, sheet = "cars", reference = c("A2:A3","A4:A5"))

# Save workbook
saveWorkbook(wb)













# if you want to test the data in this sheet, graph via the below

# ########################################################################
# # SUMMARY STATS: O2, MOISTURE ACROSS TRANSECTS AT EACH DATE
# 
# # summarySE using plyr
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
# 
# # summarySE O2
# summarytab1tmp <- summarySE(data=fulldaily, measurevar="O2", c("Date2", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
# # summarySE moisture
# summarytab2tmp <- summarySE(data=fulldaily, measurevar="avgVWC", c("Date2", "TopoLocation"), na.rm=TRUE, renameallcols=TRUE)
# 
# 
# ########################################################################
# # EXPLORATORY FIGURES: TIME SERIES
# 
# topocolors <- c("navy","blue","dark green","green","yellow","orange","red")
# topobreaks <- c("1","2","3","4","5","6","7")
# topolabs <- c("Ridge","2","3","4","5","6","Valley")
# 
# # O2 by date (mean and se)
# p1 <- ggplot(summarytab1, aes(x=Date, y=meanO2, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) + ylab("Soil O2 (Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y"))  #+ geom_line()
# 
# # moisture by date (mean and se)
# p2 <- ggplot(summarytab2, aes(x=Date2, y=meanavgVWC, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanavgVWC-seavgVWC, ymax=meanavgVWC+seavgVWC), alpha=0.5) + ylab("Soil Moisture (Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y")) #+ geom_line()










#### everything beyond here is from the pit code, so still need to do the vast majority of this work


# list of pit data files
pitdatapath = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Raw/Pit-Data-Raw-TDR/"
f_pit = list.files(path=pitdatapath, pattern="*.dat")

# separate pits CR1000 from CR10X, and CR10X pits from eachother
CR1000 = f_pit[grepl("CR1000", f_pit)]
CR100 = f_pit[grepl("CR100_", f_pit)]
CR10X_C2 = f_pit[grepl("CR10X_C2", f_pit)]
#CR10X_A1A = f_pit[grepl("CR10X_A1A", f_pit)]
#CR10X_A1C = f_pit[grepl("CR10X_A1C", f_pit)]

# go to working directory
setwd("~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Raw/Pit-Data-Raw-TDR/")



########################################################################
# INFO ABOUT THE DATA

# in both cases below, see screenshots that Paul sent me with the variable info

# CR100 and CR1000 have three types of variables: PA_uS_Avg (1-12), VW_Avg (1-12), tc_e_Avg (1-12), plus columns for time and voltage
# PA_uS_Avg is the period in u seconds that the measurement is done over
# VW_Avg is volumetric water content
# tc_e_Avg is temperature in degrees Celsius

# CR10X data loggers have unlabeled data
# see CR10X files - Screenshot 2015-06-19 11.33.15.jpg for Paul info about vars
# I'm honestly not quite sure what these variables are, but I'm pretty sure it's the temp data (why are there 12 and not 7?) and then the volumetric water content data with time/date data at the beginning
# I don't think columns with the period are included here


########################################################################
# BRING IN DATA BY EACH LOGGER TYPE

# Import all files from pits with CR1000
# note that CR1000_Mutum_ 20140228.dat temperature measures are all NaN (sensors on the fritz?)
CR1000.list = list()
for(i in 1:length(CR1000))
{
      cat("filename:", CR1000[i], "\n")
      
      # get header names
      tmp = data.frame(read.csv(CR1000[i], header=F, stringsAsFactors=F))
      colnames <- as.character(tmp[2,])
      
      # bring in and rename
      CR1000.list[[i]] <- read.csv(CR1000[i], header=F, skip=4)
      names(CR1000.list[[i]]) = colnames     
      
      # add col for file
      CR1000.list[[i]]$filename <- CR1000[i]
      
}
CR1000.import = do.call("rbind", CR1000.list)

# Import all files from pits with CR100
CR100.list = list()
for(i in 1:length(CR100))
{
      cat("filename:", CR100[i], "\n")
      
      # get header names
      tmp = data.frame(read.csv(CR100[i], header=F, stringsAsFactors=F))
      colnames <- as.character(tmp[2,])
      
      # bring in and rename
      CR100.list[[i]] <- read.csv(CR100[i], header=F, skip=4)
      names(CR100.list[[i]]) = colnames     
      
      # add col for file
      CR100.list[[i]]$filename <- CR100[i]
      
}
CR100.import = do.call("rbind", CR100.list)

# Import all files from pits with CR10X
# need to treat each CR10X pit separately, but here I only have C2, so can use a single list

# set up uniform column labels for CR10X files
colnames2 = c("prog","yr","day","hr")
tempnames <- colnames[27:38]
vwnames <- colnames[15:26]
colnames2 <- c(colnames2,tempnames,vwnames) # confirm with Paul that this is right!!!!

# Pit C2
CR10X_C2.list = list()
for(i in 1:length(CR10X_C2))
{
      cat("filename:", CR10X_C2[i], "\n")
      CR10X_C2.list[[i]] = data.frame(read.csv(CR10X_C2[i], header=F))
      head(CR10X_C2.list[[i]])
      names(CR10X_C2.list[[i]]) = colnames2
      
      # CR10X_C2_ 20150127.dat has voltage column, which the others don't
      if(CR10X_C2[i]=="CR10X_C2_ 20150127.dat") {
            CR10X_C2.list[[i]] <- CR10X_C2.list[[i]][1:28]
      }
      
      # some of the VW info looks like it's already been multiplied by 100; fix so everything's in the same units
      firstcol <- which(colnames(CR10X_C2.list[[i]])=="VW_Avg(1)")
      lastcol <- which(colnames(CR10X_C2.list[[i]])=="VW_Avg(12)")
      if(CR10X_C2.list[[i]][1, firstcol]>1) {
            CR10X_C2.list[[i]][firstcol:lastcol] <- CR10X_C2.list[[i]][firstcol:lastcol]/100
      }
      
      # add col for file
      CR10X_C2.list[[i]]$filename <- CR10X_C2[i]
      
}
CR10X_C2.import = do.call("rbind", CR10X_C2.list)


########################################################################
# ADD DATE AND SITE COLUMNS BEFORE MERGING

# dates and time
CR1000.import$DateLong <- ymd_hms(CR1000.import$TIMESTAMP, tz = "Etc/GMT-3")
CR1000.import$Date = as.Date(CR1000.import$DateLong)
CR1000.import$Hour = hour(CR1000.import$DateLong)

CR100.import$DateLong <- ymd_hms(CR100.import$TIMESTAMP, tz = "Etc/GMT-3")
CR100.import$Date = as.Date(CR100.import$DateLong)
CR100.import$Hour = hour(CR100.import$DateLong)

CR10X_C2.import$DateLong <- as.Date(paste(as.character(CR10X_C2.import$yr), "-01-01", sep=""))
CR10X_C2.import$DateLong <- CR10X_C2.import$DateLong + CR10X_C2.import$day
CR10X_C2.import$Date = as.Date(CR10X_C2.import$DateLong)
CR10X_C2.import <- transform(CR10X_C2.import, Hour = ifelse(hr==1800, as.integer(18), ifelse(hr==2400, as.integer(0), ifelse(hr==600, as.integer(6), as.integer(12)))))

# add pitID label
CR1000.import$PitID <- NA
CR1000.import$PitID[grepl("K4", CR1000.import$filename)] <- "K4"
CR1000.import$PitID[grepl("C2", CR1000.import$filename)] <- "C2"
CR1000.import$PitID[grepl("M8", CR1000.import$filename)] <- "M8"
CR1000.import$PitID[grepl("Mutum", CR1000.import$filename)] <- "Mutum"

CR100.import$PitID <- NA
CR100.import$PitID[grepl("K4", CR100.import$filename)] <- "K4"
CR100.import$PitID[grepl("C2", CR100.import$filename)] <- "C2"
CR100.import$PitID[grepl("M8", CR100.import$filename)] <- "M8"
CR100.import$PitID[grepl("Mutum", CR100.import$filename)] <- "Mutum"

CR10X_C2.import$PitID <- NA
CR10X_C2.import$PitID[grepl("K4", CR10X_C2.import$filename)] <- "K4"
CR10X_C2.import$PitID[grepl("C2", CR10X_C2.import$filename)] <- "C2"
CR10X_C2.import$PitID[grepl("M8", CR10X_C2.import$filename)] <- "M8"
CR10X_C2.import$PitID[grepl("Mutum", CR10X_C2.import$filename)] <- "Mutum"

# LUType label
CR1000.import$LUType <- NA
CR1000.import$LUType[grepl("K4", CR1000.import$filename)] <- "Forest"
CR1000.import$LUType[grepl("C2", CR1000.import$filename)] <- "Forest"
CR1000.import$LUType[grepl("M8", CR1000.import$filename)] <- "Forest"
CR1000.import$LUType[grepl("Mutum", CR1000.import$filename)] <- "Soya SC"

CR100.import$LUType <- NA
CR100.import$LUType[grepl("K4", CR100.import$filename)] <- "Forest"
CR100.import$LUType[grepl("C2", CR100.import$filename)] <- "Forest"
CR100.import$LUType[grepl("M8", CR100.import$filename)] <- "Forest"
CR100.import$LUType[grepl("Mutum", CR100.import$filename)] <- "Soya SC"

CR10X_C2.import$LUType <- NA
CR10X_C2.import$LUType[grepl("K4", CR10X_C2.import$filename)] <- "Forest"
CR10X_C2.import$LUType[grepl("C2", CR10X_C2.import$filename)] <- "Forest"
CR10X_C2.import$LUType[grepl("M8", CR10X_C2.import$filename)] <- "Forest"
CR10X_C2.import$LUType[grepl("Mutum", CR10X_C2.import$filename)] <- "Soya SC"


########################################################################
# CLEAN DUPLICATE ROWS

CR100.import <- CR100.import[!duplicated(CR100.import[,c('Date', 'Hour', 'PitID', 'VW_Avg(1)')]),]
CR1000.import <- CR1000.import[!duplicated(CR1000.import[,c('Date', 'Hour', 'PitID', 'VW_Avg(1)')]),]
CR10X_C2.import <- CR10X_C2.import[!duplicated(CR10X_C2.import[,c('Date', 'Hour', 'PitID', 'VW_Avg.2.')]),]


########################################################################
# MERGE PIT INFO

# replace names in CR100/CR1000 so they match CR10X
tmp <- gsub("\\(", ".", names(CR100.import)); tmp <- gsub("\\)", ".", tmp)
names(CR100.import) <- tmp
tmp <- gsub("\\(", ".", names(CR1000.import)); tmp <- gsub("\\)", ".", tmp)
names(CR1000.import) <- tmp

# cut down to only the needed columns
keep1 <- c(names(CR100.import)[15:38],names(CR100.import)[40:45])
CR1000.import <- subset(CR1000.import, select = keep1)
CR100.import <- subset(CR100.import, select = keep1)
CR10X_C2.import <- subset(CR10X_C2.import, select = keep1)

# combine
pitsTDR = rbind.fill(CR1000.import, CR100.import, CR10X_C2.import)


########################################################################
# CALC MORE ROBUST VWC


##### THIS PART IS FUCKED


#### not sure if i need to calculate my own vol. water content?  
# this was in paul's code - I think that it corrects for the fact that the data logger calcs VWC based on some placeholder coefficients he programmed in back in the day

# # coefficients for quadratic equation to calculate VWC:
# # (use these for all pits, later apply separate coefficients to Soy vs Forest pits)
# a <- -.0015
# # the multiplier
# b <- .0943
# # the intercept
# c <- -1.0998
# 
# # get columns that are VW data
# firstcol <- which(colnames(pitsTDR)=="VW_Avg.1.")
# lastcol <- which(colnames(pitsTDR)=="VW_Avg.12.")
# 
# # make simpler df
# simp <- pitsTDR[,c(firstcol:lastcol)]
# ###simp <- simp*100 # will this fix the neg number problem?  A: no
# tdr <- ((simp^2)*a) + (simp*b) + c

# 
# #######################################################
# # strip things down to necessities for calculating VWC
# 
# simp <- pits[,c(1:12)]
# nrow(simp)
# head(pits)
# head(simp)
# 
# 
# #XXXXXXX problems here XXXXXXXXXX
# #non-numeric argument to binary operator - 
# tdr <- ((simp^2)*a) + (simp*b) + c
# nrow(tdr)
# names(tdr)= c("VWCsfc","VWC30cm","VWC50cm","VWC1m","VWC2m","VWC3m","VWC4m","VWC5m","VWC6m","VWC7m","VWC8m","VWC9m")
# head(tdr)
# 



########################################################################
# ADD USEFUL COLS AND CLEAN THINGS SOME MORE

# set negative values to NA
is.na(pitsTDR) <- pitsTDR < 0

# month of sampling
pitsTDR <- transform(pitsTDR, Month = lubridate::month(pitsTDR$DateLong, label=TRUE))
# year of sampling
pitsTDR <- transform(pitsTDR, Year = year(pitsTDR$DateLong))
# year-month combo variable
pitsTDR <- transform(pitsTDR, YearMonth = paste(year(pitsTDR$DateLong),month(pitsTDR$DateLong),sep="-"))


# fix hours
# this converts hour from an integer to an hour:min:second thing
#pitsTDR$Hour = chron(times = paste(pitsTDR$Hour, "00", "00", sep=":")) 

# for some reason the CR1000$Date dates are messed up; try and fix
pitsTDR$Date <- paste(year(pitsTDR$DateLong),month(pitsTDR$DateLong),day(pitsTDR$DateLong),sep="-")
# I noticed the problem in 2013-12-01 00:00:00 Mutum (Date was listed as 2013-11-30)
# now appears fixed

# order according to names, date and hour
pitsTDR <- pitsTDR[order(pitsTDR$PitID, pitsTDR$Date, pitsTDR$Hour),]
# put useful columns in the front
endcol <- dim(pitsTDR)[2]
pitsTDR <- pitsTDR[,c(27:endcol,1:26)]

# convert from wide to long so you can save both versions
pitsTDR_long <- reshape(pitsTDR, 
             varying = names(pitsTDR)[8:31], 
             v.names = "measurement",
             timevar = "sensor", 
             times = names(pitsTDR)[8:31], 
             direction = "long")
pitsTDR_long$measurement <- as.numeric(pitsTDR_long$measurement)
# sensor type
pitsTDR_long$DataType <- "VW"
pitsTDR_long$DataType[grepl("tc_e", pitsTDR_long$sensor)] <- "degC"


########################################################################
# SAVE ALL DATA AS CSVS

pathsavefiles = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/"

# everything
write.csv(pitsTDR, file=paste(pathsavefiles, "pits-TDR-thermocouple-processed.csv", sep = ""), row.names=FALSE)  
write.csv(pitsTDR_long, file=paste(pathsavefiles, "pits-TDR-thermocouple-long-processed.csv", sep = ""), row.names=FALSE)  

# only temp data
pitsthermo <- pitsTDR[,c(1:7,20:endcol)]
pitsthermo_long = pitsTDR_long[grepl("tc_e", pitsTDR_long$sensor),]
write.csv(pitsthermo, file=paste(pathsavefiles, "pits-thermo-only-processed.csv", sep = ""), row.names=FALSE)  
write.csv(pitsthermo_long, file=paste(pathsavefiles, "pits-thermo-only-long-processed.csv", sep = ""), row.names=FALSE)  

# only VWC data
pitsTDRonly <- pitsTDR[,c(1:19,32:endcol)]
pitsTDRonly_long = pitsTDR_long[grepl("VW", pitsTDR_long$sensor),]
write.csv(pitsTDRonly, file=paste(pathsavefiles, "pits-TDR-only-processed.csv", sep = ""), row.names=FALSE)  
write.csv(pitsTDRonly_long, file=paste(pathsavefiles, "pits-TDR-only-long-processed.csv", sep = ""), row.names=FALSE)  


########################################################################
# SUMMARY STATISTICS

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarize by month/year, pit, and what sensor it is
pitTDRsummarytable <- summarySE(data=pitsTDR_long, measurevar="measurement", c("PitID", "Month", "Year", "DataType", "YearMonth", "sensor", "LUType"), na.rm=TRUE, renameallcols=F)

# get rid of rows with no count
pitTDRsummarytable <- subset(pitTDRsummarytable, !pitTDRsummarytable$N==0)

# include sensor depth variable
pitTDRsummarytable$sampledepth <- -9999

pitTDRsummarytable$sampledepth[grep("tc_e_Avg.1.", pitTDRsummarytable$sensor)] <- 0
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.2.", pitTDRsummarytable$sensor)] <- 15
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.3.", pitTDRsummarytable$sensor)] <- 40
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.4.", pitTDRsummarytable$sensor)] <- 75
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.5.", pitTDRsummarytable$sensor)] <- 150
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.6.", pitTDRsummarytable$sensor)] <- 250
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.7.", pitTDRsummarytable$sensor)] <- 350
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.8.", pitTDRsummarytable$sensor)] <- 450
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.9.", pitTDRsummarytable$sensor)] <- NA
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.10.", pitTDRsummarytable$sensor)] <- NA
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.11.", pitTDRsummarytable$sensor)] <- NA
pitTDRsummarytable$sampledepth[grep("tc_e_Avg.12.", pitTDRsummarytable$sensor)] <- NA

pitTDRsummarytable$sampledepth[grep("VW_Avg.1.", pitTDRsummarytable$sensor)] <- 0
pitTDRsummarytable$sampledepth[grep("VW_Avg.2.", pitTDRsummarytable$sensor)] <- 30
pitTDRsummarytable$sampledepth[grep("VW_Avg.3.", pitTDRsummarytable$sensor)] <- 50
pitTDRsummarytable$sampledepth[grep("VW_Avg.4.", pitTDRsummarytable$sensor)] <- 100
pitTDRsummarytable$sampledepth[grep("VW_Avg.5.", pitTDRsummarytable$sensor)] <- 200
pitTDRsummarytable$sampledepth[grep("VW_Avg.6.", pitTDRsummarytable$sensor)] <- 300
pitTDRsummarytable$sampledepth[grep("VW_Avg.7.", pitTDRsummarytable$sensor)] <- 400
pitTDRsummarytable$sampledepth[grep("VW_Avg.8.", pitTDRsummarytable$sensor)] <- 500
pitTDRsummarytable$sampledepth[grep("VW_Avg.9.", pitTDRsummarytable$sensor)] <- 600
pitTDRsummarytable$sampledepth[grep("VW_Avg.10.", pitTDRsummarytable$sensor)] <- 700
pitTDRsummarytable$sampledepth[grep("VW_Avg.11.", pitTDRsummarytable$sensor)] <- 800
pitTDRsummarytable$sampledepth[grep("VW_Avg.12.", pitTDRsummarytable$sensor)] <- 900


########################################################################
# ESTIMATE VW FOR THE SAME DEPTHS AS TRACE GASES

# for more info on why this needs to be done, see the commented out notes at the bottom of this script

# fix the VW and sampledepth issue
# thermocouple and pit gases are at the same recorded depths, but the TDR depths are a bit off
# calc estimates of the VW values at those intermediate depths

# pitgassummary[1:7,2]
# [1]  15  40  75 150 250 350 450

# get rid of rows with NA in sample depth
pitTDRsummarytable <- subset(pitTDRsummarytable, !is.na(pitTDRsummarytable$sampledepth))

# function to get all the intermediate depth values
dealwithVW <- function(subsettest){
      
      # make 15
      row0 <- which(subsettest$sampledepth == 0)
      row30 <- which(subsettest$sampledepth == 30)
      subsettest <- rbind(subsettest,subsettest[row0,])
      subsettest[dim(subsettest)[1],8:12] <- (subsettest[row0,8:12]+subsettest[row30,8:12])/2
      
      # make 40
      row50 <- which(subsettest$sampledepth == 50)
      subsettest <- rbind(subsettest,subsettest[row0,])
      subsettest[dim(subsettest)[1],8:12] <- (subsettest[row30,8:12]+subsettest[row50,8:12])/2
      
      # make 75
      row100 <- which(subsettest$sampledepth == 100)
      subsettest <- rbind(subsettest,subsettest[row0,])
      subsettest[dim(subsettest)[1],8:12] <- (subsettest[row50,8:12]+subsettest[row100,8:12])/2
      
      # make 150
      row200 <- which(subsettest$sampledepth == 200)
      subsettest <- rbind(subsettest,subsettest[row0,])
      subsettest[dim(subsettest)[1],8:12] <- (subsettest[row100,8:12]+subsettest[row200,8:12])/2
      
      # make 250
      row300 <- which(subsettest$sampledepth == 300)
      subsettest <- rbind(subsettest,subsettest[row0,])
      subsettest[dim(subsettest)[1],8:12] <- (subsettest[row200,8:12]+subsettest[row300,8:12])/2
      
      # make 350 and 450 (both set to 300)
      subsettest <- rbind(subsettest,subsettest[row300,])
      subsettest[dim(subsettest)[1],12] <- 350
      subsettest <- rbind(subsettest,subsettest[row300,])
      subsettest[dim(subsettest)[1],12] <- 450
      
      # return
      subsettest
      
}

# get the groups to loop through
VWsub <- subset(pitTDRsummarytable, pitTDRsummarytable$DataType=="VW")
VWsub$easycallname <- paste(VWsub$PitID,VWsub$YearMonth,sep="-")
grouplist <- unique(VWsub$easycallname)

# loop
for (i in 1:length(grouplist)) {
      
      # group info and subset
      grouphere <- grouplist[i]
      subsettest <- subset(VWsub, VWsub$easycallname == grouphere)

      # calc vals
      tmp <- dealwithVW(subsettest)      
      
      # get new rows
      rowadd1 <- which(tmp$sampledepth == 300) + 1
      rowadd2 <- dim(tmp)[1]
      colgone <- dim(tmp)[2]
      addon <- tmp[c(rowadd1:rowadd2),-colgone]
      
      # bind to pitTDRsummarytable
      pitTDRsummarytable <- rbind(pitTDRsummarytable,addon)
      
}

# get rid of any repeat rows that you made
pitTDRsummarytable <- distinct(pitTDRsummarytable, DataType, sampledepth, PitID, YearMonth, LUType)


########################################################################
# SAVE SUMMARY DATAS CSV

pathsavefiles = "~/Documents/GITHUB/cso038code_TanguroPitGas/Pit-Data-Rprocessed/"
write.csv(pitTDRsummarytable, file=paste(pathsavefiles, "pitTDRsummarytable.csv", sep = ""), row.names=FALSE)



########################################################################
# NOTES AND TESTING

# vol water content is fucked up!!!  see line 228 and onwards

# check with paul to make sure that i correctly ID'ed all the variables, esp. in CR10X

# make the column calls more flexible using which, since now I have to rewrite the columns nums each time I reorder or subset





#### VW and depth offset problem

# problem where the variables are at diff depths, so have to set them to approx similar depths
# thermocouple and pit gases are at the same recorded depths
# so need to assign "equivalent" TDR depths
# do this by putting them in order?

# pitgassummary[1:7,2]
# [1]  15  40  75 150 250 350 450

# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.1.", pitTDRsummarytable$sensor)] <- 0
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.2.", pitTDRsummarytable$sensor)] <- 15
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.3.", pitTDRsummarytable$sensor)] <- 40
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.4.", pitTDRsummarytable$sensor)] <- 75
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.5.", pitTDRsummarytable$sensor)] <- 150
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.6.", pitTDRsummarytable$sensor)] <- 250
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.7.", pitTDRsummarytable$sensor)] <- 350
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.8.", pitTDRsummarytable$sensor)] <- 450
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.9.", pitTDRsummarytable$sensor)] <- NA
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.10.", pitTDRsummarytable$sensor)] <- NA
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.11.", pitTDRsummarytable$sensor)] <- NA
# pitTDRsummarytable$sampledepth[grep("tc_e_Avg.12.", pitTDRsummarytable$sensor)] <- NA
# 
# pitTDRsummarytable$sampledepth[grep("VW_Avg.1.", pitTDRsummarytable$sensor)] <- 0
# pitTDRsummarytable$sampledepth[grep("VW_Avg.2.", pitTDRsummarytable$sensor)] <- 30
# pitTDRsummarytable$sampledepth[grep("VW_Avg.3.", pitTDRsummarytable$sensor)] <- 50
# pitTDRsummarytable$sampledepth[grep("VW_Avg.4.", pitTDRsummarytable$sensor)] <- 100
# pitTDRsummarytable$sampledepth[grep("VW_Avg.5.", pitTDRsummarytable$sensor)] <- 200
# pitTDRsummarytable$sampledepth[grep("VW_Avg.6.", pitTDRsummarytable$sensor)] <- 300
# pitTDRsummarytable$sampledepth[grep("VW_Avg.7.", pitTDRsummarytable$sensor)] <- 400
# pitTDRsummarytable$sampledepth[grep("VW_Avg.8.", pitTDRsummarytable$sensor)] <- 500
# pitTDRsummarytable$sampledepth[grep("VW_Avg.9.", pitTDRsummarytable$sensor)] <- 600
# pitTDRsummarytable$sampledepth[grep("VW_Avg.10.", pitTDRsummarytable$sensor)] <- 700
# pitTDRsummarytable$sampledepth[grep("VW_Avg.11.", pitTDRsummarytable$sensor)] <- 800
# pitTDRsummarytable$sampledepth[grep("VW_Avg.12.", pitTDRsummarytable$sensor)] <- 900






