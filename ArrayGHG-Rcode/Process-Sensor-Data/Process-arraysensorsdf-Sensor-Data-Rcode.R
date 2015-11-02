# Process-arraysensorsdf-Sensor-Data-Rcode.R
# 
# processing raw data files from the PR array sensors
#
# code heavily indebted to: pitTDR_CR1000_PL_02042014.R
# AUTHORS: Paulo Brando, Paul Lefebvre, Marcia Macedo
# LAST UPDATED: Aug 1 2013 to separate out site ID into its own column, calculate averages, and fix months to numeric
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# output products:
# arraysensorsdf.csv


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

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/SensorFigures/"

# base excel sheet to start with (from before my time in the Silver lab)


# list of pit data files to add onto that excel sheet
sensordatapath = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan/"
f_sensor = list.files(path=sensordatapath, pattern="*.dat")
# T1 .dat files is through sensor 19 (transect 4) and T2 is the sensors after that






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






