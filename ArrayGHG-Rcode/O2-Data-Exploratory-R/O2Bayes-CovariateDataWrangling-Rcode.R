# O2Bayes-CovariateDataWrangling-Rcode.R
# 
# taking soil covariates from Silver lab analyses for use in model / to include in study plan
# 
# O2 availability project
# CS O'Connell, UCB, Silver Lab

# output products:
#


########################################################################
# SET UP

library(xlsx)
library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(lubridate)
library(stringr)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso039code_O2Bayes/Exploratory O2 Analyses/"
pathsavefigures = "~/Documents/GITHUB/cso039code_O2Bayes/Exploratory O2 Analyses/Exploratory O2 Figures/"


########################################################################
# BRING IN DATA, MAKE DATAFRAME

# only do this if you haven't already made a csv
alreadybuiltcsv <- "y"

if(alreadybuiltcsv=="n") {
      
      # then build the csv from the various excel files from Whendee
      print("building csv from the excel files from Whendee") 
      
      # where excel file is
      pathfile = "~/Documents/GITHUB/cso039code_O2Bayes/Data/"
      
      ## texture
      
      # bring in excel data
      data <- read.xlsx(paste(pathfile,"PR_Texture_ElVerde_060815.xlsx",sep=""),"Sheet1")
      data2 <- as.data.frame(data, stringsAsFactors=FALSE)
      
      # get only the useful parts
      data2 <- data2[,1:5]
      data3 <- data2[c(3:12, 14:21),]
      
      # rename cols
      newnames <- c("SampleID","DateAnalyzed","%sand","%silt","%clay")
      names(data3) <- newnames
      
      # fix dates
      data3$DateAnalyzed <- as.character(data3$DateAnalyzed)
      data3$DateAnalyzed[grep("42165", data3$DateAnalyzed)] <- "6/10/15"
      data3$DateAnalyzed[grep("42177", data3$DateAnalyzed)] <- "6/22/15"
      
      # add useful columns
      data3$Depth <- -9999
      data3$Depth[grep("0-15", data3$SampleID)] <- "0-15"
      data3$Depth[grep("0-10", data3$SampleID)] <- "0-10"
      data3$Depth[grep("15-30", data3$SampleID)] <- "15-30"      
      data3$TopoType <- -9999
      data3$TopoType[grep("R", data3$SampleID)] <- "Ridge"
      data3$TopoType[grep("S", data3$SampleID)] <- "Slope"
      data3$TopoType[grep("V", data3$SampleID)] <- "Valley"
      data3$DateMeasured <- "April 2015"
      data3$Drought <- "Pre-drought"
      ##### is the number in the SampleID column the transect???? makes sense as 1, 3, 5 are the only choices
      data3$Transect <- as.numeric(substr(data3$SampleID, 2, 2))
      
      # Slope to mid-slope
      data3$TopoType <- as.character(data3$TopoType)
      data3$TopoType[grep("Slope", data3$TopoType)] <- "Mid-slope"
      
      # cull to only useful stuff
      texturedf <- data3[,2:9]
      
      ## pH (pre-drought)
      
      # bring in excel data
      data <- read.xlsx(paste(pathfile,"Fe and pH data.xlsx",sep=""),"Sheet2")
      data <- as.data.frame(data, stringsAsFactors=FALSE)
      data$DateMeasured <- "April 2015"
      data$Drought <- "Pre-drought"
      
      # Slope to mid-slope
      data$TopoType <- as.character(data$TopoType)
      data$TopoType[grep("Slope", data$TopoType)] <- "Mid-slope"
      
      # rename to useful thing
      pHFe1df <- data
      
      ## pH (post-drought)
      
      # bring in excel data
      data <- read.xlsx(paste(pathfile,"070715_pH_PuertoRico 2015 drought.xlsx",sep=""),"Sheet1")
      data2 <- as.data.frame(data, stringsAsFactors=FALSE)
      
      # get only the useful parts
      data3 <- data2[,c(2:4, 7)]
      data3$DateMeasured <- "July 2015"
      data3$Drought <- "Post-drought"
      
      # rename cols
      newnames <- c("TopoType","Rep","Depth","pH", "DateMeasured")
      names(data3) <- newnames
      
      # rename to useful thing
      pH2df <- data3
      
      ## P (post-drought)
      
      # bring in excel data
      data <- read.xlsx(paste(pathfile,"Drought P & Fe preliminary data.xlsx",sep=""),"Phosphorus Summary")
      data2 <- as.data.frame(data, stringsAsFactors=FALSE)
      
      # get only the useful parts
      data3 <- data2[,c(1:9)]
      
      # add useful columns
      data3$DateMeasured <- "July 2015"
      data3$Drought <- "Post-drought"
      data3$TopoType <- -9999
      data3$TopoType[grep("R", data3$Sample)] <- "Ridge"
      data3$TopoType[grep("Mid", data3$Sample)] <- "Mid-slope"
      data3$TopoType[grep("Low", data3$Sample)] <- "Low-slope"
      data3$TopoType[grep("V", data3$Sample)] <- "Valley"
      data3$Rep <- -9999
      data3$Sample <- as.character(data3$Sample)
      data3$Rep <- as.numeric(str_sub(data3$Sample,-1,-1))
      
      # rename cols
      newnames <- c("Sample","Depth","BicarbPi","BicarbPt","BicarbPo","NaOHPi","NaOHPt","NaOHPo","TotalP","TopoType","Rep")
      names(data3) <- newnames
      
      # rename as useful thing
      P2df <- data3
      
      ## Fe (post-drought)
      
      # bring in excel data
      data <- read.xlsx(paste(pathfile,"Drought P & Fe preliminary data.xlsx",sep=""),"Citrate-ascorbate Fe Summary")
      data2 <- as.data.frame(data, stringsAsFactors=FALSE)
      
      # get only the useful parts
      data3 <- data2[,c(1:4)]
      
      # add useful columns
      data3$DateMeasured <- "July 2015"
      data3$Drought <- "Post-drought"
      data3$TopoType <- -9999
      data3$TopoType[grep("R", data3$Sample)] <- "Ridge"
      data3$TopoType[grep("Mid", data3$Sample)] <- "Mid-slope"
      data3$TopoType[grep("Low", data3$Sample)] <- "Low-slope"
      data3$TopoType[grep("V", data3$Sample)] <- "Valley"
      data3$Rep <- -9999
      data3$Sample <- as.character(data3$Sample)
      data3$Rep <- as.numeric(str_sub(data3$Sample,-1,-1))
      
      # rename cols
      newnames <- c("Sample","Depth","Fe_ppm_citrate_ascorbate","Fe_mg_g_citrate_ascorbate","DateMeasured","Drought","TopoType","Rep")
      names(data3) <- newnames
      
      # rename as useful thing
      Fe2df <- data3
      
      ## save as csv files
      write.csv(texturedf, file=paste(pathsavefiles, "texturedf_field.csv", sep = ""), row.names=FALSE)  
      write.csv(pHFe1df, file=paste(pathsavefiles, "pHFepredroughtdf_field.csv", sep = ""), row.names=FALSE)  
      write.csv(pH2df, file=paste(pathsavefiles, "pHpostdroughtdf_field.csv", sep = ""), row.names=FALSE)  
      write.csv(P2df, file=paste(pathsavefiles, "Ppostdroughtdf_field.csv", sep = ""), row.names=FALSE)  
      write.csv(Fe2df, file=paste(pathsavefiles, "Fepostdroughtdf_field.csv", sep = ""), row.names=FALSE)  
      
} else {
      
      # if the csv has been built previously, then just load it
      print("csv built previously; loading it now")
      
      # load csv
      texturedf <- read.csv(paste(pathsavefiles, "texturedf_field.csv", sep = ""), stringsAsFactors=FALSE)
      pHFe1df <- read.csv(paste(pathsavefiles, "pHFepredroughtdf_field.csv", sep = ""), stringsAsFactors=FALSE)
      pH2df <- read.csv(paste(pathsavefiles, "pHpostdroughtdf_field.csv", sep = ""), stringsAsFactors=FALSE)
      P2df <- read.csv(paste(pathsavefiles, "Ppostdroughtdf_field.csv", sep = ""), stringsAsFactors=FALSE)
      Fe2df <- read.csv(paste(pathsavefiles, "Fepostdroughtdf_field.csv", sep = ""), stringsAsFactors=FALSE)
      
}





# 
# 
# # make factors where needed
# O2sensordf$Transect <- as.factor(O2sensordf$Transect)
# O2sensordf$TopoLocation <- as.factor(O2sensordf$TopoLocation)
# 
# 
# ########################################################################
# # ELIMINATE UNACCEPTABLE DATA
# 
# # am not going to do this for now; let's keep the data as inclusive as possible
# 
# 
# 
# ########################################################################
# # SUMMARY STATS: O2, MOISTURE ACROSS TRANSECTS AT EACH DATE
# 
# # summarySE using plyr
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
# 
# # summarySE O2
# summarytab1 <- summarySE(data=O2sensordf, measurevar="O2", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
# # summarySE moisture
# summarytab2 <- summarySE(data=O2sensordf, measurevar="SoilMoisture", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
# 
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
# p1 <- ggplot(summarytab1, aes(x=Date, y=meanO2, color=TopoLocation)) + geom_point() + geom_line() + geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) + ylab("Soil O2 (Mean Fraction +/- Standard Error)") 
# 
# # moisture by date (mean and se)
# p2 <- ggplot(summarytab2, aes(x=Date, y=meanSoilMoisture, color=TopoLocation)) + geom_point() + geom_line() + geom_errorbar(aes(ymin=meanSoilMoisture-seSoilMoisture, ymax=meanSoilMoisture+seSoilMoisture), alpha=0.5) + ylab("Soil Moisture (Mean Fraction +/- Standard Error)") 
# 
# # save figures
# png(file = paste(pathsavefigures, "time_series_O2.png", sep=""),width=8,height=7,units="in",res=400)
# p1
# dev.off()
# 
# # save figures
# png(file = paste(pathsavefigures, "time_series_moisture.png", sep=""),width=8,height=7,units="in",res=400)
# p2
# dev.off()
# 
# 
# 
# ########################################################################
# # EXPLORATORY FIGURES: O2 / MOISTURE RELATIONSHIP
# 
# # O2 by date (mean and se)
# p3 <- ggplot(O2sensordf, aes(x=SoilMoisture, y=O2, color=TopoLocation)) + geom_point(shape=1, alpha=0.5) + facet_wrap( ~ Drought, nrow=2) + xlab("Soil Moisture (Fraction)") + ylab("Soil O2 (Fraction)")
# 
# # loess smooth figure
# eq <- substitute(italic(Trend)~~italic(lines):~~italic(local)~~italic(polynomial)~~italic(fit))
# eqstr <- as.character(as.expression(eq))
# p4 <- p3 + geom_smooth(size = 1) + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3)
# 
# # linear fit figure
# eq <- substitute(italic(Trend)~~italic(lines):~~italic(linear)~~italic(fit))
# eqstr <- as.character(as.expression(eq))
# p5 <- p3 + geom_smooth(size = 1, method="lm") + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3)
# 
# # save figures
# png(file = paste(pathsavefigures, "correlation_O2moisture_loess.png", sep=""),width=6,height=9,units="in",res=400)
# p4
# dev.off()
# 
# # save figures
# png(file = paste(pathsavefigures, "correlation_O2moisture_lm.png", sep=""),width=6,height=9,units="in",res=400)
# p5
# dev.off()
# 
# 
# 
# ########################################################################
# # NOTES, TO DO AND TESTING
# 
# 
# ### put in line denoting day 150 (when the drought starts)
# 
# ### replace 1 and 7 in the legend with ridge and valley respectively
# # scale_fill_manual(values = topocolors, name="Landscape\nPosition", breaks=topobreaks, labels=topolabs) # why isn't this working?
# 
# ### O2 subscript on axes
# 
# 
# 
# 
