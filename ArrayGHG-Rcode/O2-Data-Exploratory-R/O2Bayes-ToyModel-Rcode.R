# O2Bayes-ToyModel-Rcode.R
# 
# working on something during Bayes Workshop from NutNet
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

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso039code_O2Bayes/Exploratory O2 Analyses/"
pathsavefigures = "~/Documents/GITHUB/cso039code_O2Bayes/Exploratory O2 Analyses/Exploratory O2 Figures/"


########################################################################
# BRING IN DATA, MAKE DATAFRAME

# only do this if you haven't already made a csv
alreadybuiltcsv <- "y"

if(alreadybuiltcsv=="n") {
      
      # then build the csv from the excel file from Leilei
      print("building csv from the excel file from Leilei") 
      
      # where excel file is
      pathfile = "~/Documents/GITHUB/cso039code_O2Bayes/Data/"
      
      # bring in excel data
      data <- read.xlsx(paste(pathfile,"Daily sensor Data 11132014- 07072015.xlsx",sep=""),"Sheet2")
      data2 <- as.data.frame(data, stringsAsFactors=FALSE)
      
      # rename cols
      newnames <- c("Date","DayCount","Transect","TopoLocation","O2","O2_pct","SoilMoisture","SoilMoisture_pct")
      names(data2) <- newnames
      
      # fix dates
      # not needed, since initial read is POSIXct; str(data2$Date)
      #data2$Date <- as.Date(data2$Date)
      #data2$Date2 <- ymd(data2$Date)
      
      # add useful post- and pre-drought column
      data2$Drought <- -9999
      data2$Drought[data2$DayCount <= 150] <- "Pre-drought"
      data2$Drought[data2$DayCount > 150] <- "Post-drought"
      
      # save as csv
      O2sensordf <- data2
      write.csv(O2sensordf, file=paste(pathsavefiles, "O2sensordf_field.csv", sep = ""), row.names=FALSE)  
      
} else {
      
      # if the csv has been built previously, then just load it
      print("csv built previously; loading it now")
      
      # load csv
      O2sensordf <- read.csv(paste(pathsavefiles, "O2sensordf_field.csv", sep = ""), stringsAsFactors=FALSE)
      
}


# make factors where needed
O2sensordf$Transect <- as.factor(O2sensordf$Transect)
O2sensordf$TopoLocation <- as.factor(O2sensordf$TopoLocation)


########################################################################
# ELIMINATE UNACCEPTABLE DATA

# am not going to do this for now; let's keep the data as inclusive as possible



########################################################################
# SUMMARY STATS: O2, MOISTURE ACROSS TRANSECTS AT EACH DATE

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE O2
summarytab1 <- summarySE(data=O2sensordf, measurevar="O2", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
# summarySE moisture
summarytab2 <- summarySE(data=O2sensordf, measurevar="SoilMoisture", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)



########################################################################
# EXPLORATORY FIGURES: TIME SERIES

topocolors <- c("navy","blue","dark green","green","yellow","orange","red")
topobreaks <- c("1","2","3","4","5","6","7")
topolabs <- c("Ridge","2","3","4","5","6","Valley")

# O2 by date (mean and se)
p1 <- ggplot(summarytab1, aes(x=Date, y=meanO2, color=TopoLocation)) + geom_point() + geom_line() + geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) + ylab("Soil O2 (Mean Fraction +/- Standard Error)") 

# moisture by date (mean and se)
p2 <- ggplot(summarytab2, aes(x=Date, y=meanSoilMoisture, color=TopoLocation)) + geom_point() + geom_line() + geom_errorbar(aes(ymin=meanSoilMoisture-seSoilMoisture, ymax=meanSoilMoisture+seSoilMoisture), alpha=0.5) + ylab("Soil Moisture (Mean Fraction +/- Standard Error)") 

# save figures
png(file = paste(pathsavefigures, "time_series_O2.png", sep=""),width=8,height=7,units="in",res=400)
p1
dev.off()

# save figures
png(file = paste(pathsavefigures, "time_series_moisture.png", sep=""),width=8,height=7,units="in",res=400)
p2
dev.off()



########################################################################
# EXPLORATORY FIGURES: O2 / MOISTURE RELATIONSHIP

# O2 by date (mean and se)
p3 <- ggplot(O2sensordf, aes(x=SoilMoisture, y=O2, color=TopoLocation)) + geom_point(shape=1, alpha=0.5) + facet_wrap( ~ Drought, nrow=2) + xlab("Soil Moisture (Fraction)") + ylab("Soil O2 (Fraction)")

# loess smooth figure
eq <- substitute(italic(Trend)~~italic(lines):~~italic(local)~~italic(polynomial)~~italic(fit))
eqstr <- as.character(as.expression(eq))
p4 <- p3 + geom_smooth(size = 1) + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3)

# linear fit figure
eq <- substitute(italic(Trend)~~italic(lines):~~italic(linear)~~italic(fit))
eqstr <- as.character(as.expression(eq))
p5 <- p3 + geom_smooth(size = 1, method="lm") + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3)

# save figures
png(file = paste(pathsavefigures, "correlation_O2moisture_loess.png", sep=""),width=6,height=9,units="in",res=400)
p4
dev.off()

# save figures
png(file = paste(pathsavefigures, "correlation_O2moisture_lm.png", sep=""),width=6,height=9,units="in",res=400)
p5
dev.off()



########################################################################
# NOTES, TO DO AND TESTING


### put in line denoting day 150 (when the drought starts)

### replace 1 and 7 in the legend with ridge and valley respectively
# scale_fill_manual(values = topocolors, name="Landscape\nPosition", breaks=topobreaks, labels=topolabs) # why isn't this working?

### O2 subscript on axes




