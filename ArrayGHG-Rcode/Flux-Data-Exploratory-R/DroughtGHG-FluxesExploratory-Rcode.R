# DroughtGHG-FluxesExploratory-Rcode.R
# 
# taking Leilei flux data and making data exploration figures
# 
# Drought GHG project
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
pathsavefiles = "~/Documents/GITHUB/cso040code_DroughtGHG/Flux-Data-Exploratory-R/"
pathsavefigures = "~/Documents/GITHUB/cso040code_DroughtGHG/Flux-Data-Exploratory-R/Flux-Data-Exploratory-Figures/"


########################################################################
# BRING IN DATA, MAKE DATAFRAME

# only do this if you haven't already made a csv
alreadybuiltcsv <- "y"

if(alreadybuiltcsv=="n") {
      
      # then build the csv from the excel file from Leilei
      print("building csv from the excel file from Leilei") 
      
      # where excel file is
      pathfile = "~/Documents/GITHUB/cso040code_DroughtGHG/Chamber data/Analyzed data from Leilei/"
      
      # bring in excel data
      data <- read.xlsx(paste(pathfile,"chamber_data.xlsx",sep=""),"chamber_data 141.46 date")
      data2 <- as.data.frame(data, stringsAsFactors=FALSE)
      
      # rename cols
      newnames <- c("Date","Chamber","CO2_lin_kg/ha/day","CO2_expon_kg/ha/day","CH4_lin_g/ha/day","CH4_expon_g/ha/day","N2O_lin_g/ha/day","N2O_expon_g/ha/day")
      names(data2) <- newnames
      
      # fix dates
      # not needed, since initial read is POSIXct; str(data2$Date)
      #data2$Date <- as.Date(data2$Date)
      #data2$Date2 <- ymd(data2$Date)
      
      # add useful post- and pre-drought column
      # this dataset is currently all post drought
      #data2$Drought <- -9999
      #data2$Drought[data2$DayCount <= ymd("2015-04-01")] <- "Pre-drought"
      #data2$Drought[data2$DayCount > ymd("2015-04-01")] <- "Post-drought"
      
      # clarify which chambers are on which part of the slope
      #Chamber 1,4 and 7 stay on the ridge, 2, 5 and 8 are on the slope, while 3, 6 and 9 are on the valley
      data2$Topo <- -9999
      data2$Topo[data2$Chamber == 1 | data2$Chamber == 4 | data2$Chamber == 7] <- "Ridge"
      data2$Topo[data2$Chamber == 2 | data2$Chamber == 5 | data2$Chamber == 8] <- "Slope"
      data2$Topo[data2$Chamber == 3 | data2$Chamber == 6 | data2$Chamber == 9] <- "Valley"
      
      # bring in pre-drought excel data
      data3 <- read.xlsx(paste(pathfile,"flux_data_in_Feb_and_March.xlsx",sep=""),"predrought_GHG_long")
      data4 <- as.data.frame(data3, stringsAsFactors=FALSE)
      
      # fix dates
      dec_days_passed <- data4$JulianDay
      origin <- ymd_hms("2015-01-01 00:00:00")
      # NB: the origin is treated as a date and time here, since the decimal days also hold information about time
      data4$Date <- origin + dec_days_passed * 3600 * 24
      
      # get only useful columns
      data5 <- data4[,c(3:8)]
      # rename
      newnames <- c("Chamber","CO2_lin_kg/ha/day","CH4_lin_g/ha/day","N2O_lin_g/ha/day","Topo","Date")
      names(data5) <- newnames
      
      # combine datasets
      data7 <- rbind.fill(data2,data5)
      
      # save as csv
      GHGchamberdf <- data7
      #GHGchamberdf <- data2
      write.csv(GHGchamberdf, file=paste(pathsavefiles, "GHGchamberdf.csv", sep = ""), row.names=FALSE)  
      
} else {
      
      # if the csv has been built previously, then just load it
      print("csv built previously; loading it now")
      
      # load csv
      GHGchamberdf <- read.csv(paste(pathsavefiles, "GHGchamberdf.csv", sep = ""), stringsAsFactors=FALSE)
      
}


# make factors where needed
GHGchamberdf$Chamber <- as.factor(GHGchamberdf$Chamber)
GHGchamberdf$Topo <- as.factor(GHGchamberdf$Topo)
# make sure dates work
GHGchamberdf$Date <- strptime(GHGchamberdf$Date, format="%Y-%m-%d %H:%M:%S")



########################################################################
# ELIMINATE UNACCEPTABLE DATA

# am not going to do this for now; let's keep the data as inclusive as possible





########################################################################
# EXPLORATORY FIGURES: TIME SERIES

# CO2 by date
p1 <- ggplot(GHGchamberdf, aes(x=Date, y=CO2_lin_kg.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("CO2 Flux (kg ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

p2 <- ggplot(GHGchamberdf, aes(x=Date, y=CO2_lin_kg.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("CO2 Flux (kg ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) + facet_grid(Topo ~ .) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo))

# CH4 by date
p3 <- ggplot(GHGchamberdf, aes(x=Date, y=CH4_lin_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("CH4 Flux (g ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

p4 <- ggplot(GHGchamberdf, aes(x=Date, y=CH4_lin_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("CH4 Flux (g ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) + facet_grid(Topo ~ .) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

# N2O by date
p5 <- ggplot(GHGchamberdf, aes(x=Date, y=N2O_lin_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("N2O Flux (g ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

p6 <- ggplot(GHGchamberdf, aes(x=Date, y=N2O_lin_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("N2O Flux (g ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) + facet_grid(Topo ~ .) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

# N2O but with tighter ymin and ymax
p7 <- ggplot(GHGchamberdf, aes(x=Date, y=N2O_lin_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("N2O Flux (g ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) + ylim(-6, 6) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

p8 <- ggplot(GHGchamberdf, aes(x=Date, y=N2O_lin_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("N2O Flux (g ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) + facet_grid(Topo ~ .) + ylim(-6, 6) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

# CH4 but with tighter ymin and ymax
p9 <- ggplot(GHGchamberdf, aes(x=Date, y=CH4_lin_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("CH4 Flux (g ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) + ylim(-250, 2200) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 
# p9 cuts two outliers

p10 <- ggplot(GHGchamberdf, aes(x=Date, y=CH4_lin_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + ylab("CH4 Flux (g ha^-1 d^-1), Linear Calc") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) + facet_grid(Topo ~ .) + ylim(-250, 2200) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 
# p10 cuts two outliers

# save figures (no facets)
png(file = paste(pathsavefigures, "CO2a.png", sep=""),width=8,height=7,units="in",res=250)
p1
dev.off()

png(file = paste(pathsavefigures, "CH4a.png", sep=""),width=8,height=7,units="in",res=250)
p3
dev.off()

png(file = paste(pathsavefigures, "CH4a-ylim.png", sep=""),width=8,height=7,units="in",res=250)
p9
dev.off()

png(file = paste(pathsavefigures, "N2Oa.png", sep=""),width=8,height=7,units="in",res=250)
p5
dev.off()

png(file = paste(pathsavefigures, "N2Oa-ylim.png", sep=""),width=8,height=7,units="in",res=250)
p7
dev.off()

# save figures (facets)
png(file = paste(pathsavefigures, "CO2b.png", sep=""),width=8,height=12,units="in",res=250)
p2
dev.off()

png(file = paste(pathsavefigures, "CH4b.png", sep=""),width=8,height=12,units="in",res=250)
p4
dev.off()

png(file = paste(pathsavefigures, "CH4b-ylim.png", sep=""),width=8,height=12,units="in",res=250)
p10
dev.off()

png(file = paste(pathsavefigures, "N2Ob.png", sep=""),width=8,height=12,units="in",res=250)
p6
dev.off()

png(file = paste(pathsavefigures, "N2Ob-ylim.png", sep=""),width=8,height=12,units="in",res=250)
p8
dev.off()


########################################################################
# DATA COMPARISON: LINEAR VS. EXPONENTIAL FLUX CALCS

# CO2
p9 <- ggplot(GHGchamberdf, aes(x=CO2_lin_kg.ha.day, y=CO2_expon_kg.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) + geom_abline(intercept = 0, slope = 1)

# CH4
p10 <- ggplot(GHGchamberdf, aes(x=CH4_lin_g.ha.day, y=CH4_expon_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) + geom_abline(intercept = 0, slope = 1)

# N2O
p11 <- ggplot(GHGchamberdf, aes(x=N2O_lin_g.ha.day, y=N2O_expon_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) + geom_abline(intercept = 0, slope = 1)

p12 <- ggplot(GHGchamberdf, aes(x=N2O_lin_g.ha.day, y=N2O_expon_g.ha.day)) + geom_point(data=GHGchamberdf, aes(color=Chamber, shape=Topo)) + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) + geom_abline(intercept = 0, slope = 1) + xlim(-6,6)

# save figures
png(file = paste(pathsavefigures, "CO2-lin-expo.png", sep=""),width=8,height=7,units="in",res=250)
p9
dev.off()

png(file = paste(pathsavefigures, "CH4-lin-expo.png", sep=""),width=8,height=7,units="in",res=250)
p10
dev.off()

png(file = paste(pathsavefigures, "N2O-lin-expo.png", sep=""),width=8,height=7,units="in",res=250)
p11
dev.off()

png(file = paste(pathsavefigures, "N2O-lin-expo-xlim.png", sep=""),width=8,height=7,units="in",res=250)
p12
dev.off()




# 
# ########################################################################
# # SUMMARY STATS: O2, MOISTURE ACROSS TRANSECTS AT EACH DATE
# 
# # summarySE using plyr
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
# 
# # summarySE O2
# summarytab1 <- summarySE(data=GHGchamberdf, measurevar="O2", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
# # summarySE moisture
# summarytab2 <- summarySE(data=GHGchamberdf, measurevar="SoilMoisture", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
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
# p3 <- ggplot(GHGchamberdf, aes(x=SoilMoisture, y=O2, color=TopoLocation)) + geom_point(shape=1, alpha=0.5) + facet_wrap( ~ Drought, nrow=2) + xlab("Soil Moisture (Fraction)") + ylab("Soil O2 (Fraction)")
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


########################################################################
# NOTES, TO DO AND TESTING


### put in line denoting day 150 (when the drought starts)

### replace 1 and 7 in the legend with ridge and valley respectively
# scale_fill_manual(values = topocolors, name="Landscape\nPosition", breaks=topobreaks, labels=topolabs) # why isn't this working?

### O2 subscript on axes




