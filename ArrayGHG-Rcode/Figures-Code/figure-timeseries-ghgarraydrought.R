# figure-timeseries-ghgarraydrought.R
# 
# make the drought paper time series figure
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# see also (update before making this figure if needed!): 
# Process-arraysensorsdf-Sensor-Excel-Data-Rcode.R is where arraysensorsdf.csv gets built

# output products:
# figures in folder /DroughtMSFigures/


########################################################################
# GET READY TO BRING IN DATA

library(ggplot2)
library(gridExtra)
library(scales)
#library(plyr)
#library(data.table)
#library(chron)
library(lubridate)
#library(lattice)
#library(reshape2)
#options(java.parameters = "-Xmx5000m") # make sure there's sufficient memory to open excel file
#library(xlsx)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/DroughtMSFigures/"
pathGHGdata = "~/Documents/GITHUB/NOT REPOS/cso040code_ArrayGHG_LargeFiles/Chamber-data-large-files/eosAnalyzeACProcessed/"


########################################################################
# BRING IN OXYGEN AND MOISTURE DATA, MAKE DATAFRAME

# load csv
arraysensorsdf <- read.csv(paste(pathsavefiles, "arraysensorsdf.csv", sep = ""), stringsAsFactors=FALSE)

# make factors where needed
arraysensorsdf$Transect <- as.factor(arraysensorsdf$Transect)
arraysensorsdf$TopoLocation <- as.factor(arraysensorsdf$TopoLocation)


########################################################################
# SUMMARY STATS: O2, MOISTURE ACROSS TRANSECTS AT EACH DATE

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE O2
summarytab1tmp <- summarySE(data=arraysensorsdf, measurevar="O2", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
# summarySE moisture
summarytab2tmp <- summarySE(data=arraysensorsdf, measurevar="SoilMoisture", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)

# take out weird NA lines
summarytab1 <- subset(summarytab1tmp,summarytab1tmp$N>0.5)
summarytab2 <- subset(summarytab2tmp,summarytab2tmp$N>0.5)

# convert summary table dates
summarytab1$Date <- ymd_hms(summarytab1$Date)
summarytab2$Date <- ymd_hms(summarytab2$Date)


########################################################################
# OXYGEN AND MOISTURE TIME SERIES

topocolors <- c("navy","blue","dark green","green","yellow","orange","red")
topobreaks <- c("1","2","3","4","5","6","7")
topolabs <- c("1 (Ridge)","2","3","4","5","6","7 (Valley)")

# O2 by date (mean and se)
p1 <- ggplot(summarytab1, aes(x=Date, y=meanO2, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) + ylab("Soil O2 \n(Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2015-05-01','2016-01-20'))) + scale_colour_discrete(name="Topographic\nLocation", labels=topolabs) #+ geom_line()

# moisture by date (mean and se)
p2 <- ggplot(summarytab2, aes(x=Date, y=meanSoilMoisture, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanSoilMoisture-seSoilMoisture, ymax=meanSoilMoisture+seSoilMoisture), alpha=0.5) + ylab("Soil Moisture \n(Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2015-05-01','2016-01-20'))) + scale_colour_discrete(name="Topographic\nLocation", labels=topolabs) #+ geom_line()


#### how to get these into panels nicely????
# 
# # moisture and O2 into panels for combo figure
# p3a <- ggplot(summarytab1, aes(x=Date, y=meanO2, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) + ylab("Soil O2 (Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y")) + scale_colour_discrete(name="Topographic\nLocation", labels=topolabs) #+ geom_line()
# 
# # moisture by date (mean and se)
# p3b <- ggplot(summarytab2, aes(x=Date, y=meanSoilMoisture, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanSoilMoisture-seSoilMoisture, ymax=meanSoilMoisture+seSoilMoisture), alpha=0.5) + ylab("Soil Moisture (Mean Fraction +/- Standard Error)") + theme_bw() + theme(legend.position = "bottom", legend.direction=("horizontal"), axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y")) + scale_colour_discrete(name="Topographic\nLocation", labels=topolabs) #+ geom_line()
# 
# theme(legend.position = "none", plot.margin=unit(c(-0.5,1,0,1), "cm"), axis.title.x = element_blank())



# save figures
png(file = paste(pathsavefigs, "time_series_O2.png", sep=""),width=10,height=7,units="in",res=400)
p1
dev.off()

# save figures
png(file = paste(pathsavefigs, "time_series_moisture.png", sep=""),width=10,height=7,units="in",res=400)
p2
dev.off()

# save figures
png(file = paste(pathsavefigs, "time_series_moistureO2panels.png", sep=""),width=10,height=9,units="in",res=400)
grid.arrange(p1, p2, nrow = 2, ncol = 1)
dev.off()


########################################################################
# BRING IN GHG DATA, MAKE DATAFRAME

# load csv
arrayGHGdf <- read.csv(paste(pathGHGdata, "ArrayGHG_master_chamber_data.csv", sep = ""), stringsAsFactors=FALSE)

# nice names
newnames <- c("DateTime","Chamber","MeasurementDuration_s","MeanCO2_ppm","MeanCH4_ppm","MeanN2O_ppm","MeanNH3_ppm","MeanH2O_percent","ChemDetect_0-1","CavPressure_kPa","CavTemperature_K","WaterContent_fraction","ChamberTemperature_K","ChamberPressure_kPa","FluxCO2_L_umolm2s","FluxCO2_E_umolm2s","FluxCH4_L_nmolm2s","FluxCH4_E_nmolm2s","FluxN2O_L_nmolm2s","FluxN2O_E_nmolm2s","FluxNH3_L_umolm2s","FluxNH3_E_umolm2s","e_FluxCO2_L_umolm2s","e_FluxCO2_E_umolm2s","e_FluxCH4_L_nmolm2s","e_FluxCH4_E_nmolm2s","e_FluxN2O_L_nmolm2s","e_FluxN2O_E_nmolm2s","e_FluxNH3_L_umolm2s","e_FluxNH3_E_umolm2s","Metadata")
names(arrayGHGdf) <- newnames

# make dates nice
arrayGHGdf$DateTime2 <- mdy_hm(arrayGHGdf$DateTime)

# rip out date only
arrayGHGdf$Date <- substr(arrayGHGdf$DateTime2,1,11)
arrayGHGdf$Date <- ymd(arrayGHGdf$Date)

# assign topographic location
arrayGHGdf$Topo <- NA
arrayGHGdf$Topo[arrayGHGdf$Chamber=="1" | arrayGHGdf$Chamber=="4" | arrayGHGdf$Chamber=="7"] <- "Ridge"
arrayGHGdf$Topo[arrayGHGdf$Chamber=="2" | arrayGHGdf$Chamber=="5" | arrayGHGdf$Chamber=="8"] <- "Slope"
arrayGHGdf$Topo[arrayGHGdf$Chamber=="3" | arrayGHGdf$Chamber=="6" | arrayGHGdf$Chamber=="9"] <- "Valley"

# make factors where needed
arrayGHGdf$Chamber <- as.factor(arrayGHGdf$Chamber)
arrayGHGdf$Topo <- as.factor(arrayGHGdf$Topo)
# str(arrayGHGdf)

# take out any negative CO2 fluxes (be sure to troubleshoot these later)
arrayGHGdf$FluxCO2_E_umolm2s[arrayGHGdf$FluxCO2_E_umolm2s<0] <- NA
arrayGHGdf$FluxCO2_L_umolm2s[arrayGHGdf$FluxCO2_L_umolm2s<0] <- NA

# take out any N2O fluxes over abs(100) nmol/m2/s
arrayGHGdf$FluxN2O_E_nmolm2s[abs(arrayGHGdf$FluxN2O_E_nmolm2s) > 100] <- NA
arrayGHGdf$FluxN2O_L_nmolm2s[abs(arrayGHGdf$FluxN2O_L_nmolm2s) > 100] <- NA

########################################################################
# SUMMARY STATS: CO2, CH4, N2O ACROSS TRANSECTS AT EACH DATE

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE CO2
summarytab3atmp <- summarySE(data=arrayGHGdf, measurevar="FluxCO2_E_umolm2s", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)

summarytab3btmp <- summarySE(data=arrayGHGdf, measurevar="FluxCO2_E_umolm2s", c("Date", "Topo"), na.rm=TRUE, renameallcols=TRUE)

summarytab3ctmp <- summarySE(data=arrayGHGdf, measurevar="FluxCO2_L_umolm2s", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)

# summarySE CH4
summarytab4atmp <- summarySE(data=arrayGHGdf, measurevar="FluxCH4_E_nmolm2s", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)

summarytab4btmp <- summarySE(data=arrayGHGdf, measurevar="FluxCH4_E_nmolm2s", c("Date", "Topo"), na.rm=TRUE, renameallcols=TRUE)

summarytab4ctmp <- summarySE(data=arrayGHGdf, measurevar="FluxCH4_L_nmolm2s", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)

# summarySE N2O
summarytab5atmp <- summarySE(data=arrayGHGdf, measurevar="FluxN2O_E_nmolm2s", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)

summarytab5btmp <- summarySE(data=arrayGHGdf, measurevar="FluxN2O_E_nmolm2s", c("Date", "Topo"), na.rm=TRUE, renameallcols=TRUE)

summarytab5ctmp <- summarySE(data=arrayGHGdf, measurevar="FluxN2O_L_nmolm2s", c("Date", "Chamber", "Topo"), na.rm=TRUE, renameallcols=TRUE)

# convert summary table dates
#summarytab3a$Date <- ymd_hms(summarytab3atmp$Date)
#summarytab2$Date <- ymd_hms(summarytab2$Date)

# take out weird NA lines
#summarytab1 <- subset(summarytab1tmp,summarytab1tmp$N>0.5)
#summarytab2 <- subset(summarytab2tmp,summarytab2tmp$N>0.5)


########################################################################
# CO2, CH4, N2O TIME SERIES

topocolors <- c("navy","blue","dark green","green","yellow","orange","red")
topobreaks <- c("1","2","3","4","5","6","7")
topolabs <- c("1 (Ridge)","2","3","4","5","6","7 (Valley)")

# CO2 by date (mean and se)
p3 <- ggplot(summarytab3atmp, aes(x=Date, y=meanFluxCO2_E_umolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxCO2_E_umolm2s-seFluxCO2_E_umolm2s, ymax=meanFluxCO2_E_umolm2s+seFluxCO2_E_umolm2s), alpha=0.5) + ylab("CO2 Flux (umol/m^2/s), Exp. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2015-05-01','2016-01-20'))) + scale_colour_discrete(name="Topographic\nLocation") #+ geom_line()

# CH4 by date (mean and se)
p4 <- ggplot(summarytab4atmp, aes(x=Date, y=meanFluxCH4_E_nmolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxCH4_E_nmolm2s-seFluxCH4_E_nmolm2s, ymax=meanFluxCH4_E_nmolm2s+seFluxCH4_E_nmolm2s), alpha=0.5) + ylab("CH4 Flux (nmol/m^2/s), Exp. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2015-05-01','2016-01-20'))) + scale_colour_discrete(name="Topographic\nLocation") #+ geom_line()

# N2O by date (mean and se)
p5 <- ggplot(summarytab5atmp, aes(x=Date, y=meanFluxN2O_E_nmolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxN2O_E_nmolm2s-seFluxN2O_E_nmolm2s, ymax=meanFluxN2O_E_nmolm2s+seFluxN2O_E_nmolm2s), alpha=0.5) + ylab("N2O Flux (nmol/m^2/s), Exp. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2015-05-01','2016-01-20'))) + scale_colour_discrete(name="Topographic\nLocation") #+ geom_line()

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

# save figures
png(file = paste(pathsavefigs, "time_series_allpanels.png", sep=""),width=10,height=22,units="in",res=400)
grid.arrange(p1, p2, p3, p4, p5, nrow = 5, ncol = 1)
dev.off()


# CO2 by date (mean and se)
p3b <- ggplot(summarytab3ctmp, aes(x=Date, y=meanFluxCO2_L_umolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxCO2_L_umolm2s-seFluxCO2_L_umolm2s, ymax=meanFluxCO2_L_umolm2s+seFluxCO2_L_umolm2s), alpha=0.5) + ylab("CO2 Flux (umol/m^2/s), Lin. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2015-05-01','2016-01-20'))) + scale_colour_discrete(name="Topographic\nLocation") #+ geom_line()

# CH4 by date (mean and se)
p4b <- ggplot(summarytab4ctmp, aes(x=Date, y=meanFluxCH4_L_nmolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxCH4_L_nmolm2s-seFluxCH4_L_nmolm2s, ymax=meanFluxCH4_L_nmolm2s+seFluxCH4_L_nmolm2s), alpha=0.5) + ylab("CH4 Flux (nmol/m^2/s), Lin. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2015-05-01','2016-01-20'))) + scale_colour_discrete(name="Topographic\nLocation") #+ geom_line()

# N2O by date (mean and se)
p5b <- ggplot(summarytab5ctmp, aes(x=Date, y=meanFluxN2O_L_nmolm2s, color=Topo)) + geom_point() + geom_errorbar(aes(ymin=meanFluxN2O_L_nmolm2s-seFluxN2O_L_nmolm2s, ymax=meanFluxN2O_L_nmolm2s+seFluxN2O_L_nmolm2s), alpha=0.5) + ylab("N2O Flux (nmol/m^2/s), Lin. Fit\n(Mean +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2015-05-01','2016-01-20'))) + scale_colour_discrete(name="Topographic\nLocation") #+ geom_line()

# save figures
png(file = paste(pathsavefigs, "time_series_allpanels_linear.png", sep=""),width=10,height=22,units="in",res=400)
grid.arrange(p1, p2, p3b, p4b, p5b, nrow = 5, ncol = 1)
dev.off()


########################################################################
# COMBINE TIME SERIES FIGS INTO ONE FIGURE








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
# 
# 
# 
# 
# # base excel sheet to start with (from before my time in the Silver lab)
# 
# 
# # list of pit data files to add onto that excel sheet
# sensordatapath = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan/"
# f_sensor = list.files(path=sensordatapath, pattern="*.dat")
# # T1 .dat files is through sensor 19 (transect 4) and T2 is the sensors after that
# 
# 





