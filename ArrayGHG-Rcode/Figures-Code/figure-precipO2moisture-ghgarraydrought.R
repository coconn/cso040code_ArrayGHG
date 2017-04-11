# figure-precipO2moisture-ghgarraydrought.R
# 
# make the drought paper time series figure
# do statistical work on the precip, moisture and O2 data
# there's no longer GHG stuff in this script
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# see also (update before making this figure if needed!): 
# Process-arraysensorsdf-Sensor-Excel-Data-Rcode.R is where arraysensorsdf.csv gets built
# figure-fluxes-ghgarraydrought.R - where the GHG stuff is

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
# BRING IN RAINFALL DATA, MAKE FIGURES

# load csv
rainfalldf <- read.csv(paste(pathrainfalldata, "elverde_rainfall_master.csv", sep = ""), stringsAsFactors=FALSE)

# deal with dates and year
rainfalldf$MonthDay <- substr(rainfalldf$Date, 1, nchar(rainfalldf$Date)-3)
rainfalldf$Date <- mdy(rainfalldf$Date)
rainfalldf$Year <- as.factor(rainfalldf$Year)

# subset out just the dates since 11/14/14 (first date of the sensor data)
rainfallstudypd <- subset(rainfalldf, rainfalldf$Date>"2014-11-13")
# note that there is a weird anomoly, but it seems legit: on 2014-12-16, rainfall is recorded as 147.32 mm in a day, but there is a note that says "rainstorm"
# cumulative rainfall for the jan to jan year
rainfallstudypd_365 <- subset(rainfalldf, rainfalldf$Date>"2014-12-31" & rainfalldf$Date<="2015-12-31")
rainfallstudypd_365 <- within(rainfallstudypd_365, acc_sum <- cumsum(Rainfall_mm))

# rainfall by date (study period)
p0 <- ggplot(rainfallstudypd, aes(x=Date, y=Rainfall_mm)) + geom_bar(stat = "identity") + ylab("Daily Rainfall \n(mm)") + theme_bw() + ylim(0.0,50) + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25')))

# save figure
png(file = paste(pathsavefigs, "time_series_rainfall_2015pd.png", sep=""),width=14,height=7,units="in",res=400)
p0
dev.off()

# subset out the years that you want to use for the bar graph comparining years
rainfallpast <- subset(rainfalldf, rainfalldf$Date>="2004-01-01" & rainfalldf$Date<="2015-12-31")
rainfallpast$MonthDay2 <- parse_date_time(rainfallpast$MonthDay, "md")

# what was the total precip for each of those years?
rainfallpast_totmm <-ddply(rainfallpast,.(Year),summarize,tot=sum(Rainfall_mm))
# add on mean of the 2004-13 years
mean200413 <- mean(rainfallpast_totmm$tot[1:10])
se200413 <- sd(rainfallpast_totmm$tot[1:10]) #/length(rainfallpast_totmm$tot[1:10])
rainfallpast_totmm <- rbind(rainfallpast_totmm,c(1, mean200413))
rainfallpast_totmm$Year <- as.character(rainfallpast_totmm$Year)
rainfallpast_totmm$Year[13] <- "2004-13"
rainfallpast_totmm$Year <- as.factor(rainfallpast_totmm$Year)
# order 2004-13 at the end
print(levels(rainfallpast_totmm$Year))
rainfallpast_totmm$Year <- factor(rainfallpast_totmm$Year,levels(rainfallpast_totmm$Year)[c(1, 3:13,2)])
print(levels(rainfallpast_totmm$Year))

# rainfall by date (study period)
# error bars vector
sdval <- c(rep(NA,12), se200413)
limits <- aes(ymax = tot + sdval, ymin = tot - sdval)
# ggplot
p0b <- ggplot(rainfallpast_totmm, aes(x = Year, y=tot, fill=Year)) + geom_bar(stat = "identity", color="black") + ylab("Total Precipitation (mm) \n2004-2013") + theme_bw() + theme(axis.text.x = element_text(angle = 65, hjust = 1)) + scale_y_continuous(breaks=pretty_breaks(n=9)) + theme(legend.position="none") + scale_fill_manual(values = c(rep("grey25",10), "grey25", "red3","grey85")) + geom_errorbar(position=position_dodge(width=0.9),limits, width=0.25) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

# save figure
png(file = paste(pathsavefigs, "rainfall_years_totalmm.png", sep=""),width=8,height=5,units="in",res=400)
p0b
dev.off()

# subset out the years that you want to use as the comparison for stats other than 2014-5 years
rainfallpast <- subset(rainfalldf, rainfalldf$Date>="2004-01-01" & rainfalldf$Date<="2013-12-31")
rainfallpast$MonthDay2 <- parse_date_time(rainfallpast$MonthDay, "md")

# get average of those ten years for each day
summarytabraincomp <- summarySE(data=rainfallpast, measurevar="Rainfall_mm", c("MonthDay2"), na.rm=TRUE, renameallcols=TRUE) 
# cumulative rainfall for the year
summarytabraincomp <- within(summarytabraincomp, acc_sum <- cumsum(meanRainfall_mm))

# rainfall by date (comparison time period)
p0c <- ggplot(summarytabraincomp, aes(x=MonthDay2, y=meanRainfall_mm)) + geom_bar(stat = "identity") + ylab("Daily Precipitation (mm) \n2004-2013 Mean") + theme_bw() + scale_x_datetime(labels = date_format("%b-%d")) + ylim(0.0,50) 

# save figure
png(file = paste(pathsavefigs, "time_series_rainfall_referencepd.png", sep=""),width=14,height=7,units="in",res=400)
p0c
dev.off()

# rainfall deviation by date
str(rainfallstudypd_365) # Date, Rainfall_mm
str(summarytabraincomp) # MonthDay2, meanRainfall_mm
# add leap year row to 2015 data
tmprow <- data.frame(Date = NA, Year = "2015", Julian = NA, Rainfall_mm = 0, Comments = NA, MonthDay = "2/29", acc_sum = NA)
newData <- rbind(rainfallstudypd_365[1:59,], tmprow, rainfallstudypd_365[60:dim(rainfallstudypd_365)[1],])
# new data frame
rainfalldeviation <- data.frame(rain2015 = newData$Rainfall_mm, rainrefmean = summarytabraincomp$meanRainfall_mm, Date = summarytabraincomp$MonthDay2)
rainfalldeviation$diff <- rainfalldeviation$rain2015 - rainfalldeviation$rainrefmean

# rainfall by date (comparison time period)
p0d <- ggplot(rainfalldeviation, aes(x=Date, y=diff)) + geom_bar(stat = "identity", aes(fill = diff>0), position="dodge") + ylab("Daily Precipitation Anomaly (mm) \n2015 vs. 2004-13 Mean") + theme_bw() + scale_x_datetime(labels = date_format("%b-%d")) + theme(legend.position="none") + scale_fill_manual(values=c("red3", "grey25")) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  #+ geom_vline(xintercept=as.numeric(rainfalldeviation$Date[116]), linetype=2) + geom_vline(xintercept=as.numeric(rainfalldeviation$Date[237]), linetype=2) + geom_vline(xintercept=as.numeric(rainfalldeviation$Date[329]), linetype=2) 

# save figure
png(file = paste(pathsavefigs, "time_series_rainfall_anomaly.png", sep=""),width=14,height=7,units="in",res=400)
p0d
dev.off()

# cumulative rainfall graphs onto the same plot
# get both datasets into same df

# reference data
summarytabraincomp$DateJoin <- as.character(summarytabraincomp$MonthDay2)
summarytabraincomp$DateJoin2 <- substr(summarytabraincomp$DateJoin, 6, nchar(summarytabraincomp$DateJoin))

# our study data
rainfallstudypd_365$DateJoin <- as.character(rainfallstudypd_365$Date)
rainfallstudypd_365$DateJoin2 <- substr(rainfallstudypd_365$DateJoin, 6, nchar(rainfallstudypd_365$DateJoin))
rainfallstudypd_365_tojoin <- rainfallstudypd_365[,c(4,7,9)]
names(rainfallstudypd_365_tojoin) <- c("Rainfall_mm_studypd","acc_sum_studypd","DateJoin2")

# join study onto reference
summarytabraincomp <- full_join(summarytabraincomp, rainfallstudypd_365_tojoin, by="DateJoin2")

# cumulative rainfall, reference period and study year
p0f <- ggplot(summarytabraincomp) + geom_ribbon(aes(x=MonthDay2, ymin=acc_sum-sdRainfall_mm, ymax=acc_sum+sdRainfall_mm), alpha=0.5) + geom_line(aes(x=MonthDay2, y=acc_sum)) + ylab("Cumulative Precipitation (mm) \n2004-2013 vs. 2015") + theme_bw() + scale_x_datetime(labels = date_format("%b-%d"), name="") + geom_line(aes(x=MonthDay2, y=acc_sum_studypd), linetype=3)

# save figure
png(file = paste(pathsavefigs, "cumulativerain_comparison.png", sep=""),width=8,height=5,units="in",res=400)
p0f
dev.off()
### why can't I get a legend working on this graph???


########################################################################
# RAINFALL STATS AND NUMBERS REPORTING

rainfallpast_totmm$tot[rainfallpast_totmm$Year=="2004-13"]
rainfallpast_totmm$tot[rainfallpast_totmm$Year=="2015"]
mean(rainfalldeviation$diff)
sd(rainfalldeviation$diff)
sum(rainfalldeviation$diff>0)
sum(rainfalldeviation$diff<=0)
sum(rainfalldeviation$diff>0) + sum(rainfalldeviation$diff<=0) # check you didn't double count


########################################################################
# BRING IN OXYGEN AND MOISTURE DATA, MAKE DATAFRAME

# load csv
arraysensorsdf <- read.csv(paste(pathsavefiles, "arraysensorsdf.csv", sep = ""), stringsAsFactors=FALSE)

# make factors where needed
arraysensorsdf$Transect <- as.factor(arraysensorsdf$Transect)
arraysensorsdf$TopoLocation <- as.factor(arraysensorsdf$TopoLocation)

# fix weird character O2 thing
arraysensorsdf$O2 <- as.numeric(arraysensorsdf$O2)

# get rid of any O2 values above 0.23
arraysensorsdf$O2[arraysensorsdf$O2>0.225] <- NA


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

# save these as csv for use internally (to pass to Whendee, etc.)
write.csv(summarytab1, file=paste(pathsavefiles, "arraysensorsdf_O2dailysummarystats.csv", sep = ""), row.names=FALSE)
write.csv(summarytab2, file=paste(pathsavefiles, "arraysensorsdf_moisturedailysummarystats.csv", sep = ""), row.names=FALSE)

# convert summary table dates (needed for doing analysis, but not good before writing to csv)
summarytab1$Date <- ymd(summarytab1$Date)
summarytab2$Date <- ymd(summarytab2$Date)


########################################################################
# OXYGEN AND MOISTURE STATISTICS AND NUMBERS REPORTING

# what are the breakpoints in the moisture data?
# using strucchange


#### see these websites when trying to figure out how to nest the model within the segments
# https://stackoverflow.com/questions/22313728/interpretation-of-piecewise-mixed-effects-output
# https://rpubs.com/MarkusLoew/12164
# https://climateecology.wordpress.com/2012/08/19/r-for-ecologists-putting-together-a-piecewise-regression/#comments


# FYI, this is slow, so placed within if statement
recalcbreakpoints <- 2.5
if(recalcbreakpoints < 2) {
  bp.moisture <- breakpoints(meanSoilMoisture ~ Date, data = summarytab2, h = 0.2)
  # model option info
  summary(bp.moisture)
  # how many breakpoints to pick based on RSS and BIC (low is good)
  plot(bp.moisture)
  png(file = paste(pathsavefigs, "breakpoints_moisture_BIC.png", sep=""),width=5,height=4,units="in",res=400)
  plot(bp.moisture)
  dev.off()
  # prints info associated with optimal breakpoints
  breakpoints(bp.moisture)
}

## printed info
# breakpoints(bp.moisture)
# 
# Optimal 4-segment partition: 
#   
#   Call:
#   breakpoints.breakpointsfull(obj = bp.moisture)
# 
# Breakpoints at observation number:
#   1099 1942 2588 
# 
# Corresponding to breakdates:
#   0.3398268 0.6004947 0.8002474 

bp1 <- summarytab2$Date[1099]
bp2 <- summarytab2$Date[1942]
bp3 <- summarytab2$Date[2588]

bpnum1 <- 1099
bpnum2 <- 1942
bpnum3 <- 2588

# plot with bp lines
# plot(meanSoilMoisture ~ Date, data = summarytab2)
# abline(v=bp1)
# abline(v=bp2)
# abline(v=bp3)

# fstats test
# fs.moisture <- Fstats(meanSoilMoisture ~ Date * TopoLocation, data = summarytab2, from=0.1)
# plot(fs.moisture, alpha = 0.01)
# sctest(fs.moisture)


########################################################################
# OXYGEN AND MOISTURE BAR GRAPHS BY DROUGHT TIME PERIOD

# define the four time periods
# pre-drought: before 2015-04-20
# drought: 2015-04-21 to 2015-08-20, 16 weeks
# drought recovery: 2015-08-21 to 2015-11-02, 10 weeks
# post-drought: after 2015-11-03
#### note that Christine manually updated these below based on the piecewise regression results

# assign topographic location
# have to run these in this order
summarytab1$DroughtTimePds <- "Post-Drought"
summarytab1$DroughtTimePds[summarytab1$Date < "2015-11-24"] <- "Drought Recovery"
summarytab1$DroughtTimePds[summarytab1$Date < "2015-08-24"] <- "Drought"
summarytab1$DroughtTimePds[summarytab1$Date < "2015-04-25"] <- "Pre-Drought"

summarytab2$DroughtTimePds <- "Post-Drought"
summarytab2$DroughtTimePds[summarytab2$Date < "2015-11-24"] <- "Drought Recovery"
summarytab2$DroughtTimePds[summarytab2$Date < "2015-08-24"] <- "Drought"
summarytab2$DroughtTimePds[summarytab2$Date < "2015-04-25"] <- "Pre-Drought"

arraysensorsdf$DroughtTimePds <- "Post-Drought"
arraysensorsdf$DroughtTimePds[arraysensorsdf$Date < "2015-11-24"] <- "Drought Recovery"
arraysensorsdf$DroughtTimePds[arraysensorsdf$Date < "2015-08-24"] <- "Drought"
arraysensorsdf$DroughtTimePds[arraysensorsdf$Date < "2015-04-25"] <- "Pre-Drought"

# make DroughtTimePds a factor
summarytab1$DroughtTimePds <- as.factor(summarytab1$DroughtTimePds)
summarytab2$DroughtTimePds <- as.factor(summarytab2$DroughtTimePds)
arraysensorsdf$DroughtTimePds <- as.factor(arraysensorsdf$DroughtTimePds)
# order the levels so the plot is temporally appropriate
print(levels(summarytab2$DroughtTimePds))
summarytab1$DroughtTimePds <- factor(summarytab1$DroughtTimePds,levels(summarytab1$DroughtTimePds)[c(4,1,2,3)])
summarytab2$DroughtTimePds <- factor(summarytab2$DroughtTimePds,levels(summarytab2$DroughtTimePds)[c(4,1,2,3)])
arraysensorsdf$DroughtTimePds <- factor(arraysensorsdf$DroughtTimePds,levels(arraysensorsdf$DroughtTimePds)[c(4,1,2,3)])

# get means, std, ste for each period

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE sets - moisture and O2
summarytabDroughtTimePdsO2 <- summarySE(data=summarytab1, measurevar="meanO2", c("DroughtTimePds", "TopoLocation"), na.rm=TRUE, renameallcols=TRUE)

summarytabDroughtTimePdsMoisture <- summarySE(data=summarytab2, measurevar="meanSoilMoisture", c("DroughtTimePds", "TopoLocation"), na.rm=TRUE, renameallcols=TRUE)


########################################################################
# TWO-WAY REPEATED MEASURES ANOVA BY DROUGHT PERIOD, TOPO: O2, MOISTURE

# read these on lme() for two-way repeated measures
# http://www.researchgate.net/post/Has_anyone_performed_linear_mixed_model_with_repeated_measures
# http://www.jason-french.com/tutorials/repeatedmeasures.html

##### do I have to do interaction terms for these tests?  Because they're all significant.  Then what happens?

# Topo and DroughtTimePds are both already factors
# str(arrayGHGdf)

## O2

# lme can't handle columns with any NAs
summarytabDroughtTimePdsO2_noNA <- subset(summarytabDroughtTimePdsO2, !is.na(meanmeanO2))

# should log transform based on the diagnostic plots of doing this without transformation

# aov version of the two-way test
aov_twoway_droughttopo_O2 = aov(meanmeanO2 ~ DroughtTimePds + TopoLocation, data=summarytabDroughtTimePdsO2_noNA)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_twoway_droughttopo_O2)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/O2_twowayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(aov_twoway_droughttopo_O2,col=summarytabDroughtTimePdsO2_noNA$DroughtTimePds, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "ANOVAdiagnostics/O2_fluxuse_twowayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(aov_twoway_droughttopo_O2),sqrt(abs(resid(aov_twoway_droughttopo_O2))))
dev.off()

# save anova info

# post hoc test
posthoc_glht_1 <- summary(glht(aov_twoway_droughttopo_O2, linfct=mcp(DroughtTimePds = "Tukey")), test = adjusted(type = "bonferroni"))
posthoc_glht_2 <- summary(glht(aov_twoway_droughttopo_O2, linfct=mcp(TopoLocation = "Tukey")), test = adjusted(type = "bonferroni"))

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
write.xlsx(anova(aov_twoway_droughttopo_O2), file=paste(path.expand(pathsavetab), "stats-tables/O2_twowayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests_1, file=paste(path.expand(pathsavetab), "stats-tables/O2_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #1", append=TRUE)
write.xlsx(mtests_2, file=paste(path.expand(pathsavetab), "stats-tables/O2_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #2", append=TRUE)

# set significance stars for graphs
cor_stars <- numeric(length=2)
# cycle through to set number of stars
for (i in 1:2) {
  
  if(anova(aov_twoway_droughttopo_O2)[i,5] < 0.001){
    cor_stars[i] <- "***"
  } else if(anova(aov_twoway_droughttopo_O2)[i,5] < 0.01){
    cor_stars[i] <- "**"
  } else if(anova(aov_twoway_droughttopo_O2)[i,5] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_O2twoway <- cor_stars

## moisture

# lme can't handle columns with any NAs
summarytabDroughtTimePdsMoisture_noNA <- subset(summarytabDroughtTimePdsMoisture, !is.na(meanmeanSoilMoisture))

# should log transform based on the diagnostic plots of doing this without transformation

# aov version of the two-way test
aov_twoway_droughttopo_moisture = aov(meanmeanSoilMoisture ~ DroughtTimePds + TopoLocation, data=summarytabDroughtTimePdsMoisture_noNA)

# summary info; note different F values (apparently this is a known consequence)
summary(aov_twoway_droughttopo_moisture)

# save diagnostic plots

# fitted vs residuals
png(file = paste(pathsavetab, "ANOVAdiagnostics/moisture_twowayANOVAdiagnostics_1.png", sep=""),width=6,height=6,units="in",res=150)
plot(aov_twoway_droughttopo_moisture,col=summarytabDroughtTimePdsMoisture_noNA$DroughtTimePds, main="Diagnostic Trellis Plot")
dev.off()

# fitted vs sqrt(abs(residuals))
png(file = paste(pathsavetab, "ANOVAdiagnostics/moisture_fluxuse_twowayANOVAdiagnostics_2.png", sep=""),width=6,height=6,units="in",res=150)
scatter.smooth(fitted(aov_twoway_droughttopo_moisture),sqrt(abs(resid(aov_twoway_droughttopo_moisture))))
dev.off()

# save anova info

# post hoc test
posthoc_glht_1 <- summary(glht(aov_twoway_droughttopo_moisture, linfct=mcp(DroughtTimePds = "Tukey")), test = adjusted(type = "bonferroni"))
posthoc_glht_2 <- summary(glht(aov_twoway_droughttopo_moisture, linfct=mcp(TopoLocation = "Tukey")), test = adjusted(type = "bonferroni"))

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
write.xlsx(anova(aov_twoway_droughttopo_moisture), file=paste(path.expand(pathsavetab), "stats-tables/moisture_twowayANOVA.xlsx",sep=""), sheetName="lme() table")
# post-hoc tukey with bonferroni
write.xlsx(mtests_1, file=paste(path.expand(pathsavetab), "stats-tables/moisture_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #1", append=TRUE)
write.xlsx(mtests_2, file=paste(path.expand(pathsavetab), "stats-tables/moisture_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey with bonf. IV #2", append=TRUE)

# set significance stars for graphs
cor_stars <- numeric(length=2)
# cycle through to set number of stars
for (i in 1:2) {
  
  if(anova(aov_twoway_droughttopo_moisture)[i,5] < 0.001){
    cor_stars[i] <- "***"
  } else if(anova(aov_twoway_droughttopo_moisture)[i,5] < 0.01){
    cor_stars[i] <- "**"
  } else if(anova(aov_twoway_droughttopo_moisture)[i,5] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_moisturetwoway <- cor_stars


########################################################################
# OXYGEN AND MOISTURE CORRELATIONS WITH DATE

##### THIS IS FUCKED UP BECAUSE OF THE DATE TO NUMERIC ISSUE

# valley
tmp <- summarytab2[summarytab2$DroughtTimePds=="Drought Recovery" & summarytab2$TopoLocation==c(7),]
with(tmp, cor(x=as.numeric(Date), y=meanSoilMoisture))

# ridge
tmp <- summarytab2[summarytab2$DroughtTimePds=="Drought Recovery" & summarytab2$TopoLocation==c(1),]
with(tmp, cor(x=as.numeric(Date), y=meanSoilMoisture))

# all slope
tmp <- summarytab2[summarytab2$DroughtTimePds=="Drought Recovery" & summarytab2$TopoLocation!=c(1,7),]
with(tmp, cor(x=as.numeric(Date), y=meanSoilMoisture))


# what happened to the O2 values before and after drought

# valley
tmp <- summarytab1[summarytab1$DroughtTimePds=="Drought" & summarytab1$TopoLocation==c(6),]
mean(tmp$meanO2)

# valley, drought avg 15.3%, pre-drought 3.7%
# topo #6, drought avg 17.5%, pre-drought 12.3%
# ridge, drought avg 19.1%, pre-drought 18.5%

tmp <- summarytab1[summarytab1$DroughtTimePds=="Post-Drought" | summarytab1$DroughtTimePds=="Pre-Drought",]
tmp2 <- tmp[tmp$TopoLocation!=c(7),]
#mean(tmp2$meanO2)
#sd(tmp2$meanO2)
summary(aov(meanO2 ~ DroughtTimePds, data=tmp2))
t.test(meanO2 ~ DroughtTimePds, data=tmp2)

aov_twoway_droughttopo = aov(log(CO2_umolm2s_fluxuse) ~ DroughtTimePds + Topo + Error(Date/(DroughtTimePds + Topo)), data=arrayGHGdf_noNA)



########################################################################
# OXYGEN AND MOISTURE TIME SERIES

# save summarytab2 as a csv since that's the final version you're using in the paper
# write.csv(summarytab2, file=paste(pathsavefiles, "arraysensorsdf_moisturedailysummarystats_finalpaperdataset_4-11-2017.csv", sep = ""), row.names=FALSE)

# many color options christine was looking into when switching to a defined color map and not ggplot default
topocolors <- rev(colorRampPalette(c('red','orange','purple','blue','dark blue'), space = "Lab")(7))
#topocolors <- heat_hcl(7, h=c(0,-100), l=c(75,40), c=c(40,80), power=1)
#topocolors <- diverge_hcl(7, l=c(50,90), c=100, power=1)
#topocolors <- diverge_hcl(7, h=c(255,330), l=c(40,90))
#topocolors <- rev(colorRampPalette(c('orange','red','purple','blue'), space = "Lab")(7))
#topocolors <- c("#d73027","#fc8d59","#fee090","#ffffbf","#e6f598","#99d594","#4575b4")

topobreaks <- c("1","2","3","4","5","6","7")
topolabs <- c("1 (Ridge)","2","3","4","5","6","7 (Valley)")

# O2 by date (mean and se)
p1 <- ggplot(summarytab1, aes(x=Date, y=meanO2, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) + ylab("Soil O2 \n(Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(values=topocolors, name="Topographic\nLocation", labels=topolabs) + geom_vline(xintercept=as.numeric(summarytab2$Date[bpnum1]), linetype=2) + geom_vline(xintercept=as.numeric(summarytab2$Date[bpnum2]), linetype=2) + geom_vline(xintercept=as.numeric(summarytab2$Date[bpnum3]), linetype=2) 
#+ scale_colour_discrete(name="Topographic\nLocation", labels=topolabs)

# moisture by date (mean and se)
p2 <- ggplot(summarytab2, aes(x=Date, y=meanSoilMoisture, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanSoilMoisture-seSoilMoisture, ymax=meanSoilMoisture+seSoilMoisture), alpha=0.5) + ylab("Soil Moisture \n(Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%Y-%m-%d"), limits = ymd(c('2014-11-01','2016-02-25'))) + scale_colour_manual(values=topocolors, name="Topographic\nLocation", labels=topolabs) + geom_vline(xintercept=as.numeric(summarytab2$Date[bpnum1]), linetype=2) + geom_vline(xintercept=as.numeric(summarytab2$Date[bpnum2]), linetype=2) + geom_vline(xintercept=as.numeric(summarytab2$Date[bpnum3]), linetype=2) #+ scale_colour_discrete(name="Topographic\nLocation", labels=topolabs) + ylim(0.0,NA) #+ geom_line()


# save figures
png(file = paste(pathsavefigs, "time_series_O2.png", sep=""),width=14,height=7,units="in",res=400)
p1
dev.off()

# save figures
png(file = paste(pathsavefigs, "time_series_moisture.png", sep=""),width=14,height=7,units="in",res=400)
p2
dev.off()

# save figures
png(file = paste(pathsavefigs, "time_series_moistureO2panels.png", sep=""),width=14,height=12,units="in",res=400)
grid.arrange(p1, p2, nrow = 2, ncol = 1)
dev.off()

# save figures
png(file = paste(pathsavefigs, "time_series_moistureO2rainpanels.png", sep=""),width=10,height=14,units="in",res=400)
grid.arrange(p0d, p2 + theme(legend.position="none"), p1 + theme(legend.position="bottom"), nrow = 3, ncol = 1)
dev.off()


########################################################################
# OXYGEN AND MOISTURE CORRELATIONS

# correlation scatter plot
p3 <- ggplot(arraysensorsdf, aes(x=SoilMoisture, y=O2, color=TopoLocation)) + geom_point(shape=1, alpha=0.5) + facet_wrap( ~ DroughtTimePds, nrow=4) + xlab("Soil Moisture (Fraction)") + ylab("Soil O2 (Fraction)") + scale_colour_manual(values=topocolors, name="Topographic\nLocation", labels=topolabs)

# linear fit figure
eq <- substitute(italic(Trend)~~italic(lines):~~italic(linear)~~italic(fit))
eqstr <- as.character(as.expression(eq))
p5 <- p3 + geom_smooth(size = 1, method="lm") + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3) + theme_bw()  + scale_colour_manual(values=topocolors, name="Topographic\nLocation", labels=topolabs)

# save figures
png(file = paste(pathsavefigs, "correlation_O2moisture_lm.png", sep=""),width=6,height=9,units="in",res=400)
p5
dev.off()

# # loess smooth figure
# eq <- substitute(italic(Trend)~~italic(lines):~~italic(local)~~italic(polynomial)~~italic(fit))
# eqstr <- as.character(as.expression(eq))
# p4 <- p3 + geom_smooth(size = 1) + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3) + scale_colour_manual(values=topocolors, name="Topographic\nLocation", labels=topolabs)

# save figures
#png(file = paste(pathsavefigures, "correlation_O2moisture_loess.png", sep=""),width=6,height=9,units="in",res=400)
#p4
#dev.off()



#############################################################################
# NOTES AND TO DOS

# everything below this is random code that I might want to reference later
# mostly for printing stats and correlations onto figures


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







