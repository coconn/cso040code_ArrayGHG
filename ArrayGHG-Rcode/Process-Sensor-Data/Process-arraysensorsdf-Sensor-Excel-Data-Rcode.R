# Process-arraysensorsdf-Sensor-Excel-Data-Rcode.R
# 
# processing the excel files that Ryan sends with the PR array sensor data
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# output products:
# arraysensorsdf_excel.csv


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
options(java.parameters = "-Xmx5000m") # make sure there's sufficient memory to open excel file
library(xlsx)

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/SensorFigures/"


########################################################################
# BRING IN DATA, MAKE DATAFRAME

# only do this if you haven't already made a csv
alreadybuiltcsv <- "y"

if(alreadybuiltcsv=="n") {
      
      # then build the csv from the excel file from Leilei
      print("building csv from the excel file from Ryan") 
      
      # where excel file is
      pathfile = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan-practice/Getting things together for Whendee/"
      
      # bring in excel data
      data <- read.xlsx(paste(pathfile,"Copy of Daily 11132013- 12152015 GAPS INCLUDED.xlsx",sep=""),"Sheet2", colIndex = c(1:8))
      data2 <- subset(data, !is.na(data$Date))
      data2 <- as.data.frame(data2, stringsAsFactors=FALSE)
      
      # rename cols
      newnames <- c("Date","DayCount","Transect","TopoLocation","O2","O2_pct","SoilMoisture","SoilMoisture_pct")
      names(data2) <- newnames
      
      # fix dates
      # sometimes not needed, since initial read is POSIXct; str(data2$Date)
      data2$Date <- as.Date(data2$Date)
      data2$Date <- ymd(data2$Date)
      
      # add useful post- and pre-drought column
      data2$Drought <- "Post-Drought"
      data2$Drought[data2$DayCount <= 300] <- "Drought"
      data2$Drought[data2$DayCount <= 150] <- "Pre-drought"

      # save as csv
      arraysensorsdf_precleaned <- data2
      write.csv(arraysensorsdf_precleaned, file=paste(pathsavefiles, "arraysensorsdf_precleaned.csv", sep = ""), row.names=FALSE)  
      
} else {
      
      # if the csv has been built previously, then just load it
      print("csv built previously; loading it now")
      
      # load csv
      arraysensorsdf_precleaned <- read.csv(paste(pathsavefiles, "arraysensorsdf_precleaned.csv", sep = ""), stringsAsFactors=FALSE)
      
}


# make factors where needed
arraysensorsdf_precleaned$Transect <- as.factor(arraysensorsdf_precleaned$Transect)
arraysensorsdf_precleaned$TopoLocation <- as.factor(arraysensorsdf_precleaned$TopoLocation)

# make sure that formats are ok
arraysensorsdf_precleaned$Date <- as.Date(arraysensorsdf_precleaned$Date)
arraysensorsdf_precleaned$O2 <- as.numeric(arraysensorsdf_precleaned$O2)


########################################################################
# ELIMINATE UNACCEPTABLE DATA

# sensor 21 and sensor 22 from 7/7/2015 were malfunctioning
# that's transect 3 topolocation 7, and transect 4 topolocation 1

# transect 3 topolocation 7
arraysensorsdf_precleaned$O2[arraysensorsdf_precleaned$DayCount>226 & arraysensorsdf_precleaned$DayCount<335 & arraysensorsdf_precleaned$Transect=="3" & arraysensorsdf_precleaned$TopoLocation=="7"] <- NA
arraysensorsdf_precleaned$O2_pct[arraysensorsdf_precleaned$DayCount>226 & arraysensorsdf_precleaned$DayCount<335 & arraysensorsdf_precleaned$Transect=="3" & arraysensorsdf_precleaned$TopoLocation=="7"] <- NA
# transect 4 topolocation 1
arraysensorsdf_precleaned$O2[arraysensorsdf_precleaned$DayCount>226 & arraysensorsdf_precleaned$DayCount<335 & arraysensorsdf_precleaned$Transect=="4" & arraysensorsdf_precleaned$TopoLocation=="1"] <- NA
arraysensorsdf_precleaned$O2_pct[arraysensorsdf_precleaned$DayCount>226 & arraysensorsdf_precleaned$DayCount<335 & arraysensorsdf_precleaned$Transect=="4" & arraysensorsdf_precleaned$TopoLocation=="1"] <- NA

# make sure that NAs line up between value and pct columns
# O2
arraysensorsdf_precleaned$O2_pct[is.na(arraysensorsdf_precleaned$O2)] <- NA
# soil moisture
arraysensorsdf_precleaned$SoilMoisture_pct[is.na(arraysensorsdf_precleaned$SoilMoisture)] <- NA

# are there any O2 measurements above 21?
length(arraysensorsdf_precleaned$O2[!is.na(arraysensorsdf_precleaned$O2) & arraysensorsdf_precleaned$O2>0.221])
arraysensorsdf_precleaned$O2[!is.na(arraysensorsdf_precleaned$O2) & arraysensorsdf_precleaned$O2>0.221] <- NA
arraysensorsdf_precleaned$O2_pct[!is.na(arraysensorsdf_precleaned$O2) & arraysensorsdf_precleaned$O2>0.221] <- NA

# checked this in excel to make sure everything looked normal
#arraysensorsdf_precleaned2 <- arraysensorsdf_precleaned
#write.csv(arraysensorsdf_precleaned2, file=paste(pathsavefiles, "arraysensorsdf_precleaned_lookat.csv", sep = ""), row.names=FALSE)  

# are there any moisture or O2 values below 0? no, all good!
tmp <- !is.na(arraysensorsdf_precleaned$SoilMoisture_pct)<0; sum(tmp<0)
tmp <- !is.na(arraysensorsdf_precleaned$SoilMoisture)<0; sum(tmp<0)
tmp <- !is.na(arraysensorsdf_precleaned$O2_pct)<0; sum(tmp<0)
tmp <- !is.na(arraysensorsdf_precleaned$O2)<0; sum(tmp<0)

# save as csv
arraysensorsdf <- arraysensorsdf_precleaned
write.csv(arraysensorsdf, file=paste(pathsavefiles, "arraysensorsdf.csv", sep = ""), row.names=FALSE)  




# the rest of the below ended up getting moved over to the figure making files



# 
# ########################################################################
# # SUMMARY STATS: O2, MOISTURE ACROSS TRANSECTS AT EACH DATE
# 
# # summarySE using plyr
# source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
# 
# # summarySE O2
# summarytab1tmp <- summarySE(data=arraysensorsdf, measurevar="O2", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
# # summarySE moisture
# summarytab2tmp <- summarySE(data=arraysensorsdf, measurevar="SoilMoisture", c("Date", "DayCount", "TopoLocation", "Drought"), na.rm=TRUE, renameallcols=TRUE)
# 
# # take out weird NA lines
# summarytab1 <- subset(summarytab1tmp,summarytab1tmp$N>0.5)
# summarytab2 <- subset(summarytab2tmp,summarytab2tmp$N>0.5)
# 
# # convert summary table dates
# summarytab1$Date <- ymd(summarytab1$Date)
# summarytab2$Date <- ymd(summarytab2$Date)
# 
# 
# ########################################################################
# # EXPLORATORY FIGURES: TIME SERIES
# 
# topocolors <- c("navy","blue","dark green","green","yellow","orange","red")
# topobreaks <- c("1","2","3","4","5","6","7")
# topolabs <- c("1 (Ridge)","2","3","4","5","6","7 (Valley)")
# 
# # O2 by date (mean and se)
# p1 <- ggplot(summarytab1, aes(x=Date, y=meanO2, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) + ylab("Soil O2 (Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y")) + scale_colour_discrete(name="Topographic\nLocation", labels=topolabs) #+ geom_line()
# 
# # moisture by date (mean and se)
# p2 <- ggplot(summarytab2, aes(x=Date, y=meanSoilMoisture, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanSoilMoisture-seSoilMoisture, ymax=meanSoilMoisture+seSoilMoisture), alpha=0.5) + ylab("Soil Moisture (Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y")) + scale_colour_discrete(name="Topographic\nLocation", labels=topolabs) #+ geom_line()
# 
# # moisture and O2 into panels for combo figure
# p3a <- ggplot(summarytab1, aes(x=Date, y=meanO2, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) + ylab("Soil O2 (Mean Fraction +/- Standard Error)") + theme_bw() + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y")) + scale_colour_discrete(name="Topographic\nLocation", labels=topolabs) #+ geom_line()
# 
# # moisture by date (mean and se)
# p3b <- ggplot(summarytab2, aes(x=Date, y=meanSoilMoisture, color=TopoLocation)) + geom_point() + geom_errorbar(aes(ymin=meanSoilMoisture-seSoilMoisture, ymax=meanSoilMoisture+seSoilMoisture), alpha=0.5) + ylab("Soil Moisture (Mean Fraction +/- Standard Error)") + theme_bw() + theme(legend.position = "bottom", legend.direction=("horizontal"), axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y")) + scale_colour_discrete(name="Topographic\nLocation", labels=topolabs) #+ geom_line()
# 
# 
# theme(legend.position = "none", plot.margin=unit(c(-0.5,1,0,1), "cm"), axis.title.x = element_blank())
# 
# 
# 
# # save figures
# png(file = paste(pathsavefigs, "time_series_O2.png", sep=""),width=10,height=7,units="in",res=400)
# p1
# dev.off()
# 
# # save figures
# png(file = paste(pathsavefigs, "time_series_moisture.png", sep=""),width=10,height=7,units="in",res=400)
# p2
# dev.off()
# 
# # save figures
# png(file = paste(pathsavefigs, "time_series_moistureO2panels.png", sep=""),width=10,height=9,units="in",res=400)
# grid.arrange(p1, p2, nrow = 2, ncol = 1)
# dev.off()

# 
# ########################################################################
# # EXPLORATORY FIGURES: O2 / MOISTURE RELATIONSHIP
# 
# # O2 by date (mean and se)
# p3 <- ggplot(arraysensorsdf, aes(x=SoilMoisture, y=O2, color=TopoLocation)) + geom_point(shape=1, alpha=0.5) + facet_wrap( ~ Drought, nrow=2) + xlab("Soil Moisture (Fraction)") + ylab("Soil O2 (Fraction)")
# 
# # loess smooth figure
# eq <- substitute(italic(Trend)~~italic(lines):~~italic(local)~~italic(polynomial)~~italic(fit))
# eqstr <- as.character(as.expression(eq))
# p4 <- p3 + geom_smooth(size = 1) + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3)
# 
# # linear fit figure
# eq <- substitute(italic(Trend)~~italic(lines):~~italic(linear)~~italic(fit))
# eqstr <- as.character(as.expression(eq))
# p5 <- p3 + geom_smooth(size = 1, method="lm") + annotate(geom="text", x = -Inf, y = -Inf, hjust=-0.08, vjust=-0.5, label = eqstr, parse = TRUE, size = 3) + theme_bw() + scale_colour_discrete(name="Topographic\nLocation", labels=topolabs)
# 
# # save figures
# #png(file = paste(pathsavefigures, "correlation_O2moisture_loess.png", sep=""),width=6,height=9,units="in",res=400)
# #p4
# #dev.off()
# 
# # save figures
# png(file = paste(pathsavefigs, "correlation_O2moisture_lm.png", sep=""),width=6,height=9,units="in",res=400)
# p5
# dev.off()
# 
# 



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





