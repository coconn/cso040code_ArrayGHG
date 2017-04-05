# figure-soilvars-ghgarraydrought.R
# 
# make the drought paper soil variables bar graph figures
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# see also: 
# PR_ElVerdeArray_SoilData_Master_Combo_csv.csv is organized by hand by Christine

# output products:
# figures in folder /DroughtMSFigures/
# tables in /DroughtMSTables/


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

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")
# gen_data_aov_onlymeansdN
source("~/Documents/GITHUB/RPersonalFunctionsChristine/gen_data_aov_onlymeansdN.r")

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/DroughtMSFigures/"
pathsavetab = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/DroughtMSTables/"
pathsoildata = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Soil-data/"


########################################################################
# ORGANIZE SOIL VARIABLES

# load csv
soilvardf <- read.csv(paste(pathsoildata, "PR_ElVerdeArray_SoilData_Master_Combo_csv.csv", sep = ""), stringsAsFactors=FALSE)

# nice names
newnames <- c("Sample","Location","Replicate","Depth","DroughtTimePds","SamplingMonth","TinWgt_g","TinWetSoil_g","TinDrySoil_g","SoilMeasured_FeinHCl_g","SoilMeasured_PinNaHCO3_g","SoilMeasured_pH_g","pH_timeH2Oin","pH_timeread","pH","ReagentMeasured_NaOH","ReagentMeasured_NaHCO3","ReagentMeasured_HCl","ReagentMeasured_KClday0","ReagentMeasured_KClday7","SoilMeasured_NinKClconcentrationsday0_g","SoilMeasured_NinKCltransformationsday0_g","SoilMeasured_NinKCltransformationsday7_g","FeIIconcentration_mgFeg","FeII_IIIconcentration_mgFeg","FeIIIconcentration_mgFeg","P_i_bicarb","P_t_bicarb","P_o_bicarb","P_i_NaOH","P_t_NaOH","P_o_NaOH","P_total")
names(soilvardf) <- newnames

# make factors where needed
soilvardf$Location <- as.factor(soilvardf$Location)
soilvardf$Depth <- as.factor(soilvardf$Depth)
soilvardf$DroughtTimePds <- as.factor(soilvardf$DroughtTimePds)

# reorder time period factor to make temporally appropriate
print(levels(soilvardf$DroughtTimePds))
soilvardf$DroughtTimePds <- factor(soilvardf$DroughtTimePds,levels(soilvardf$DroughtTimePds)[c(4,2,3,1)])
print(levels(soilvardf$DroughtTimePds))

# make numeric where needed
soilvardf$FeIIconcentration_mgFeg <- as.numeric(soilvardf$FeIIconcentration_mgFeg)
soilvardf$FeII_IIIconcentration_mgFeg <- as.numeric(soilvardf$FeII_IIIconcentration_mgFeg)
# str(soilvardf)

# make variable where mid and low slope are combined
soilvardf$Topo <- NA
soilvardf$Topo[soilvardf$Location=="Ridge"] <- "Ridge"
soilvardf$Topo[soilvardf$Location=="Valley"] <- "Valley"
soilvardf$Topo[soilvardf$Location=="Slope" | soilvardf$Location=="Low-slope" | soilvardf$Location=="Mid-slope"] <- "Slope"

# get rid of places where Fe(III) is negative
soilvardf$FeIIIconcentration_mgFeg[soilvardf$FeIIIconcentration_mgFeg < 0] <- NA
soilvardf$FeIIconcentration_mgFeg[soilvardf$FeIIIconcentration_mgFeg < 0] <- NA
soilvardf$FeII_IIIconcentration_mgFeg[soilvardf$FeIIIconcentration_mgFeg < 0] <- NA

# get means, std, ste for each period

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# summarySE sets - by drought period and topographic location for each variable of interest

# pH
summarytabSoilVarspH <- summarySE(data=soilvardf, measurevar="pH", c("DroughtTimePds", "Topo","Depth"), na.rm=TRUE, renameallcols=TRUE)
# get rid of weird categories
summarytabSoilVarspH <- subset(summarytabSoilVarspH, summarytabSoilVarspH$N>0)

# Fe(II)
summarytabSoilVarsFeII <- summarySE(data=soilvardf, measurevar="FeIIconcentration_mgFeg", c("DroughtTimePds", "Topo","Depth"), na.rm=TRUE, renameallcols=TRUE)
# get rid of weird categories
summarytabSoilVarsFeII <- subset(summarytabSoilVarsFeII, summarytabSoilVarsFeII$N>0)

# Fe(III)
#soilvardf$FeIIIminusFeII <- soilvardf$FeII_IIIconcentration_mgFeg - soilvardf$FeIIconcentration_mgFeg
summarytabSoilVarsFeIII <- summarySE(data=soilvardf, measurevar="FeIIIconcentration_mgFeg", c("DroughtTimePds", "Topo","Depth"), na.rm=TRUE, renameallcols=TRUE)
# get rid of weird categories
summarytabSoilVarsFeIII <- subset(summarytabSoilVarsFeIII, summarytabSoilVarsFeIII$N>0)

# Pi
soilvardf$PiCombo <- soilvardf$P_i_bicarb + soilvardf$P_i_NaOH
summarytabSoilVarsPiCombo <- summarySE(data=soilvardf, measurevar="PiCombo", c("DroughtTimePds", "Topo","Depth"), na.rm=TRUE, renameallcols=TRUE)
# get rid of weird categories
summarytabSoilVarsPiCombo <- subset(summarytabSoilVarsPiCombo, summarytabSoilVarsPiCombo$N>0)

# Po
soilvardf$PoCombo <- soilvardf$P_o_bicarb + soilvardf$P_o_NaOH
summarytabSoilVarsPoCombo <- summarySE(data=soilvardf, measurevar="PoCombo", c("DroughtTimePds", "Topo","Depth"), na.rm=TRUE, renameallcols=TRUE)
# get rid of weird categories
summarytabSoilVarsPoCombo <- subset(summarytabSoilVarsPoCombo, summarytabSoilVarsPoCombo$N>0)

# save csvs
fname = paste(pathsavetab, "drought_periods_summarytable_topo_pH.csv" ,sep="")
write.csv(summarytabSoilVarspH, file = fname, na = "")
fname = paste(pathsavetab, "drought_periods_summarytable_topo_FeII.csv" ,sep="")
write.csv(summarytabSoilVarsFeII, file = fname, na = "")
fname = paste(pathsavetab, "drought_periods_summarytable_topo_FeIII.csv" ,sep="")
write.csv(summarytabSoilVarsFeIII, file = fname, na = "")
fname = paste(pathsavetab, "drought_periods_summarytable_topo_Pi.csv" ,sep="")
write.csv(summarytabSoilVarsPiCombo, file = fname, na = "")
fname = paste(pathsavetab, "drought_periods_summarytable_topo_Po.csv" ,sep="")
write.csv(summarytabSoilVarsPoCombo, file = fname, na = "")


########################################################################
# TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: PH

## pH, 0-15cm

# lme can't handle columns with any NAs
soilvardf_noNA <- subset(soilvardf, !is.na(pH))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15")

# aov version of the two-way test
aov_twoway_droughttopo = aov(pH ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/pH_15cm_twowayANOVAdiagnostics.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="lme() table 15cm")
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 15cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 15cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_pHtwoway15 <- cor_stars

## pH, 15-30cm

# lme can't handle columns with any NAs
soilvardf_noNA <- subset(soilvardf, !is.na(pH))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "15-30")

# aov version of the two-way test
aov_twoway_droughttopo = aov(pH ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/pH_30cm_twowayANOVAdiagnostics.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="lme() table 30cm", append=TRUE)
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 30cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/pH_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 30cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_pHtwoway30 <- cor_stars


########################################################################
# TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: FE(II)

## FeIIconcentration_mgFeg, 0-15cm

# lme can't handle columns with any NAs
soilvardf_noNA <- subset(soilvardf, !is.na(FeIIconcentration_mgFeg))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15")

# aov version of the two-way test
aov_twoway_droughttopo = aov(FeIIconcentration_mgFeg ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/FeIIconcentration_mgFeg_twowayANOVAdiagnostics_15cm.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="lme() table 15cm")
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 15cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 15cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_FeIIconcentration_mgFegtwoway15 <- cor_stars

## FeIIconcentration_mgFeg, 15-30cm

# lme can't handle columns with any NAs
soilvardf_noNA <- subset(soilvardf, !is.na(FeIIconcentration_mgFeg))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "15-30")

# aov version of the two-way test
aov_twoway_droughttopo = aov(FeIIconcentration_mgFeg ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/FeIIconcentration_mgFeg_twowayANOVAdiagnostics_30cm.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="lme() table 30cm", append=TRUE)
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 30cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/FeIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 30cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_FeIIconcentration_mgFegtwoway30 <- cor_stars


########################################################################
# TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: FE(III)

## FeII_IIIconcentration_mgFeg, 0-15cm

# lme can't handle columns with any NAs
soilvardf_noNA <- subset(soilvardf, !is.na(FeII_IIIconcentration_mgFeg))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15")

# aov version of the two-way test
aov_twoway_droughttopo = aov(FeII_IIIconcentration_mgFeg ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/FeII_IIIconcentration_mgFeg_twowayANOVAdiagnostics_15cm.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="lme() table 15cm")
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 15cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 15cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_FeII_IIIconcentration_mgFegtwoway15 <- cor_stars

## FeII_IIIconcentration_mgFeg, 15-30cm

# lme can't handle columns with any NAs
soilvardf_noNA <- subset(soilvardf, !is.na(FeII_IIIconcentration_mgFeg))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "15-30")

# aov version of the two-way test
aov_twoway_droughttopo = aov(FeII_IIIconcentration_mgFeg ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/FeII_IIIconcentration_mgFeg_twowayANOVAdiagnostics_30cm.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="lme() table 30cm", append=TRUE)
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 30cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/FeII_IIIconcentration_mgFeg_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 30cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_FeII_IIIconcentration_mgFegtwoway30 <- cor_stars


########################################################################
# TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: P_O

## PoCombo, 0-15cm

# lme can't handle columns with any NAs
soilvardf_noNA <- subset(soilvardf, !is.na(PoCombo))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15")

# aov version of the two-way test
aov_twoway_droughttopo = aov(PoCombo ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/PoCombo_15cm_twowayANOVAdiagnostics.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="lme() table, 0-15cm")
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1, 0-15cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2, 0-15cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_PoCombotwoway_15cm <- cor_stars

## PoCombo, 15-30cm

# lme can't handle columns with any NAs
soilvardf_noNA <- subset(soilvardf, !is.na(PoCombo))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "15-30")

# aov version of the two-way test
aov_twoway_droughttopo = aov(PoCombo ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/PoCombo_30cm_twowayANOVAdiagnostics.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="lme() table, 15-30cm", append=TRUE)
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1, 15-30cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/PoCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2, 15-30cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_PoCombotwoway_30cm <- cor_stars

########################################################################
# TWO-WAY REPEATED MEASURES ANOVA OF VARS BY DROUGHT PERIOD, TOPO: P_I

## PiCombo, 0-15cm

# take out NAs
soilvardf_noNA <- subset(soilvardf, !is.na(PiCombo))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15")

# aov version of the two-way test
aov_twoway_droughttopo = aov(PiCombo ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/PiCombo_twowayANOVAdiagnostics_15cm.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="lme() table 15cm")
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 15cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 15cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_PiCombotwoway15 <- cor_stars

## PiCombo, 15-30cm

# take out NAs
soilvardf_noNA <- subset(soilvardf, !is.na(PiCombo))
# take out depths that aren't 0-30 cm
#soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "0-15" | soilvardf_noNA$Depth == "15-30")
soilvardf_noNA <- subset(soilvardf_noNA, soilvardf_noNA$Depth == "15-30")

# aov version of the two-way test
aov_twoway_droughttopo = aov(PiCombo ~ DroughtTimePds*Topo, data=soilvardf_noNA)
aovsummary<-xtable(aov_twoway_droughttopo)
posthoc1 <- TukeyHSD(x=aov_twoway_droughttopo, 'Topo', conf.level=0.95)
posthoc2 <- TukeyHSD(x=aov_twoway_droughttopo, 'DroughtTimePds', conf.level=0.95)
# summary info
summary(aov_twoway_droughttopo)
posthoc1
posthoc2

# save diagnostic plots
png(file = paste(pathsavetab, "ANOVAdiagnostics/PiCombo_twowayANOVAdiagnostics_30cm.png", sep=""),width=6,height=6,units="in",res=150)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov_twoway_droughttopo) # diagnostic plots
dev.off()

# save anova info

# save tables as single excel doc
# aov() anova output
write.xlsx(aovsummary, file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="lme() table 30cm", append=TRUE)
# post-hoc tukey tests
write.xlsx(data.frame(posthoc1$Topo), file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #1 30cm", append=TRUE)
write.xlsx(data.frame(posthoc2$DroughtTimePds), file=paste(path.expand(pathsavetab), "stats-tables/PiCombo_twowayANOVA.xlsx",sep=""), sheetName="post-hoc tukey #2 30cm", append=TRUE)

# set significance stars for graphs
pvals <- summary(aov_twoway_droughttopo)[[1]][["Pr(>F)"]]
cor_stars <- numeric(length=3)
# cycle through to set number of stars
for (i in 1:3) {
  
  if(pvals[i] < 0.001){
    cor_stars[i] <- "***"
  } else if(pvals[i] < 0.01){
    cor_stars[i] <- "**"
  } else if(pvals[i] < 0.05){
    cor_stars[i] <- "*"
  } else {
    cor_stars[i] <- "NS"
  }
  
}
sig_stars_PiCombotwoway30 <- cor_stars


########################################################################
# MAKE SOIL VARIABLE BAR GRAPHS BY PRE VS POST DROUGHT

# bar graphs; bar for each DroughtTimePds

topocolorsGHG <- rev(colorRampPalette(c('red','orange','purple','blue','dark blue'), space = "Lab")(3))

topocolorsDrought <- rev(colorRampPalette(c('red','orange','purple','blue','dark blue'), space = "Lab")(2))
#topocolorsDrought <- c("grey35","grey65")
topocolorsDrought <- c("#feb24c","#f03b20", "grey")

rhosize=3

# pH
# subset so it's only 0-15 and 15-30
#summarytabSoilVarspH <- subset(summarytabSoilVarspH, summarytabSoilVarspH$Depth == "0-15" | summarytabSoilVarspH$Depth == "15-30")
summarytabSoilVarspH <- subset(summarytabSoilVarspH, summarytabSoilVarspH$Depth == "0-15")
# ggplot
figpH <- ggplot(summarytabSoilVarspH, aes(x=DroughtTimePds, y=meanpH, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("pH\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanpH-sepH, ymax=meanpH+sepH), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) #+ facet_grid(Depth ~ .)

# other grouping option
figpH2 <- ggplot(subset(summarytabSoilVarspH, summarytabSoilVarspH$DroughtTimePds=="PreDrought" | summarytabSoilVarspH$DroughtTimePds=="Drought"), aes(x=Topo, y=meanpH, fill=DroughtTimePds)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Topographic Location") + ylab("pH\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanpH-sepH, ymax=meanpH+sepH), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Drought\nTime Period", values=topocolorsDrought, labels = c("Pre-Drought", "Drought")) + coord_cartesian(ylim=c(3.5,6.25)) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_pHtwoway15[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_pHtwoway15[2]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.11, vjust=4.1, label = paste("Interaction ",sig_stars_pHtwoway15[3]), size=rhosize) # + facet_grid(Depth ~ .)

# Fe(II)
# subset so it's only 0-15 and 15-30
#summarytabSoilVarsFeII <- subset(summarytabSoilVarsFeII, summarytabSoilVarsFeII$Depth == "0-15" | summarytabSoilVarsFeII$Depth == "15-30")
summarytabSoilVarsFeII <- subset(summarytabSoilVarsFeII, summarytabSoilVarsFeII$Depth == "0-15")
# gpplot
figFeII <- ggplot(summarytabSoilVarsFeII, aes(x=DroughtTimePds, y=meanFeIIconcentration_mgFeg, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("Fe(II) (mg-Fe/g)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanFeIIconcentration_mgFeg-seFeIIconcentration_mgFeg, ymax=meanFeIIconcentration_mgFeg+seFeIIconcentration_mgFeg), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) # + facet_grid(Depth ~ .)

# other grouping option
figFeII2 <- ggplot(summarytabSoilVarsFeII, aes(x=Topo, y=meanFeIIconcentration_mgFeg, fill=DroughtTimePds)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Topographic Location") + ylab("Fe(II) (mg-Fe/g)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanFeIIconcentration_mgFeg-seFeIIconcentration_mgFeg, ymax=meanFeIIconcentration_mgFeg+seFeIIconcentration_mgFeg), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Drought\nTime Period", values=topocolorsDrought, labels = c("Pre-Drought", "Drought", "Post-Drought")) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_FeIIconcentration_mgFegtwoway15[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_FeIIconcentration_mgFegtwoway15[2]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.11, vjust=4.1, label = paste("Interaction ",sig_stars_FeIIconcentration_mgFegtwoway15[3]), size=rhosize) # + facet_grid(Depth ~ .)

# Fe(III)
# subset so it's only 0-15 and 15-30
#summarytabSoilVarsFeIII <- subset(summarytabSoilVarsFeIII, summarytabSoilVarsFeIII$Depth == "0-15" | summarytabSoilVarsFeIII$Depth == "15-30")
summarytabSoilVarsFeIII <- subset(summarytabSoilVarsFeIII, summarytabSoilVarsFeIII$Depth == "0-15")
# gpplot
figFeIII <- ggplot(summarytabSoilVarsFeIII, aes(x=DroughtTimePds, y=meanFeIIIconcentration_mgFeg, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("Fe(III) (mg-Fe/g)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanFeIIIconcentration_mgFeg-seFeIIIconcentration_mgFeg, ymax=meanFeIIIconcentration_mgFeg+seFeIIIconcentration_mgFeg), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) # + facet_grid(Depth ~ .)

# other grouping option
figFeIII2 <- ggplot(summarytabSoilVarsFeIII, aes(x=Topo, y=meanFeIIIconcentration_mgFeg, fill=DroughtTimePds)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Topographic Location") + ylab("Fe(III) (mg-Fe/g)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanFeIIIconcentration_mgFeg-seFeIIIconcentration_mgFeg, ymax=meanFeIIIconcentration_mgFeg+seFeIIIconcentration_mgFeg), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Drought\nTime Period", values=topocolorsDrought, labels = c("Pre-Drought", "Drought", "Post-Drought")) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_FeII_IIIconcentration_mgFegtwoway15[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_FeII_IIIconcentration_mgFegtwoway15[2]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.11, vjust=4.1, label = paste("Interaction ",sig_stars_FeII_IIIconcentration_mgFegtwoway15[3]), size=rhosize) # + facet_grid(Depth ~ .)

# P_i
# subset so it's only 0-15 and 15-30
#summarytabSoilVarsPiCombo <- subset(summarytabSoilVarsPiCombo, summarytabSoilVarsPiCombo$Depth == "0-15" | summarytabSoilVarsPiCombo$Depth == "15-30")
summarytabSoilVarsPiCombo <- subset(summarytabSoilVarsPiCombo, summarytabSoilVarsPiCombo$Depth == "0-15")
# gpplot
figPi <- ggplot(summarytabSoilVarsPiCombo, aes(x=DroughtTimePds, y=meanPiCombo, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("Inorganic Phosphorus (ug/g)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanPiCombo-sePiCombo, ymax=meanPiCombo+sePiCombo), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Topographic\nLocation", values=topocolorsGHG) # + facet_grid(Depth ~ .)

# other grouping option
figPi2 <- ggplot(summarytabSoilVarsPiCombo, aes(x=Topo, y=meanPiCombo, fill=DroughtTimePds)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Topographic Location") + ylab("Inorganic Phosphorus (ug/g)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanPiCombo-sePiCombo, ymax=meanPiCombo+sePiCombo), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Drought\nTime Period", values=topocolorsDrought, labels = c("Pre-Drought", "Drought")) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_PiCombotwoway15[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_PiCombotwoway15[2]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.11, vjust=4.1, label = paste("Interaction ",sig_stars_PiCombotwoway15[3]), size=rhosize) # + facet_grid(Depth ~ .)

# P_o
# subset so it's only 0-15 and 15-30
#summarytabSoilVarsPoCombo <- subset(summarytabSoilVarsPoCombo, summarytabSoilVarsPoCombo$Depth == "0-15" | summarytabSoilVarsPoCombo$Depth == "15-30")
summarytabSoilVarsPoCombo <- subset(summarytabSoilVarsPoCombo, summarytabSoilVarsPoCombo$Depth == "0-15")
# gpplot
figPo <- ggplot(summarytabSoilVarsPoCombo, aes(x=DroughtTimePds, y=meanPoCombo, fill=Topo)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Drought Time Period") + ylab("Organic Phosphorus (ug/g)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanPoCombo-sePoCombo, ymax=meanPoCombo+sePoCombo), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Drought\nTime Period", values=topocolorsGHG) # + facet_grid(Depth ~ .)

# other grouping option
figPo2 <- ggplot(summarytabSoilVarsPoCombo, aes(x=Topo, y=meanPoCombo, fill=DroughtTimePds)) + geom_bar(stat = "identity", position=position_dodge(width=0.9)) + xlab("Topographic Location") + ylab("Organic Phosphorus (ug/g)\n(Mean +/- Standard Error)") + theme_bw() + geom_errorbar(aes(ymin=meanPoCombo-sePoCombo, ymax=meanPoCombo+sePoCombo), position = position_dodge(width=0.9), width = 0.65) + scale_fill_manual(name="Drought\nTime Period", values=topocolorsDrought, labels = c("Pre-Drought", "Drought")) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=1.4, label = paste("Drought period main effect ",sig_stars_PoCombotwoway_15cm[1]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.05, vjust=2.7, label = paste("Topography main effect ",sig_stars_PoCombotwoway_15cm[2]), size=rhosize) + annotate(geom="text", x = -Inf, y = Inf, hjust=-0.11, vjust=4.1, label = paste("Interaction ",sig_stars_PoCombotwoway_15cm[3]), size=rhosize) + ylim(0,200)# + facet_grid(Depth ~ .)

## save figures

# save figures without topo categories
png(file = paste(pathsavefigs, "drought_periods_pH.png", sep=""),width=6,height=4,units="in",res=400)
figpH
dev.off()

png(file = paste(pathsavefigs, "drought_periods_FeII.png", sep=""),width=6,height=4,units="in",res=400)
figFeII
dev.off()

png(file = paste(pathsavefigs, "drought_periods_FeIII.png", sep=""),width=6,height=4,units="in",res=400)
figFeIII
dev.off()

png(file = paste(pathsavefigs, "drought_periods_Po.png", sep=""),width=6,height=4,units="in",res=400)
figPo
dev.off()

png(file = paste(pathsavefigs, "drought_periods_Pi.png", sep=""),width=6,height=4,units="in",res=400)
figPi
dev.off()

# all panels
png(file = paste(pathsavefigs, "drought_periods_soilvars.png", sep=""),width=10,height=10,units="in",res=400)
grid.arrange(figFeII + theme(legend.position="none"), figFeIII + theme(legend.position="none"), figPo + theme(legend.position="none"), figPi + theme(legend.position="bottom"), figpH + theme(legend.position="none"), ncol=2)
dev.off()

# other grouping option

# save figures without topo categories
png(file = paste(pathsavefigs, "drought_periods_pH2.png", sep=""),width=6,height=4,units="in",res=400)
figpH2
dev.off()

png(file = paste(pathsavefigs, "drought_periods_FeII2.png", sep=""),width=6,height=4,units="in",res=400)
figFeII2
dev.off()

png(file = paste(pathsavefigs, "drought_periods_FeIII2.png", sep=""),width=6,height=4,units="in",res=400)
figFeIII2
dev.off()

png(file = paste(pathsavefigs, "drought_periods_Po2.png", sep=""),width=6,height=4,units="in",res=400)
figPo2
dev.off()

png(file = paste(pathsavefigs, "drought_periods_Pi2.png", sep=""),width=6,height=4,units="in",res=400)
figPi2
dev.off()

# all panels
png(file = paste(pathsavefigs, "drought_periods_soilvars2.png", sep=""),width=10,height=10,units="in",res=400)
grid.arrange(figFeII2 + theme(legend.position="none"), figFeIII2 + theme(legend.position="none"), figPo2 + theme(legend.position="none"), figPi2 + theme(legend.position="bottom"), figpH2 + theme(legend.position="none"), ncol=2)
dev.off()



########################################################################
# NOTES, ETC.






