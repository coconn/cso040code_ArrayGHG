# GHGchamberdf_eosense-Figures-Rcode.R
# 
# take the cleaned csv of Eosense chamber flux data and make exploratory figures
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
library(dplyr)
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

# load csv
GHGchamberdf_eosense <- read.csv(paste(pathsavefiles, "GHGchamberdf_eosense.csv", sep = ""), stringsAsFactors=FALSE)

# fix dates
GHGchamberdf_eosense$DateTime2 <- ymd_hms(GHGchamberdf_eosense$DateTime)

# fix factors
GHGchamberdf_eosense$ChamberNumber <- as.factor(GHGchamberdf_eosense$ChamberNumber)
GHGchamberdf_eosense$TopoLocation <- as.factor(GHGchamberdf_eosense$TopoLocation)


########################################################################
# EXPLORATORY FIGURES: HISTOGRAMS

# mean fluxes for each topolocation
cdat1 <- ddply(GHGchamberdf_eosense, "TopoLocation", summarise, CO2.mean=mean(FluxCO2E_umol_per_m2_per_s, na.rm=T))

# CO2 flux histogram
h1 <- ggplot(GHGchamberdf_eosense, aes(x=FluxCO2E_umol_per_m2_per_s)) + geom_histogram(binwidth=.5, colour="black", fill="white") + facet_grid(TopoLocation ~ .) + geom_vline(data=cdat1, aes(xintercept=CO2.mean), linetype="dashed", size=1, colour="red") + theme_bw() + scale_x_continuous(limits = c(0, 50))

# mean error over fluxes for each topolocation
cdat2 <- ddply(GHGchamberdf_eosense, "TopoLocation", summarise, CO2error.mean=mean(CO2Eerrorperflux, na.rm=T))

# CO2 flux error ratio histogram
h2 <- ggplot(GHGchamberdf_eosense, aes(x=CO2Eerrorperflux)) + geom_histogram(binwidth=.005, colour="black", fill="white") + facet_grid(TopoLocation ~ .) + geom_vline(data=cdat2, aes(xintercept=CO2error.mean), linetype="dashed", size=1, colour="red") + theme_bw() + scale_x_continuous(limits = c(0, 0.50))

# save figures
png(file = paste(pathsavefigs, "CO2E_histogram.png", sep=""),width=6,height=4,units="in",res=400)
h1
dev.off()

png(file = paste(pathsavefigs, "CO2E_errorperflux_histogram.png", sep=""),width=6,height=4,units="in",res=400)
h2
dev.off()


########################################################################
# EXPLORATORY FIGURES: TIME SERIES

topocolors <- c("navy","blue","dark green","green","yellow","orange","red")
topobreaks <- c("1","2","3","4","5","6","7")
topolabs <- c("Ridge","2","3","4","5","6","Valley")

# CO2
p1 <- ggplot(GHGchamberdf_eosense, aes(x=DateTime2, y=FluxCO2E_umol_per_m2_per_s)) + geom_point(data=GHGchamberdf_eosense, aes(color=ChamberNumber, shape=TopoLocation)) + theme_bw() + ylab("FluxCO2E_umol_per_m2_per_s") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

png(file = paste(pathsavefigs, "CO2E_timeseries.png", sep=""),width=8,height=6,units="in",res=400)
p1
dev.off()

# N2O

p2 <- ggplot(GHGchamberdf_eosense, aes(x=DateTime2, y=FluxN2OE_nmol_per_m2_per_s)) + geom_point(data=GHGchamberdf_eosense, aes(color=ChamberNumber, shape=TopoLocation)) + ylab("FluxN2OE_nmol_per_m2_per_s") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

# there are some weirdly high N2O values
# GHGchamberdf_eosense$FluxN2OE_nmol_per_m2_per_s[GHGchamberdf_eosense$FluxN2OE_nmol_per_m2_per_s>3]

# CH4

p3 <- ggplot(GHGchamberdf_eosense, aes(x=DateTime2, y=FluxCH4E_nmol_per_m2_per_s)) + geom_point(data=GHGchamberdf_eosense, aes(color=ChamberNumber, shape=TopoLocation)) + ylab("FluxCH4E_nmol_per_m2_per_s") + theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("1 week"), labels = date_format("%d-%m-%y")) # + geom_line(data=GHGchamberdf, aes(group=Chamber, color=Chamber, linetype=Topo)) 

# there are some quite anomolous values, but not orders of magnitude too high
# GHGchamberdf_eosense$FluxCH4E_nmol_per_m2_per_s[GHGchamberdf_eosense$FluxCH4E_nmol_per_m2_per_s>125]
# GHGchamberdf_eosense$FluxCH4E_nmol_per_m2_per_s[GHGchamberdf_eosense$FluxCH4E_nmol_per_m2_per_s<=-20]




########################################################################
# NOTES, TO DO AND TESTING

##### bring in pre-drought (Feb and March data) - have this in the processed Leilei version (linear algorithm flux solve only), but get the raw data and then bring it through the same workflow



