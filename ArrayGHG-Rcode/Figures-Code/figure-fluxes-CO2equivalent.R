# figure-fluxes-CO2equivalent.R
# 
# make the nicer version of the CO2 equivalent time series graph
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# note that Christine put together this dataset by hand in excel
# see ghg-co2e-analysis.xlsx

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
library(reshape2)
#options(java.parameters = "-Xmx5000m") # make sure there's sufficient memory to open excel file
library(xlsx)
library(strucchange) # piecewise regression
library(truncnorm)

# summarySE using plyr
source("~/Documents/GITHUB/RPersonalFunctionsChristine/summarySE.r")

# where to save outputs
pathsavefiles = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Rprocessed/"
pathsavefigs = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/DroughtMSFigures/"
pathGHGdata = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/GHG-CO2e-analysis/"
pathsavetab = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Analyses/DroughtMSTables/"


########################################################################
# APPROACH 2: MONTE CARLO SIMULATION

# normal for CH4
# log-normal, chi-squared or gamma for CO2 (can't go negative with any of these 3)
# wait, but it's not that the multiplier can't go negative, it's that CO2 flux shouldn't be negative... so need to figure that out
# ended up going with a truncated normal distribution for CO2

# applies to both CO2 and CH4

lengthperiod <- c(61,121,92,52)
simulationreps <- 5000

# topolocation proportion
ridgeprop	<- 0.17
slopeprop <-	0.65
valleyprop <-	0.18


########################################################################
# CO2-e CO2 SIMULATION

## CO2e CO2 calcs

# meanCO2_umolm2s
CO2meanR <- c(1.829500922,1.508863804,1.88162955,2.772106974)
CO2meanS <- c(3.793756534,6.058448581,5.13065924,4.515184411)
CO2meanV <- c(0.572222123,1.506285204,1.360654498,2.946791535)
# sdCO2_umolm2s
CO2sdR <- c(1.467956332,1.046804732,1.636949891,2.420277315)
CO2sdS <- c(2.921048514,4.255562169,4.339260199,2.797889389)
CO2sdV <- c(0.172000807,0.752823235,0.473832261,1.025457778)

# topo wgt avg, CO2_umolm2s
CO2meanRSV <- (CO2meanR*ridgeprop)+(CO2meanS*slopeprop)+(CO2meanV*valleyprop)
CO2sdRSV <- (CO2sdR*ridgeprop)+(CO2sdS*slopeprop)+(CO2sdV*valleyprop)

# CO2_kg_per_ha_per_d
CO2e_CO2_daymeanRSV <- CO2meanRSV * 12.01 * 10000 * 86400 / 1000000000
CO2e_CO2_daysdRSV <- CO2sdRSV * 12.01 * 10000 * 86400 / 1000000000

# create a matrix of many cumulative sum steps

# observed drought periods
M1 <- replicate(simulationreps, rtruncnorm(n=lengthperiod[1], a=0, b=Inf, mean=CO2e_CO2_daymeanRSV[1], sd=CO2e_CO2_daysdRSV[1]))
M2 <- replicate(simulationreps, rtruncnorm(n=lengthperiod[2], a=0, b=Inf, mean=CO2e_CO2_daymeanRSV[2], sd=CO2e_CO2_daysdRSV[2]))
M3 <- replicate(simulationreps, rtruncnorm(n=lengthperiod[3], a=0, b=Inf, mean=CO2e_CO2_daymeanRSV[3], sd=CO2e_CO2_daysdRSV[3]))
M4 <- replicate(simulationreps, rtruncnorm(n=lengthperiod[4], a=0, b=Inf, mean=CO2e_CO2_daymeanRSV[4], sd=CO2e_CO2_daysdRSV[4]))
# baseline
M1_base <- replicate(simulationreps, rtruncnorm(n=sum(lengthperiod), a=0, b=Inf, mean=CO2e_CO2_daymeanRSV[1], sd=CO2e_CO2_daysdRSV[1]))

Mnotcs <- rbind(M1, M2, M3, M4)
M <- apply(Mnotcs, 2, cumsum)
M_base <- apply(M1_base, 2, cumsum)

# get the summary stats from that simulation
# observed
summaryMmean <- rowMeans(M)
summaryMsd <- apply(M,1,sd)
summaryM975 <- apply(M,1,quantile,probs=c(.975))
summaryM25 <- apply(M,1,quantile,probs=c(.025))
summaryM <- data.frame(summaryMmean=summaryMmean, summaryMsd=summaryMsd, summaryM975=summaryM975, summaryM25=summaryM25, day=c(1:sum(lengthperiod)))
# baseline
summaryMmean_base <- rowMeans(M_base)
summaryMsd_base <- apply(M_base,1,sd)
summaryM975_base <- apply(M_base,1,quantile,probs=c(.975))
summaryM25_base <- apply(M_base,1,quantile,probs=c(.025))
summaryM_base <- data.frame(summaryMmean_base=summaryMmean_base, summaryMsd_base=summaryMsd_base, summaryM975_base=summaryM975_base, summaryM25_base=summaryM25_base, day=c(1:sum(lengthperiod)))

# plot the summary stats, observed
pM1 <- ggplot(summaryM, aes(day)) + geom_line(aes(y = summaryMmean, colour = "summaryMmean"), size=1, color="black") + geom_errorbar(aes(ymin=summaryM25,ymax=summaryM975), alpha=0.3, color="black", width = 0) + theme_bw()

# plot the summary stats, baseline
pM2 <- ggplot(summaryM_base, aes(day)) + geom_line(aes(y = summaryMmean_base, colour = "summaryMmean"), size=1, color="black") + geom_errorbar(aes(ymin=summaryM25_base,ymax=summaryM975_base), alpha=0.3, color="black", width = 0) + theme_bw()

# plot the observed and baseline
pM3 <- ggplot(summaryM, aes(day)) + geom_line(aes(y = summaryMmean, colour = "summaryMmean"), size=1, color="black") + geom_errorbar(aes(ymin=summaryM25,ymax=summaryM975), alpha=0.3, color="black", width = 0) + theme_bw() + geom_line(data = summaryM_base, aes(y = summaryMmean_base, colour = "summaryMmean_base"), size=1, color="blue") + geom_errorbar(data = summaryM_base, aes(ymin=summaryM25_base,ymax=summaryM975_base), alpha=0.3, color="blue", width = 0)

# save the simulations
# observed
fname = paste(pathsavetab, "CO2eSimulationResults/CO2esimulationresults_CO2_observed.csv" ,sep="")
write.csv(summaryM, file = fname, na = "")
# baseline
fname = paste(pathsavetab, "CO2eSimulationResults/CO2esimulationresults_CO2_baseline.csv" ,sep="")
write.csv(summaryM_base, file = fname, na = "")


########################################################################
# CO2-e CH4 SIMULATION

# now do the same with CH4

# meanCH4_umolm2s
CH4meanR <- c(-0.63629477,-1.120537043,-0.666803886,10.1226501)
CH4meanS <- c(0.148996378,-1.933354222,-0.605964719,15.15579608)
CH4meanV <- c(17.42923214,1.677103246,1.503544041,19.2124457)
# sdCH4_umolm2s
CH4sdR <- c(0.758049582,1.002026476,0.660281571,7.842487625	)
CH4sdS <- c(0.690664063,1.725763755,0.969675935,7.025239716)
CH4sdV <- c(29.60551411,4.088934826,2.58395312,8.36574107)

# topo wgt avg, CH4_umolm2s
CH4meanRSV <- (CH4meanR*ridgeprop)+(CH4meanS*slopeprop)+(CH4meanV*valleyprop)
CH4sdRSV <- (CH4sdR*ridgeprop)+(CH4sdS*slopeprop)+(CH4sdV*valleyprop)

# CH4_kg_per_ha_per_d
CO2e_CH4_daymeanRSV <- CH4meanRSV * 12.01 * 10000 * 86400 / 1000000000 * 34
CO2e_CH4_daysdRSV <- CH4sdRSV * 12.01 * 10000 * 86400 / 1000000000 * 34

# create a matrix of many cumulative sum steps
lengthperiod <- c(61,121,92,52)
simulationreps <- 10
# observed drought periods
D1 <- replicate(simulationreps, rnorm(n=lengthperiod[1], mean=CO2e_CH4_daymeanRSV[1], sd=CO2e_CH4_daysdRSV[1]))
D2 <- replicate(simulationreps, rnorm(n=lengthperiod[2], mean=CO2e_CH4_daymeanRSV[2], sd=CO2e_CH4_daysdRSV[2]))
D3 <- replicate(simulationreps, rnorm(n=lengthperiod[3], mean=CO2e_CH4_daymeanRSV[3], sd=CO2e_CH4_daysdRSV[3]))
D4 <- replicate(simulationreps, rnorm(n=lengthperiod[4], mean=CO2e_CH4_daymeanRSV[4], sd=CO2e_CH4_daysdRSV[4]))
# baseline
D1_base <- replicate(simulationreps, rnorm(n=sum(lengthperiod), mean=CO2e_CH4_daymeanRSV[1], sd=CO2e_CH4_daysdRSV[1]))

Dnotcs <- rbind(D1, D2, D3, D4)
D <- apply(Dnotcs, 2, cumsum)
D_base <- apply(D1_base, 2, cumsum)

# get the summary stats from that simulation
# observed
summaryDmean <- rowMeans(D)
summaryDsd <- apply(D,1,sd)
summaryD975 <- apply(D,1,quantile,probs=c(.975))
summaryD25 <- apply(D,1,quantile,probs=c(.025))
summaryD <- data.frame(summaryDmean=summaryDmean, summaryDsd=summaryDsd, summaryD975=summaryD975, summaryD25=summaryD25, day=c(1:sum(lengthperiod)))
# baseline
summaryDmean_base <- rowMeans(D_base)
summaryDsd_base <- apply(D_base,1,sd)
summaryD975_base <- apply(D_base,1,quantile,probs=c(.975))
summaryD25_base <- apply(D_base,1,quantile,probs=c(.025))
summaryD_base <- data.frame(summaryDmean_base=summaryDmean_base, summaryDsd_base=summaryDsd_base, summaryD975_base=summaryD975_base, summaryD25_base=summaryD25_base, day=c(1:sum(lengthperiod)))


# plot the summary stats, observed
pD1 <- ggplot(summaryD, aes(day)) + geom_line(aes(y = summaryDmean, colour = "summaryDmean"), size=1, color="black") + geom_errorbar(aes(ymin=summaryD25,ymax=summaryD975), alpha=0.3, color="black", width = 0) + theme_bw()

# plot the summary stats, baseline
pD2 <- ggplot(summaryD_base, aes(day)) + geom_line(aes(y = summaryDmean_base, colour = "summaryDmean"), size=1, color="black") + geom_errorbar(aes(ymin=summaryD25_base,ymax=summaryD975_base), alpha=0.3, color="black", width = 0) + theme_bw()

# plot the observed and baseline
pD3 <- ggplot(summaryD, aes(day)) + geom_line(aes(y = summaryDmean, colour = "summaryDmean"), size=1, color="black") + geom_errorbar(aes(ymin=summaryD25,ymax=summaryD975), alpha=0.3, color="black", width = 0) + theme_bw() + geom_line(data = summaryD_base, aes(y = summaryDmean_base, colour = "summaryDmean_base"), size=1, color="blue") + geom_errorbar(data = summaryD_base, aes(ymin=summaryD25_base,ymax=summaryD975_base), alpha=0.3, color="blue", width = 0)

# save the simulations
# observed
fname = paste(pathsavetab, "CO2eSimulationResults/CO2esimulationresults_CH4_observed.csv" ,sep="")
write.csv(summaryD, file = fname, na = "")
# baseline
fname = paste(pathsavetab, "CO2eSimulationResults/CO2esimulationresults_CH4_baseline.csv" ,sep="")
write.csv(summaryD_base, file = fname, na = "")


########################################################################
# SAVE FIGURE OF COMBINED RESULTS

# now get a full combined figure

pMD3 <- ggplot(summaryD, aes(day)) + geom_line(aes(y = summaryDmean, colour = "summaryDmean"), size=1, color="black") + geom_errorbar(aes(ymin=summaryD25,ymax=summaryD975), alpha=0.3, color="black", width = 0) + theme_bw() + geom_line(data = summaryD_base, aes(y = summaryDmean_base, colour = "summaryDmean_base"), size=1, color="blue") + geom_errorbar(data = summaryD_base, aes(ymin=summaryD25_base,ymax=summaryD975_base), alpha=0.3, color="blue", width = 0) + geom_line(data = summaryM, aes(y = summaryMmean, colour = "summaryMmean"), size=1, color="black") + geom_errorbar(data = summaryM, aes(ymin=summaryM25,ymax=summaryM975), alpha=0.3, color="black", width = 0) + geom_line(data = summaryM_base, aes(y = summaryMmean_base, colour = "summaryMmean_base"), size=1, color="blue") + geom_errorbar(data = summaryM_base, aes(ymin=summaryM25_base,ymax=summaryM975_base), alpha=0.3, color="blue", width = 0)


# define the key two plots again so you can add things like the legend, etc.

pD3 <- ggplot(summaryD, aes(day)) + geom_line(aes(y = summaryDmean, colour = "summaryDmean"), size=1) + geom_errorbar(aes(ymin=summaryD25,ymax=summaryD975), alpha=0.3, color="blue", width = 0) + theme_bw() + geom_line(data = summaryD_base, aes(y = summaryDmean_base, colour = "summaryDmean_base"), size=1) + geom_errorbar(data = summaryD_base, aes(ymin=summaryD25_base,ymax=summaryD975_base), alpha=0.3, color="black", width = 0) + xlab("Day") + ylab("Cumulative CO2-e, CH4 (kg/ha)") + theme(legend.position="none") + scale_color_manual("",values=c(summaryDmean="blue",summaryDmean_base="black"), labels=c("Observed", "Baseline"))

pM3 <- ggplot(summaryM, aes(day)) + geom_line(aes(y = summaryMmean, colour = "summaryMmean"), size=1) + geom_errorbar(aes(ymin=summaryM25,ymax=summaryM975), alpha=0.3, color="blue", width = 0) + theme_bw() + geom_line(data = summaryM_base, aes(y = summaryMmean_base, colour = "summaryMmean_base"), size=1) + geom_errorbar(data = summaryM_base, aes(ymin=summaryM25_base,ymax=summaryM975_base), alpha=0.3, color="black", width = 0) + theme(axis.title.x=element_blank()) + ylab("Cumulative CO2-e, CO2 (kg/ha)") + theme(legend.position="none") + scale_color_manual("",values=c(summaryMmean="blue",summaryMmean_base="black"), labels=c("Observed", "Baseline"))


# save combined panels
png(file = paste(pathsavefigs, "CO2e_cumulative.png", sep=""),width=5,height=8,units="in",res=400)
grid.arrange(pM3, pD3, nrow = 2, ncol = 1)
dev.off()

# save this one so you can get the legend to make it look nice later
png(file = paste(pathsavefigs, "CO2e_CH4_getlegend.png", sep=""),width=5,height=6,units="in",res=400)
pD3 + theme(legend.position="bottom")
dev.off()


########################################################################
# GET 95% CI NUMBERS FOR RESULTS PARAGRAPH IN TEXT

# for reference
# lengthperiod <- c(61,121,92,52)

# During the drought and drought recovery periods, 56 t of CO2e ha-1 (X-Y 95% CI) was consumed as CH4 at the watershed scale
summaryD[62,] # beginning of drought
rowtmp <- 61+ 121 + 92
summaryD[rowtmp,] # end of drought recovery
meantmp <- (summaryD[62,1]-summaryD[rowtmp,1])/1000
tmp975 <- (summaryD[62,3]-summaryD[rowtmp,3])/1000
tmp025 <- (summaryD[62,4]-summaryD[rowtmp,4])/1000

# If CH4 emissions had continued at the same rate during the drought and drought recovery periods as during pre-drought, 234 t CO2e (X-Y 95% CI) would have been emitted per hectare.
summaryD_base[62,] # beginning of drought
rowtmp <- 61+ 121 + 92
summaryD_base[rowtmp,] # end of drought recovery
meantmp <- (summaryD_base[rowtmp,1]-summaryD_base[62,1])/1000
tmp975 <- (summaryD_base[rowtmp,3]-summaryD_base[62,3])/1000
tmp025 <- (summaryD_base[rowtmp,4]-summaryD_base[62,4])/1000

# During the same time period, CO2 emissions rates increased above pre-drought levels, and led to an additional 3.5 t CO2e ha-1 (X-Y 95% CI) in emissions.  
summaryM[62,] # beginning of drought
rowtmp <- 61+ 121 + 92
summaryM[rowtmp,] # end of drought recovery
meantmpA <- (summaryM[rowtmp,1]-summaryM[62,1])/1000
tmp975A <- (summaryM[rowtmp,3]-summaryM[62,3])/1000
tmp025A <- (summaryM[rowtmp,4]-summaryM[62,4])/1000

summaryM_base[62,] # beginning of drought
rowtmp <- 61+ 121 + 92
summaryM_base[rowtmp,] # end of drought recovery
meantmpB <- (summaryM_base[rowtmp,1]-summaryM_base[62,1])/1000
tmp975B <- (summaryM_base[rowtmp,3]-summaryM_base[62,3])/1000
tmp025B <- (summaryM_base[rowtmp,4]-summaryM_base[62,4])/1000

meantmpA-meantmpB
tmp975A-tmp975B
tmp025A-tmp025B

# However, CH4 emissions rose dramatically during the post-drought period, offsetting 75% (X-Y 95% CI) of that C sink per hectare in the first 50 days of higher CH4 emissions.
# amt emitted as CH4 during post drought
rowtmp <- 61+ 121 + 92
summaryD[rowtmp,] # end of drought recovery
summaryD[dim(summaryD)[1],] # end of period
meantmpA <- (summaryD[dim(summaryD)[1],1]-summaryD[rowtmp,1])/1000
tmp975A <- (summaryD[dim(summaryD)[1],3]-summaryD[rowtmp,3])/1000
tmp025A <- (summaryD[dim(summaryD)[1],4]-summaryD[rowtmp,4])/1000
# amt of difference between base and observed at the end of drought recovery
summaryD[rowtmp,] # end of drought recovery
summaryD_base[rowtmp,] # end of drought recovery
meantmpB <- (summaryD_base[rowtmp,1]-summaryD[rowtmp,1])/1000
tmp975B <- (summaryD_base[rowtmp,3]-summaryD[rowtmp,3])/1000
tmp025B <- (summaryD_base[rowtmp,4]-summaryD[rowtmp,4])/1000
# recovered by certain present
meantmpA/meantmpB
tmp975A/tmp975B
tmp025A/tmp025B

# from abstract:
# Pseudo-continuous greenhouse gas (GHG) measurements showed that drought impacts varied across topography: soil carbon dioxide (CO2) emissions increased by 60% (slopes) to 163% (valleys), while methane (CH4) sinks increased on slopes and ridges and CH4 release from valleys fell by 90%, leading to a landscape-scale net consumption of 43 t CO2-e ha-1 over four months of drought.  
# for this see the csv calcs, since this doesn't rely on the simulations and is drawn from the mean outcomes

# diff for CO2 at end of drought period
rowtmp <- 61+ 121
# amt of difference between base and observed at the end of drought recovery
summaryM[rowtmp,] # end of drought recovery
summaryM_base[rowtmp,] # end of drought recovery
meantmpB <- (summaryM_base[rowtmp,1]-summaryM[rowtmp,1])/1000
tmp975B <- (summaryM_base[rowtmp,3]-summaryM[rowtmp,3])/1000
tmp025B <- (summaryM_base[rowtmp,4]-summaryM[rowtmp,4])/1000

# diff for CH4 at end of drought period
# amt of difference between base and observed at the end of drought recovery
summaryD[rowtmp,] # end of drought recovery
summaryD_base[rowtmp,] # end of drought recovery
meantmpB <- (summaryD_base[rowtmp,1]-summaryD[rowtmp,1])/1000
tmp975B <- (summaryD_base[rowtmp,3]-summaryD[rowtmp,3])/1000
tmp025B <- (summaryD_base[rowtmp,4]-summaryD[rowtmp,4])/1000

# However, high post-drought CH4 emissions offset 75% of this consumption over just 7 weeks.  
# same as above



########################################################################
# APPROACH 1: PROPAGATE THE SD ACROSS THE TIME PERIOD

# this failed massively, so everything below is now commented out
# this is based off of the excel spreadsheet work that I did by hand at the outset for this analysis

# 
# ########################################################################
# # BRING IN GHG DATA, MAKE DATAFRAME
# 
# # load csv
# arrayCO2edf <- read.csv(paste(pathGHGdata, "ghg-co2e-analysis-csv.csv", sep = ""), stringsAsFactors=FALSE)
# 
# # for reference, these are the variables in order
# # day
# # drought period
# # CO2-e CO2 cumulative kg per ha - actual	
# # CO2-e CH4 cumulative kg per ha - actual	
# # CO2-e total cumulative kg per ha - actual	
# # CO2-e CO2 cumulative kg per ha - baseline	
# # CO2-e CH4 cumulative kg per ha - baseline	
# # CO2-e total cumulative kg per ha - baseline	
# # CO2-e CO2 cumulative kg per ha - actual, high	
# # CO2-e CH4 cumulative kg per ha - actual, high	
# # CO2-e total cumulative kg per ha - actual, high	
# # CO2-e CO2 cumulative kg per ha - baseline, high	
# # CO2-e CH4 cumulative kg per ha - baseline, high	
# # CO2-e total cumulative kg per ha - baseline, high	
# # CO2-e CO2 cumulative kg per ha - actual, low	
# # CO2-e CH4 cumulative kg per ha - actual, low	
# # CO2-e total cumulative kg per ha - actual, low	
# # CO2-e CO2 cumulative kg per ha - baseline, low	
# # CO2-e CH4 cumulative kg per ha - baseline, low	
# # CO2-e total cumulative kg per ha - baseline, low
# 
# # nice names
# newnames <- c("day","DroughtPeriod","CO2eCO2_obs_mean","CO2eCH4_obs_mean","CO2eTOT_obs_mean","CO2eCO2_base_mean","CO2eCH4_base_mean","CO2eTOT_base_mean","CO2eCO2_obs_hi","CO2eCH4_obs_hi","CO2eTOT_obs_hi","CO2eCO2_base_hi","CO2eCH4_base_hi","CO2eTOT_base_hi","CO2eCO2_obs_lo","CO2eCH4_obs_lo","CO2eTOT_obs_lo","CO2eCO2_base_lo","CO2eCH4_base_lo","CO2eTOT_base_lo")
# names(arrayCO2edf) <- newnames
# 
# # make factors where needed
# arrayCO2edf$DroughtPeriod <- as.factor(arrayCO2edf$DroughtPeriod)
# # str(arrayCO2edf)
# 
# 
# ########################################################################
# # CO2 CO2e TIME SERIES
# 
# pCO2 <- ggplot(arrayCO2edf, aes(day)) + geom_line(aes(y = CO2eCO2_obs_mean, colour = "CO2eCO2_obs_mean"), size=1) + geom_line(aes(y = CO2eCO2_base_mean, colour = "CO2eCO2_base_mean"), size=1) + geom_errorbar(aes(ymin=CO2eCO2_obs_lo,ymax=CO2eCO2_obs_hi), alpha=0.1, color="blue", width = 0) + geom_errorbar(aes(ymin=CO2eCO2_base_lo,ymax=CO2eCO2_base_hi), alpha=0.1, color="black", width = 0) + theme_bw() + scale_color_manual("",values=c(CO2eCO2_obs_mean="blue",CO2eCO2_base_mean="black"), labels=c("Baseline", "Observed")) + ylab("Cumulative CO2-e, CO2 (kg/ha)") + xlab("Day")
# 
# pCH4 <- ggplot(arrayCO2edf, aes(day)) + geom_line(aes(y = CO2eCH4_obs_mean, colour = "CO2eCH4_obs_mean"), size=1) + geom_line(aes(y = CO2eCH4_base_mean, colour = "CO2eCH4_base_mean"), size=1) + geom_errorbar(aes(ymin=CO2eCH4_obs_lo,ymax=CO2eCH4_obs_hi), alpha=0.1, color="blue", width = 0) + geom_errorbar(aes(ymin=CO2eCH4_base_lo,ymax=CO2eCH4_base_hi), alpha=0.1, color="black", width = 0) + theme_bw() + scale_color_manual("",values=c(CO2eCH4_obs_mean="blue",CO2eCH4_base_mean="black"), labels=c("Baseline", "Observed")) + ylab("Cumulative CO2-e, CH4 (kg/ha)") + xlab("Day")
# 
# pTOT <- ggplot(arrayCO2edf, aes(day)) + geom_line(aes(y = CO2eTOT_obs_mean, colour = "CO2eTOT_obs_mean"), size=1) + geom_line(aes(y = CO2eTOT_base_mean, colour = "CO2eTOT_base_mean"), size=1) + geom_errorbar(aes(ymin=CO2eTOT_obs_lo,ymax=CO2eTOT_obs_hi), alpha=0.1, color="blue", width = 0) + geom_errorbar(aes(ymin=CO2eTOT_base_lo,ymax=CO2eTOT_base_hi), alpha=0.1, color="black", width = 0) + theme_bw() + scale_color_manual("",values=c(CO2eTOT_obs_mean="blue",CO2eTOT_base_mean="black"), labels=c("Baseline", "Observed")) + ylab("Cumulative CO2-e, CO2+CH4 (kg/ha)") + xlab("Day")
# 
# pCombo <- ggplot(arrayCO2edf, aes(day)) + geom_line(aes(y = CO2eTOT_obs_mean, colour = "CO2eTOT_obs_mean"), size=1) + geom_line(aes(y = CO2eTOT_base_mean, colour = "CO2eTOT_base_mean"), size=1) + geom_line(aes(y = CO2eCO2_obs_mean, colour = "CO2eCO2_obs_mean"), size=1) + geom_line(aes(y = CO2eCO2_base_mean, colour = "CO2eCO2_base_mean"), size=1) + geom_line(aes(y = CO2eCH4_obs_mean, colour = "CO2eCH4_obs_mean"), size=1) + geom_line(aes(y = CO2eCH4_base_mean, colour = "CO2eCH4_base_mean"), size=1) + theme_bw() #+ geom_errorbar(aes(ymin=CO2eTOT_obs_lo,ymax=CO2eTOT_obs_hi), alpha=0.1, width = 0) + geom_errorbar(aes(ymin=CO2eTOT_base_lo,ymax=CO2eTOT_base_hi), alpha=0.1, width = 0) + geom_errorbar(aes(ymin=CO2eCH4_obs_lo,ymax=CO2eCH4_obs_hi), alpha=0.1, width = 0) + geom_errorbar(aes(ymin=CO2eCH4_base_lo,ymax=CO2eCH4_base_hi), alpha=0.1, width = 0) + geom_errorbar(aes(ymin=CO2eCO2_obs_lo,ymax=CO2eCO2_obs_hi), alpha=0.1, width = 0) + geom_errorbar(aes(ymin=CO2eCO2_base_lo,ymax=CO2eCO2_base_hi), alpha=0.1, width = 0) + ylab("Cumulative CO2-e, CO2+CH4 (kg/ha)") + xlab("Day") #+ scale_color_manual("",values=c(CO2eTOT_obs_mean="blue",CO2eTOT_base_mean="black"), labels=c("Baseline", "Observed")) 
# 
# 
# # save combined figure
# 
# # only the gas panels
# #png(file = paste(pathsavefigs, "CO2e_cumulative.png", sep=""),width=8,height=12,units="in",res=400)
# grid.arrange(pCO2, pCH4, pCombo, nrow = 3, ncol = 1)
# #dev.off()
# 


