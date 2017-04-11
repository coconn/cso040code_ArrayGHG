#Allegra Mayer  GC processing
#modified from ABM matlab code

#TraceGasIncubation<-function(){

#install.packages("ggplot2", lib="C:/Users/alleg_000/OneDrive/Documents/PhD/Compost studies/Data/GC processed flux spreadsheets")
library(ggplot2)
#Clear memory

#Clear Figures

########################update every time! ################################
#Find data source and file to save the output under
setwd("C:/Users/alleg_000/OneDrive/Documents/PhD/Compost studies/Data/GC processed flux spreadsheets")

m<-read.csv("150305.csv", header=TRUE) ##reads in file, update everytime new file is required
datafile1<-"150305.csv" ## maybe include source here? data table.
#sample dates for each data set
dates<-c(150305) #dates of folder names
sampledate<-150305

days<-c(0) # days elapsed since amendments added (in order of files above) may 5
#----------------------------------------------
#AIR TEMPERATURE ON SAMPLE DATES (constant, lab air temp, affects air density- moles of air in headspace)
Tair<-20
#----------------------------------------------
JarArea<-0.00588 #(meters squared) INCUBATION JAR AREA
#----------------------------------------------
count<-0 #row number for all chamber data
#----------------------------------------------


#----------------------------------------------------
####################################################
#LOOP THROUGH SAMPLE DATES (THEN INDIVIDUAL CHAMBERS)

#for(dd in 1:length(dates)){


  
   #m<- read.csv("150312.csv", header=TRUE) # OR (sourcepath, dates([i]'\'dates[i], 'gc.csv')
   m[m==99999]<-NaN ##does not include standards as samples, or no data
  
  o<-read.csv("totaldryweight.csv", header=TRUE) ##reads file with dryweight data for mass calculations
#loop through dryweight to include only 6 reps if after the 3rd week
rp<-o[,1]
drywt<-o[,2]
amdrywt<-o[,3]
if (days>21){
  for(i in 1:length(drywt)){
    if (rp[i]==7){
      rp<-rp[-i]
      drywt<-drywt[-i]
      amdrywt<-amdrywt[-i]
    }
  }
}

   P<-101.3 #(kPa = air pressure in the lab (Assumed))
  
  
  injnum<-m[,1]  #injection #
  run <- m[,2] #autosampler run 
  pos <- m[,3]  #tray position
  trt<- m[,4] #Treatment number (std=100, 1 = goat bedding, 2 = Cow manure, 3 = Horse bedding, 4= New Greenwaste, 5 = Old Greenwaste, 6 = compost, 7 = control soil)
  rep<-m[,5] #rep (A-G= 1-7)
  stds<-trt==100 #assign standards
  volairjar<- m[,6]*1*10^-6 #convert volume of headspace in the jar ml to m3
  
  etime<- m[,7] #elapsed time for incubation ...should be ~ 3 hours 
  ch4pk<- m[,8] #methane peak
  co2pk<- m[,9] #Co2 peak
  n2opk<-m[,10] #n2o peak
  drywt<-drywt #from the o file, adjusted for # reps


   

  ###############################################
  # STANDARD PEAK AREA FILTERING AND AVERAGING
  # FIND INITIAL MEDIAN ###### DO THIS IN A FOR LOOP FOR EACH RUN!!!!?#?#?#?#?#?#?#?
     
 
  
  for(j in 1:max(run)){
    runnum<-run==j
    ch4pkrun<-ch4pk[runnum]
    n2opkrun<-n2opk[runnum]
    co2pkrun<-co2pk[runnum]
    stds<-trt[runnum]==100 #assign standards within the run
    ch4std1 <- ch4pkrun[stds]
    ch4std<-ch4std1[!is.na(ch4std1)] #remove NANs
     
    n2ostd1 <- n2opkrun[stds]
    n2ostd<-n2ostd1[!is.na(n2ostd1)] #remove NANs
    
    co2std1 <- co2pkrun[stds]
    co2std<-co2std1[!is.na(co2std1)] #remove NANs
    injrun1 <- injnum[runnum]
    injrun<-injrun1[!is.na(injrun1)] #remove NANs
    injstdwna<-injrun[stds]
    injstd<-injstdwna[!is.na(injstdwna)] #get rid of NA values
    
    ch4med = median(ch4std, na.rm=TRUE)
    n2omed = median(n2ostd, na.rm=TRUE)
    co2med = median(co2std, na.rm=TRUE)
    # find outliers from the median by greater than 5%
    badch4std <- (ch4std>1.05*ch4med | ch4std<0.95*ch4med)
    badn2ostd <-(n2ostd>1.05*n2omed | n2ostd<0.95*n2omed)
    badco2std <- (co2std>1.05*co2med | co2std<0.95*co2med)
    # remove outliers, re-average
    ch4f<-ch4std
    ch4f[badch4std]<-NaN
    ch4stdpkavg<-mean(ch4f, na.rm=TRUE)
    n2of<-n2ostd
    n2of[badn2ostd]<-NaN
    n2ostdpkavg <-mean(n2of, na.rm=TRUE)
    co2f<-co2std
    co2f[badco2std] <- NaN
    co2stdpkavg <-mean(co2f, na.rm=TRUE)
    
    ##########################################################
    # look at standards by plotting
    ###########################
    # DON'T CURRENTLY SEE ANY DRIFT THAT NEEDS CORRECTING
    #COULD ADD THIS LATER, BASED ON RESIDUAL TESTS AFTER REMOVING BAD STDS
    
    ##PLOT CH4 STDS ########################
    plot(injrun[stds], ch4pkrun[stds], type="l", lwd=2, ylab="CH_4 peak height", xlab="injection #")
    points(injrun[stds],ch4pkrun[stds],type='p') #'MarkerFaceColor','r')
    points(injstd[badch4std],ch4std[badch4std], pch=4, col="red") #'LineWidth',2,'MarkerSize',14)
    lines(c(0, max(injrun)), c(ch4stdpkavg, ch4stdpkavg), col="blue")
    
    #############plot N2O STDS#################
    plot(injrun[stds], n2opkrun[stds],  type="l", ylab="N2O height",xlab="injection #", lwd=2)
    points(injnum[stds],n2opk[stds],type='p', col="light blue") #'MarkerFaceColor','r')
    points(injstd[badn2ostd],n2ostd[badn2ostd], pch=4, col="red") #'LineWidth',2,'MarkerSize',14)
    lines(c(0, max(injrun)), c(n2ostdpkavg, n2ostdpkavg), col="blue")
   # xlim = c(0, length(injnum))
   
   
   ##############################plot CO2 standards ###################################
   plot(injrun[stds], co2pkrun[stds],  type="l", ylab="CO2 height",xlab="injection #", lwd=2)
   points(injnum[stds],co2pk[stds],type='p', col="light blue") #'MarkerFaceColor','r')
   points(injstd[badco2std],co2std[badco2std], pch=4, col="red") #'LineWidth',2,'MarkerSize',14)
   lines(c(0, max(injrun)), c(co2stdpkavg, co2stdpkavg), col="blue")
   #------------------------------------------------------------------
   #%--------------------------------------------------------------------------
   # % save a pdf of the current figure
   # figname = ['standards_' dates{dd}];
   #if (makefigs==1)
   # print('-dpdf', [chfigpath figname '.pdf']);
   # end
   #%--------------------------------------------------------------------------
   
   ##############################################33
   # APPLY CALIBRATIONS - This assumes linear fit through (0,0)
   #which was confirmed in GC_autosample_calib.m
   ch4calib <- (9.91/ch4stdpkavg)*ch4pk[runnum]
   n2ocalib <- (10.4/n2ostdpkavg)*n2opk[runnum]
   co2calib <- (997/co2stdpkavg)*co2pk[runnum]
   #-------------------------------------------
   
   #Append all the calibrated runs together
   
   if(j==1){
     #calibrated raw data
     ch4new<-ch4calib 
     n2onew<-n2ocalib
     co2new<-co2calib
   
     
     
   }else {
     ch4new<-c(ch4new, ch4calib)
     n2onew<-c(n2onew, n2ocalib)
     co2new<-c(co2new, co2calib)
   }
     
   
  
  }
  ####save into a file of the calibrated data, all appended from the runs for that sampling day #########
calibrateddata<- data.frame(trt, rep, volairjar, etime, ch4new, co2new, n2onew)
  
 
 ############################################################33
 ##CALCULATE THE FLUX##########

 ##moles per jar at T3 - moles per jar at T0
 
 ##divided by etime
 
 #------------------------------------------
 # umoles trace gases per jar volume
 
 ######this will be different for each jar. Quart Jar  is about 500 mL full of sample, so volume for gas is 946-500=446 mL--convert to m3
 # air density moles of air in m3 at site Air T and Pressure,
 # Campbell/Norman pg 38
 
 aird <- 44.6*(101.3/101.3)*(273.15/(22+273.15)) ###########################################??? lab conditions, ambient at 22 degrees celsius
 # umoles of trace gas inside jar
 # umol/mol air * mol air/m3 * m3/jar = umol/jar
 #calculate the actual ppm in the jar
 ch4mol<-ch4new*aird*volairjar
 n2omol<-n2onew*aird*volairjar
 co2mol<-co2new*aird*volairjar
 
 
 ch4jar<-ch4mol[!is.na(ch4mol)] #remove the standards
 n2ojar<-n2omol[!is.na(n2omol)] #remove the standards
 co2jar<-co2mol[!is.na(co2mol)] #remove the standards
 
 ## ELAPSED TIME IN CHAMBER
 ti <-etime[!is.na(etime)] # elapsed hours without NA (aka standards)
 
 xrange <- c(0, max(ti)) # range for fitted curve to plot OR 0:100:max(ti)
 
 ##calculate Flux
 ###############################################################3
 # LOOP THROUGH DATA FROM EACH CHAMBER MEASUREMENT
 
#define all of the vectors
 jaridwna <- trt + (rep/10)
 jarid<-jaridwna[!is.na(jaridwna)] # remove stds/NAs 
 jarind <- unique(jarid[jarid>0.5 & jarid<9])#, na.rm=TRUE) #why do I have an extra NA? And why the range?
 Telapsed<-c(length(jarind))
  TRT<-c(length(jarind))
 ch4flux<-c(length(jarind))
 n2oflux<-c(length(jarind))
 co2flux<-c(length(jarind))
ch4fluxPerS<-c(length(jarind))
n2ofluxPerS<-c(length(jarind))
co2fluxPerS<-c(length(jarind))
 ch4t0<-c(length(jarind))
n2ot0<-c(length(jarind))
co2t0<-c(length(jarind))
ch4t3<-c(length(jarind))
n2ot3<-c(length(jarind))
co2t3<-c(length(jarind))

 for (i in 1:length(jarind)){
   
   #print(dates[dd],"_", toString(jarind[i])) # show which plot we're on
   count <-count+1
   tst1 <- jarid==jarind[i]
   x<-!is.na(tst1)
   tst<-tst1[x]
   T<-ti[tst]
   
   T0<-T[1]
   T3<-T[2]
   
   ch4pts<-ch4jar[tst] #get the methane values for each time point, is a vector with 2 elements
   ch4t0[i]<-ch4pts[1] #first value in vector must be the T0 value
   ch4t3[i]<-ch4pts[2] # second element is T3 value
   
   n2opts<-n2ojar[tst] #get the nitrous oxide values for each time point, is a vector with 2 elements
   n2ot0[i]<-n2opts[1] #first value in vector must be the T0 value
   n2ot3[i]<-n2opts[2] # second element is T3 value
   
   co2pts<-co2jar[tst] #get the nitrous oxide values for each time point, is a vector with 2 elements
   co2t0[i]<-co2pts[1] #first value in vector must be the T0 value
   co2t3[i]<-co2pts[2] # second element is T3 value
   
   Telapsed[i]<-T3-T0
   
  # ch4fluxPerSSU[i]<-(ch4t3[i]-ch4t0[i])/(Telapsed[i]*JarArea*3600) #umol/m2/s
  # n2ofluxPerSSU[i]<-(n2ot3[i]-n2ot0[i])/(Telapsed[i]*JarArea*3600) #umol//m2/s
  # co2fluxPerSSU[i]<-(co2t3[i]-co2t0[i])/(Telapsed[i]*JarArea*3600) #umol/m2/s
   
  #--------------------------------------------
   #need Silver Lab Units ugC-CO2/cm2/hr: % Silver lab units
  #% CO2 - (umol m-2 s-1) to (ug C cm-2 hr-1)
   #% (umol m-2 s-1) * (1 mol/10^6 umol) * (12 g/mol) * (10^6 ug/g) * (1 m2/10^4 cm2) * (3600 s/hr)
   #% = ug C cm-2 hr-1    NOTE THAT 10^6 AND 1/10^6 CANCEL OUT
  #----------------------
 #% Silver lab units
  # CH4 - (umol m-2 s-1) to (ng C cm-2 hr-1)
  # (umol m-2 s-1) * (1 mol/10^6 umol) * (12 g/mol) * (10^9 ng/g) * (1 m2/10^4 cm2) * (3600 s/hr)
  # = ng C cm-2 hr-1    NOTE THAT 10^9 * 1/10^4 * 1/10^6 REDUCES TO 1/10
 #-------------------------
 #   % Silver lab units
 #% N2O - (umol m-2 s-1) to (ng N cm-2 hr-1)
 #% (umol m-2 s-1) * (1 mol/10^6 umol) * (14 g/mol) * (10^9 ng/g) * (1 m2/10^4 cm2) * (3600 s/hr)
 #% = ng N cm-2 hr-1    NOTE THAT 10^9 * 1/10^4 * 1/10^6 REDUCES TO 1/10

 # ch4fluxSU[i]<-ch4fluxPerSSU[i]*12*(1/10)*3600 #ng C (CH4) cm-2 hr-1 SILVER UNITS
#  co2fluxSU[i]<-co2fluxPerSSU[i]*12*(1/10^4)*3600 #ug C cm-2 hr-1 SILVER UNITS
#  n2ofluxSU[i]<-n2ofluxPerSSU[i]*14*2*(1/10)*3600 #ng N cm-2 hr-2 SILVER UNITS
 
 #more appropriate units are micrograms C/g soil+amendment /hr
#NewUnits ugC-CO2/g/hr: % Silver lab units
#% CO2 - (umol g-1 hr-1) to (ug C g-1 hr-1)
#% (umol g-1 hr-1) * (1 mol/10^6 umol) * (12 g/mol) * (10^6 ug/g) 
#% = ug C cm-2 hr-1    NOTE THAT 10^6 AND 1/10^6 CANCEL OUT
#----------------------
#% New units
# CH4 - (umol g-1 hr-1) to (ng C g-1 hr-1)
# (umol g-1 hr-1) * (1 mol/10^6 umol) * (12 g/mol) * (10^9 ng/g) 
# = ng C /g hr-1    NOTE THAT 10^9 * 1/10^6 * 1/10^6 REDUCES TO 10^3
#-------------------------
#   % New lab units
#% N2O - (umol g-1 hr-1) to (ng N g-1 hr-1)
#% (umol g-1 hr-1) * (1 mol/10^6 umol) * (14 g/mol) * (10^9 ng/g) 
#% = ng N cm-2 hr-1    NOTE THAT 10^9 * 1/10^6 REDUCES TO 10^3


 ch4fluxPerS[i]<-(ch4t3[i]-ch4t0[i])/(Telapsed[i]*drywt[i]) #umol/g/hr
 n2ofluxPerS[i]<-(n2ot3[i]-n2ot0[i])/(Telapsed[i]*drywt[i]) #umol//g/hr
 co2fluxPerS[i]<-(co2t3[i]-co2t0[i])/(Telapsed[i]*drywt[i]) #umol/g/hr

 
 ch4flux[i]<-ch4fluxPerS[i]*12*(10^3) #ng C (CH4) g-1 hr-1 
 co2flux[i]<-co2fluxPerS[i]*12 #ug C g-1 hr-1 
 n2oflux[i]<-n2ofluxPerS[i]*14*2*(10^3) #ng N g-1 hr-1 
 
###########now also with calculations for comparison with Caleb- using g drywt of amendment only #####
ch4fluxPerSam[i]<-(ch4t3[i]-ch4t0[i])/(Telapsed[i]*amdrywt[i]) #umol/g/hr
n2ofluxPerSam[i]<-(n2ot3[i]-n2ot0[i])/(Telapsed[i]*amdrywt[i]) #umol//g/hr
co2fluxPerSam[i]<-(co2t3[i]-co2t0[i])/(Telapsed[i]*amdrywt[i]) #umol/g/hr

ch4fluxam[i]<-ch4fluxPerSam[i]*12*(10^3) #ng C (CH4) g-2 hr-1 
co2fluxam[i]<-co2fluxPerSam[i]*12 #ug C g-2 hr-1 
n2ofluxam[i]<-n2ofluxPerSam[i]*14*2*(10^3) #ng N g-2 hr-2 
   
 }

#assign the treatments again to each flux
TRT<-c(length(jarind))
for(i in 1:length(jarind)){
  if(jarind[i]<2 & jarind[i]>1){
    TRT[i]<-1
  } else if(jarind[i]<3 & jarind[i]>2){
    TRT[i]<-2
  } else if (jarind[i]<4 & jarind[i]>3){
    TRT[i]<-3
  } else if (jarind[i]<5 & jarind[i]>4){
    TRT[i]<-4
  } else if (jarind[i]<6 & jarind[i]>5){
    TRT[i]<-5
  } else if (jarind[i]<7 & jarind[i]>6){
    TRT[i]<-6
  } else if (jarind[i]<8 & jarind[i]>7){
    TRT[i]<-7
  }
}
#input C to N ratios for each of the treatments
CtoN<-c()
for(i in 1:length(TRT)){
  if(TRT[i]==1){
    CtoN[i]<-9.16 #goat bedding  
  }
  else if(TRT[i]==2){
    CtoN[i]<-38.47 #cow manure
  }
  else if(TRT[i]==3){
    CtoN[i]<-22.94 #horse bedding
  }
  else if(TRT[i]==4){
    CtoN[i]<-29.04 #new greenwaste
  }
  else if(TRT[i]==5){
    CtoN[i]<-33.23 #old greenwaste
  }
  else if(TRT[i]==6){
    CtoN[i]<-15.29 #compost
  }
  else if(TRT[i]==7){
    CtoN[i]<-0 #control
  }
}
CtoN<-CtoN[!is.na(CtoN)]

day<-rep(days, length(jarind))
datacalcs<-data.frame(day,jarind,TRT,Telapsed,ch4flux,n2oflux,co2flux)
datacalcsGoldstein<-data.frame(day, jarind, TRT, ch4fluxam, n2ofluxam, co2fluxam)

plot(ch4flux~TRT)
plot(co2flux~TRT, col="red")
plot(n2oflux~TRT, col="blue")

plot(n2oflux~CtoN)
plot(co2flux~CtoN, col="red")
plot(n2oflux~CtoN, col="blue")

#goat bedding fluxes
#CO2
GBco2<-co2flux[TRT==1]
GBco2avg<-mean(GBco2, na.rm=TRUE)
GBco2sd<-sd(GBco2, na.rm=TRUE) 
#N2o
GBn2o<-n2oflux[TRT==1]
GBn2oavg<-mean(GBn2o, na.rm=TRUE)
GBn2osd<-sd(GBn2o, na.rm=TRUE) 
#CH4
GBch4<-ch4flux[TRT==1]
GBch4avg<-mean(GBch4, na.rm=TRUE)
GBch4sd<-sd(GBch4, na.rm=TRUE) 

#cow manure  fluxes
CMco2<-co2flux[TRT==2]
CMco2avg<-mean(CMco2, na.rm=TRUE)
CMco2sd<-sd(CMco2, na.rm=TRUE) 
#N2o
CMn2o<-n2oflux[TRT==2]
CMn2oavg<-mean(CMn2o, na.rm=TRUE)
CMn2osd<-sd(CMn2o, na.rm=TRUE) 
#CH4
CMch4<-ch4flux[TRT==2]
CMch4avg<-mean(CMch4, na.rm=TRUE)
CMch4sd<-sd(CMch4, na.rm=TRUE) 

#horse bedding  fluxes
HBco2<-co2flux[TRT==3]
HBco2avg<-mean(HBco2, na.rm=TRUE)
HBco2sd<-sd(HBco2, na.rm=TRUE) 
#N2o
HBn2o<-n2oflux[TRT==3]
HBn2oavg<-mean(HBn2o, na.rm=TRUE)
HBn2osd<-sd(HBn2o, na.rm=TRUE) 
#CH4
HBch4<-ch4flux[TRT==3]
HBch4avg<-mean(HBch4, na.rm=TRUE)
HBch4sd<-sd(HBch4, na.rm=TRUE) 

#New Greenwaste  fluxes
NGWco2<-co2flux[TRT==4]
NGWco2avg<-mean(NGWco2, na.rm=TRUE)
NGWco2sd<-sd(NGWco2, na.rm=TRUE) 
#N2o
NGWn2o<-n2oflux[TRT==4]
NGWn2oavg<-mean(NGWn2o, na.rm=TRUE)
NGWn2osd<-sd(NGWn2o, na.rm=TRUE) 
#CH4
NGWch4<-ch4flux[TRT==4]
NGWch4avg<-mean(NGWch4, na.rm=TRUE)
NGWch4sd<-sd(NGWch4, na.rm=TRUE) 

#Old Greenwaste  fluxes
OGWco2<-co2flux[TRT==5]
OGWco2avg<-mean(OGWco2, na.rm=TRUE)
OGWco2sd<-sd(OGWco2, na.rm=TRUE) 
#N2o
OGWn2o<-n2oflux[TRT==5]
OGWn2oavg<-mean(OGWn2o, na.rm=TRUE)
OGWn2osd<-sd(OGWn2o, na.rm=TRUE) 
#CH4
OGWch4<-ch4flux[TRT==5]
OGWch4avg<-mean(OGWch4, na.rm=TRUE)
OGWch4sd<-sd(OGWch4, na.rm=TRUE) 

#Compost  fluxes
Cco2<-co2flux[TRT==6]
Cco2avg<-mean(Cco2, na.rm=TRUE)
Cco2sd<-sd(Cco2, na.rm=TRUE) 
#N2o
Cn2o<-n2oflux[TRT==6]
Cn2oavg<-mean(Cn2o, na.rm=TRUE)
Cn2osd<-sd(Cn2o, na.rm=TRUE) 
#CH4
Cch4<-ch4flux[TRT==6]
Cch4avg<-mean(Cch4, na.rm=TRUE)
Cch4sd<-sd(Cch4, na.rm=TRUE) 

#New Greenwaste  fluxes
contco2<-co2flux[TRT==7]
contco2avg<-mean(contco2, na.rm=TRUE)
contco2sd<-sd(contco2, na.rm=TRUE) 
#N2o
contn2o<-n2oflux[TRT==7]
contn2oavg<-mean(contn2o, na.rm=TRUE)
contn2osd<-sd(contn2o, na.rm=TRUE) 
#CH4
contch4<-ch4flux[TRT==7]
contch4avg<-mean(contch4, na.rm=TRUE)
contch4sd<-sd(contch4, na.rm=TRUE) 

#compile avgs and sds
treatments<-seq(1:7)

co2avg<-c(GBco2avg, CMco2avg, HBco2avg, NGWco2avg, OGWco2avg,Cco2avg,contco2avg)
co2sd<-c(GBco2sd, CMco2sd, HBco2sd, NGWco2sd, OGWco2sd,Cco2sd,contco2sd)

ch4avg<-c(GBch4avg, CMch4avg, HBch4avg, NGWch4avg, OGWch4avg,Cch4avg,contch4avg)
ch4sd<-c(GBch4sd, CMch4sd, HBch4sd, NGWch4sd, OGWch4sd,Cch4sd,contch4sd)

n2oavg<-c(GBn2oavg, CMn2oavg, HBn2oavg, NGWn2oavg, OGWn2oavg,Cn2oavg,contn2oavg)
n2osd<-c(GBn2osd, CMn2osd, HBn2osd, NGWn2osd, OGWn2osd,Cn2osd,contn2osd)

#~~~~~~~~~~~~~~~~~~~~~~~now the Goldstein calculations using only amendment wt~~~~~#
#goat bedding fluxes
#CO2
GBco2am<-co2fluxam[TRT==1]
GBco2avgam<-mean(GBco2am, na.rm=TRUE)
GBco2sdam<-sd(GBco2am, na.rm=TRUE) 
#N2o
GBn2oam<-n2ofluxam[TRT==1]
GBn2oavgam<-mean(GBn2oam, na.rm=TRUE)
GBn2osdam<-sd(GBn2oam, na.rm=TRUE) 
#CH4
GBch4am<-ch4fluxam[TRT==1]
GBch4avgam<-mean(GBch4am, na.rm=TRUE)
GBch4sdam<-sd(GBch4am, na.rm=TRUE) 

#cow manure  fluxes
CMco2am<-co2fluxam[TRT==2]
CMco2avgam<-mean(CMco2am, na.rm=TRUE)
CMco2sdam<-sd(CMco2am, na.rm=TRUE) 
#N2o
CMn2oam<-n2ofluxam[TRT==2]
CMn2oavgam<-mean(CMn2oam, na.rm=TRUE)
CMn2osdam<-sd(CMn2oam, na.rm=TRUE) 
#CH4
CMch4am<-ch4fluxam[TRT==2]
CMch4avgam<-mean(CMch4am, na.rm=TRUE)
CMch4sdam<-sd(CMch4am, na.rm=TRUE) 

#horse bedding  fluxes
HBco2am<-co2fluxam[TRT==3]
HBco2avgam<-mean(HBco2am, na.rm=TRUE)
HBco2sdam<-sd(HBco2am, na.rm=TRUE) 
#N2o
HBn2oam<-n2ofluxam[TRT==3]
HBn2oavgam<-mean(HBn2oam, na.rm=TRUE)
HBn2osdam<-sd(HBn2oam, na.rm=TRUE) 
#CH4
HBch4am<-ch4fluxam[TRT==3]
HBch4avgam<-mean(HBch4am, na.rm=TRUE)
HBch4sdam<-sd(HBch4am, na.rm=TRUE) 

#New Greenwaste  fluxes
NGWco2am<-co2fluxam[TRT==4]
NGWco2avgam<-mean(NGWco2am, na.rm=TRUE)
NGWco2sdam<-sd(NGWco2am, na.rm=TRUE) 
#N2o
NGWn2oam<-n2ofluxam[TRT==4]
NGWn2oavgam<-mean(NGWn2oam, na.rm=TRUE)
NGWn2osdam<-sd(NGWn2oam, na.rm=TRUE) 
#CH4
NGWch4am<-ch4fluxam[TRT==4]
NGWch4avgam<-mean(NGWch4am, na.rm=TRUE)
NGWch4sdam<-sd(NGWch4am, na.rm=TRUE) 

#Old Greenwaste  fluxes
OGWco2am<-co2fluxam[TRT==5]
OGWco2avgam<-mean(OGWco2am, na.rm=TRUE)
OGWco2sdam<-sd(OGWco2am, na.rm=TRUE) 
#N2o
OGWn2oam<-n2ofluxam[TRT==5]
OGWn2oavgam<-mean(OGWn2oam, na.rm=TRUE)
OGWn2osdam<-sd(OGWn2oam, na.rm=TRUE) 
#CH4
OGWch4am<-ch4fluxam[TRT==5]
OGWch4avgam<-mean(OGWch4am, na.rm=TRUE)
OGWch4sdam<-sd(OGWch4am, na.rm=TRUE) 

#Compost  fluxes
Cco2am<-co2fluxam[TRT==6]
Cco2avgam<-mean(Cco2am, na.rm=TRUE)
Cco2sdam<-sd(Cco2am, na.rm=TRUE) 
#N2o
Cn2oam<-n2ofluxam[TRT==6]
Cn2oavgam<-mean(Cn2oam, na.rm=TRUE)
Cn2osdam<-sd(Cn2oam, na.rm=TRUE) 
#CH4
Cch4am<-ch4fluxam[TRT==6]
Cch4avgam<-mean(Cch4am, na.rm=TRUE)
Cch4sdam<-sd(Cch4am, na.rm=TRUE) 

#New Greenwaste  fluxes
contco2am<-co2fluxam[TRT==7]
contco2avgam<-mean(contco2am, na.rm=TRUE)
contco2sdam<-sd(contco2am, na.rm=TRUE) 
#N2o
contn2oam<-n2ofluxam[TRT==7]
contn2oavgam<-mean(contn2oam, na.rm=TRUE)
contn2osdam<-sd(contn2oam, na.rm=TRUE) 
#CH4
contch4am<-ch4fluxam[TRT==7]
contch4avgam<-mean(contch4am, na.rm=TRUE)
contch4sdam<-sd(contch4am, na.rm=TRUE) 

#compile avgs and sds
treatments<-seq(1:7)

co2avgam<-c(GBco2avgam, CMco2avgam, HBco2avgam, NGWco2avgam, OGWco2avgam,Cco2avgam,contco2avgam)
co2sdam<-c(GBco2sdam, CMco2sdam, HBco2sdam, NGWco2sdam, OGWco2sdam,Cco2sdam,contco2sdam)

ch4avgam<-c(GBch4avgam, CMch4avgam, HBch4avgam, NGWch4avgam, OGWch4avgam,Cch4avgam,contch4avgam)
ch4sdam<-c(GBch4sdam, CMch4sdam, HBch4sdam, NGWch4sdam, OGWch4sdam,Cch4sdam,contch4sdam)

n2oavgam<-c(GBn2oavgam, CMn2oavgam, HBn2oavgam, NGWn2oavgam, OGWn2oavgam,Cn2oavgam,contn2oavgam)
n2osdam<-c(GBn2osdam, CMn2osdam, HBn2osdam, NGWn2osdam, OGWn2osdam,Cn2osdam,contn2osdam)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


CtoN<-c(14.6, 27.9, 29.7, 29.04, 26.4, 19.8, 0)
percentN<-c(2.259, 0.795, 1.62, 0.741, 0.926, 1.755,0)

fluxmeans<-data.frame(day[1:length(treatments)], treatments, CtoN,co2avg,co2sd, n2oavg, n2osd, ch4avg, ch4sd)

#PLOT by treatment
ggplot(fluxmeans, aes(treatments, co2avg))+geom_point(aes(x=treatments,y=co2avg), colour='red')+
                                          geom_errorbar(aes(ymin=co2avg-co2sd, ymax=co2avg+co2sd), width=0.25, colour='red')+
                                          labs(y="co2 flux ug C -CO2 g-1 hr-1")

ggplot(fluxmeans, aes(treatments, n2oavg)) +geom_point(aes(x=treatments,y=n2oavg), colour='blue')+
                                            geom_errorbar(aes(ymin=n2oavg-n2osd, ymax=n2oavg+n2osd), width=0.25, colour='blue')+
                                           labs(y="n2o flux ng N -N2O g-1 hr-1")

ggplot(fluxmeans, aes(treatments, ch4avg))+geom_point(aes(x=treatments,y=ch4avg), colour='dark blue')+
  geom_errorbar(aes(ymin=ch4avg-ch4sd, ymax=ch4avg+ch4sd), width=0.25, colour='dark blue')+
  labs(y="ng C -CH4 g soil-1 hr-1")
#errbar(treatments,co2avg, yplus=co2avg+co2sd, yminus=co2avg-co2sd, cap=0.020, xlab="treatment", ylab= "GHG flux umol/hr", main=paste("CO2 flux ", day), col="dark red")

#PLOT By percentN

#PLOT
ggplot(fluxmeans, aes(percentN, co2avg))+geom_point(aes(x=percentN,y=co2avg), colour='red')+
  geom_errorbar(aes(ymin=co2avg-co2sd, ymax=co2avg+co2sd), width=0.15, colour='red')+
  labs(y="co2 flux ug C -CO2 g-1 hr-1")

ggplot(fluxmeans, aes(percentN, n2oavg)) +geom_point(aes(x=percentN,y=n2oavg), colour='blue')+
  geom_errorbar(aes(ymin=n2oavg-n2osd, ymax=n2oavg+n2osd), width=0.15, colour='blue')+
  labs(y="n2o flux ng N -N2O g-1 hr-1")

ggplot(fluxmeans, aes(percentN, ch4avg))+geom_point(aes(x=percentN,y=ch4avg), colour='dark blue')+
  geom_errorbar(aes(ymin=ch4avg-ch4sd, ymax=ch4avg+ch4sd), width=0.15, colour='dark blue')+
  labs(y="ng C -CH4 g soil-1 hr-1")

#plot by CtoN

ggplot(fluxmeans, aes(CtoN, co2avg))+geom_point(aes(x=CtoN,y=co2avg), colour='red')+
  geom_errorbar(aes(ymin=co2avg-co2sd, ymax=co2avg+co2sd), width=0.25, colour='red')+
  labs(y="co2 flux ug C -CO2 g-1 hr-1")

ggplot(fluxmeans, aes(CtoN, n2oavg)) +geom_point(aes(x=CtoN,y=n2oavg), colour='blue')+
  geom_errorbar(aes(ymin=n2oavg-n2osd, ymax=n2oavg+n2osd), width=0.25, colour='blue')+
  labs(y="n2o flux ng N -N2O g-1 hr-1")

ggplot(fluxmeans, aes(CtoN, ch4avg))+geom_point(aes(x=CtoN,y=ch4avg), colour='dark blue')+
  geom_errorbar(aes(ymin=ch4avg-ch4sd, ymax=ch4avg+ch4sd), width=0.25, colour='dark blue')+
  labs(y="ng C -CH4 g soil-1 hr-1")
 
 #COMPILES INTO FILES
 write.csv(calibrateddata, file= paste("calibrated data", sampledate,".csv"))
 write.csv(datacalcs, file=paste("flux calcs", sampledate,".csv"))
 write.table(fluxmeans, file="GHG mean flux data compiled.csv", append=TRUE, sep=",")
  
 


  

  

