## PACKAGES
import os
import csv
import numpy as np
from scipy import stats
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import rc_file
from matplotlib.font_manager import FontProperties

## PANDAS DISPLAY PROPERTIES
font = FontProperties()
font.set_family('sans-serif')
pd.options.display.mpl_style = 'default'
font = {'family' : 'sans-serif'}
matplotlib.rc('font', **font)

## RCPARAMS PROPERTIES

#rc_file('/Users/exnihilo/Dropbox/Picarro/graphs/doe/doe2014rc')
#rc_file('/Users/exnihilo/Dropbox/Lab Presentation/2014-11-11/labmeet2014rc')
rc_file('/Users/exnihilo/Dropbox/Data/AGU 2014 Poster/aguposter2014rc')

## GLOBAL VARIABLES
figNum = 1

picPath = "/Users/exnihilo/Dropbox/Picarro/"

flux = pd.read_csv(picPath + "flux_data-09-30_processed.csv")
soil = pd.read_csv(picPath + "SOILS.CSV")
o2 = pd.read_csv('/Users/exnihilo/Dropbox/Picarro/o2.csv')
# flux['Epoch'] = flux['Epoch'].astype('datetime64[s]')
flux = flux.set_index('Epoch')
soil = soil.set_index('stamp')
o2 = o2.set_index('Stamp')
# We want a common zero time with flux
soil = soil[soil['date'] >= flux.iloc[0]['Date']]
soil_moist = soil[soil['vol_moist'] > 10] # Exclude bad sensor reads
soil_t = soil[soil['temp'] > 1] # Exclude bad sensor reads

#labels = ['CH4_ug*m^-2*min^-1', 'N_ug*m^-2*min^-1', 
#          'CO2_umol*m^-2*s^-1', 'vol_moist', 'temp']

 
labels = ['CH4_ng*cm^-2*h^-1', 'N_ng*cm^-2*h^-1', 
          'CO2_ug*cm^-2*h^-1', 'vol_moist', 'temp']

# Plot colors & line styles dictionaries
#colors={'C':'#882255', 'F':'#88CCEE', 'I':'#DDCC77'} # Color-blind safe
colors={'C':'#820514', 'F':'#3E728C', 'I':'#DDCC77'} # Darker
lstyles={'C':':', 'F':'--', 'I':'-'}

## FUNCTION DEFINITIONS
def xLabelsByDay (df, interval):
    seconds = df.index[-1] - df.index[0]
    days = seconds // (60*60*24)
    secInterval = interval*60*60*24
    dayLabels = list(range(0, days+interval))[0::interval]
    epochLocs = list(range(df.index[0],df.index[0] + (secInterval * (len(dayLabels)-1))+1,secInterval))
    return epochLocs, dayLabels

# This is the nicer graph collapsed into means with +/- SEM error bars
def MakeGraph(df, gid, roll, label):
    # Plot height by width
    #plotHxW = (8, 3)
    # Default plot resolution
    #plotRes = 100
    # Line width
    linwid = 3
    # Interval of days shown on x axis
    dayInterval = 10
    plt.figure(num=figNum)
    # Produce rolling means and SDs
    rolled = pd.rolling_mean(df[df['GroupID'] == gid][label], roll)
    sdRoll = pd.rolling_std(df[df['GroupID'] == gid][label], roll)
    # SEM = SD/sqrt(n)
    sem = (sdRoll.iloc[roll-1::roll])/(roll**.5)
#     font = {'family' : 'sans-serif'}
#     matplotlib.rc('font', **font)
    rolled.iloc[roll-1::roll].plot(color=colors[gid[0]],
        linestyle=lstyles[gid[0]],
        linewidth=linwid,
        yerr = sem,
        ecolor='black',
        elinewidth = .75)
    locs, labels = xLabelsByDay(df, dayInterval)
    plt.xticks(locs, labels)
    plt.tick_params(top='off')
    plt.xlabel("")
    plt.xlim(1405395347, 1412275367)

# This is the graph showing the beautiful mess of all the data
def quickDirtyGraph(thisLabel):
    # Circle, star, triange, square, diamond
    styles = {1:'o', 2:'*', 3:'^', 4:'s', 5:'d'}
    colors={'C':'#882255', 'F':'#88CCEE', 'I':'#DDCC77'}

    for gid in ('CON', 'FG', 'IG'):
        for rep in range(1, 6):
            flux[(flux['GroupID']==gid) & (flux['Rep']==rep)][thisLabel].plot(marker=styles[rep],
                markersize=7,
                linestyle='-',
                color=colors[gid[0]],
                linewidth=2)

## FLUX GRAPHS
# Run the rest of these blocks of code by selection!

# plt.tight_layout() is buggy on OS X, call this first instead
plt.gcf().set_tight_layout(True)

plt.subplot(411)

# CH4 flux
for gid in ('CON', 'FG', 'IG'):
    MakeGraph(flux, gid, 5, labels[0])
# pd.rolling_mean(flux[flux['GroupID'] == 'CON']['CH4_ug*m^-2*min^-1'], 5).plot(color='#882255', linewidth=2.0)
# pd.rolling_mean(flux[flux['GroupID'] == 'FG']['CH4_ug*m^-2*min^-1'], 5).plot(color='#88CCEE', linewidth=2.0)
# pd.rolling_mean(flux[flux['GroupID'] == 'IG']['CH4_ug*m^-2*min^-1'], 5).plot(color='#DDCC77', linewidth=2.0)
# flux.groupby('GroupID').plot(y='CH4_ug*m^-2*min^-1', style = 'o')
#flux.groupby('GroupID').plot(y=labels[0], markersize=3, style='.')
plt.legend(["Control", "Flooded", "Intermittent"])
plt.title("Soil methane flux", fontweight='bold')
plt.ylabel("$\mu$g CH$_4$ m$^{-2}$ min$^{-1}$")
#plt.xlabel("Day of experiment")
#plt.ylabel("ng CH$_4$ cm$^{-2}$ h$^{-1}$")

plt.subplot(412)

# N2O flux
for gid in ('CON', 'FG', 'IG'):
    MakeGraph(flux, gid, 5, labels[1])
# pd.rolling_mean(flux[flux['GroupID'] == 'CON']['N_ug*m^-2*min^-1'], 5).plot(color='#882255', linewidth=2.0)
# pd.rolling_mean(flux[flux['GroupID'] == 'FG']['N_ug*m^-2*min^-1'], 5).plot(color='#88CCEE', linewidth=2.0)
# pd.rolling_mean(flux[flux['GroupID'] == 'IG']['N_ug*m^-2*min^-1'], 5).plot(color='#DDCC77', linewidth=2.0)
#flux.groupby('GroupID').plot(y='N_ug*m^-2*min^-1', style = 'o')
#flux.groupby('GroupID').plot(y=labels[1], markersize=3, style='.')
#plt.legend(["Control", "Flooded", "Intermittent"], loc=4)
plt.title("Soil nitrous oxide flux", fontweight='bold')
plt.ylabel("$\mu$g N m$^{-2}$ min$^{-1}$")
#plt.xlabel("Day of experiment")
#plt.ylabel("ng N cm$^{-2}$ h$^{-1}$")

plt.subplot(413)

# CO2 flux
for gid in ('CON', 'FG', 'IG'):
    MakeGraph(flux, gid, 5, labels[2])
# pd.rolling_mean(flux[flux['GroupID'] == 'CON']['CO2_umol*m^-2*s^-1'], 5).plot(color='#882255', linewidth=2.0)
# pd.rolling_mean(flux[flux['GroupID'] == 'FG']['CO2_umol*m^-2*s^-1'], 5).plot(color='#88CCEE', linewidth=2.0)
# pd.rolling_mean(flux[flux['GroupID'] == 'IG']['CO2_umol*m^-2*s^-1'], 5).plot(color='#DDCC77', linewidth=2.0)
#flux.groupby('GroupID').plot(y='CO2_umol*m^-2*s^-1', style = 'o')
#flux.groupby('GroupID').plot(y=labels[2], markersize=3, style='.')
#plt.legend(["Control", "Flooded", "Intermittent"])
plt.title("Soil carbon dioxide flux", fontweight='bold')
plt.ylabel("$\mu$mol CO$_2$ m$^{-2}$ s$^{-1}$")
#plt.xlabel("Day of experiment")
#plt.ylabel("$\mu$g CO$_2$ cm$^{-2}$ h$^{-1}$")

#plt.tight_layout()

## O2 SENSOR GRAPH

plt.subplot(414)

o2['O2 Ctrl'].plot(color=colors['C'], linestyle=lstyles['C'])
o2['O2 Flood'].plot(color=colors['F'], linestyle=lstyles['F'])
o2['O2 Int'].plot(color=colors['I'], linestyle=lstyles['I'])
locs, dlabels = xLabelsByDay(o2, 10)
plt.xticks(locs, dlabels)
plt.tick_params(top='off')
plt.xlabel("Day of experiment")
plt.xlim(1405395347, 1412275367)
#plt.legend(["Control", "Flooded", "Intermittent"], loc=4)
plt.title("Soil O$_2$ concentration", fontweight='bold')
plt.ylabel("Percent O$_2$")


## ENV GRAPHS

# Soil moisture
for gid in ('CO', 'IP'):
    MakeGraph(soil_moist, gid, 8, labels[3])
# pd.rolling_mean(soil_moist[soil_moist['GroupID'] == 'CO']['vol_moist'], 4).plot(color='#882255', linewidth=2.0)
# pd.rolling_mean(soil_moist[soil_moist['GroupID'] == 'FP']['vol_moist'], 4).plot(color='#88CCEE', linewidth=2.0)
# pd.rolling_mean(soil_moist[soil_moist['GroupID'] == 'IP']['vol_moist'], 4).plot(color='#DDCC77', linewidth=2.0)
# soil_moist.groupby('GroupID').plot(y='vol_moist', style = 'o')
plt.legend(["Control", "Intermittent"])
plt.title("Soil moisture")
plt.ylabel("Percent water by volume")

# Soil temperature
for gid in ('CO', 'FP', 'IP'):
    MakeGraph(soil_t, gid, 8, labels[4])
# pd.rolling_mean(soil_t[soil_t['GroupID'] == 'CO']['temp'], 4).plot(color='#882255', linewidth=2.0)
# pd.rolling_mean(soil_t[soil_t['GroupID'] == 'FP']['temp'], 4).plot(color='#88CCEE', linewidth=2.0)
# pd.rolling_mean(soil_t[soil_t['GroupID'] == 'IP']['temp'], 4).plot(color='#DDCC77', linewidth=2.0)
# soil_t.groupby('GroupID').plot(y='temp', style = 'o')
plt.legend(["Control", "Flooded", "Intermittent"])
plt.title("Soil temperature at 5cm depth")
plt.ylabel("°C")

## STATS
flux.describe()


for date in ('2014-09-25', '2014-09-30'):
    lastFlux = flux[flux['Date'] == date]
    for label in labels[:3]:
        treats = []
        for gid in ('FG', 'IG', 'CON'):
            treats.append(
                np.log(lastFlux[lastFlux['GroupID']==gid][label] + np.e))
        f_val, p_val = stats.f_oneway(*treats)
        print("{}, {}: F statistic = {}, p={}".format(date, 
                                                      label,
                                                      f_val,
                                                      p_val))