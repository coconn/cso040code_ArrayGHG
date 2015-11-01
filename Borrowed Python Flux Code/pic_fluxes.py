# pic_fluxes.py

# This script takes the measurement time log and matches it up with
# the Picarro data log and collar offsets to give a ppm/s gas flux
# of each measurement interval 

# Copyright (C) 2014-2015 Robert Paul

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.

#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.

#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os
import csv
import numpy as np
from scipy import stats
import pandas as pd
import matplotlib.pyplot as plt

picPath = "/Users/exnihilo/Dropbox/Picarro/"
logPath = picPath + "gas_logs.csv"
offPath = picPath + "offset.csv"

# How long it takes (in seconds) between an action taken and a
# measurement response to appear in the Picarro data
responseTime = 38

# We're looking for these labels for fluxes
getLabels = ["CO2_dry", "CH4_dry", "N2O_dry", "NH3"]

# Load the logs of chamber on & off times to a data frame
myLog = pd.read_csv(logPath, header=0)
myLog = myLog.set_index('Unix Stamp') # Index by Unix time

# The output file name
fluxResults = "flux_data-09-30.csv"

def give_offset_df():
    offsets = pd.read_csv(offPath, header=0)
    offsets = offsets.set_index('stamp') # Index by Unix time
    # Get the mean for each of 2 length values
    off_av = offsets['length'].groupby(np.arange(len(offsets))//2).mean()
    offsets = offsets[offsets.side == 1]
    offsets.length = list(off_av)
    offsets.drop('side', axis = 1, inplace=True)
    return offsets

# This returns the closest timestamp (index) in the data frame nearest
# the search stamp value
def find_closest_offset(df, searchStamp):
    return df.iloc[np.argmin(np.abs(df.index.values - searchStamp))].name
    
def get_offset_val(stamp, collarID):
    off_stamp = find_closest_offset(offsets[offsets.collar_id == collarID], stamp)
    return offsets.loc[off_stamp].length

def pull_fluxes(data, log):
    # The line will be Epoch, Date, Time, GroupID, Rep, Offset,
    # CO2_ppm/s, CO2_R2, CH4_ppm/s, CH4_R2, N2O_ppm/s, N2O_R2,
    # NH3_ppm/s, NH3_R2
    line = []
    # Get list of start times
    startList = list(log[log.Action == "chamber on"].index)
    # Get list of end times
    endList = list(log[log.Action == "chamber off"].index)
    # Now zip them together
    zipStEnd = zip(startList, endList)
    
    for stTime, endTime in zipStEnd:
        print("Getting fluxes for {0} to {1}".format(stTime, endTime))
        measureSubset = data[(stTime + responseTime):(endTime + responseTime)]
        first = log.loc[stTime]
        line.extend([stTime + responseTime, first.Date, first.Time,
                     first.CollarID[:-1], first.CollarID[-1:]])
        line.append(get_offset_val(stTime, first.CollarID))
        for label in getLabels:
            fluxFrame = measureSubset[label]
            slope, r_value = slope_Rval(fluxFrame)
            line.extend([slope, r_value])
        write_flux(line)
        line = []

def slope_Rval (df):
    # Slope and R value from a linear regression of the dataframe
    slope, intercept, r_value, p_value, std_err = stats.linregress(list(df.index),
                                                                   list(df))
    return slope, r_value

def write_flux(line):
    # The line will be Epoch, Date, Time, GroupID, Rep, Offset, CO2_ppm/s, CO2_R2,
    # CH4_ppm/s, CH4_R2, N2O_ppm/s, N2O_R2, NH3_ppm/s, NH3_R2
    filename = fluxResults
    try: # See if the file exists
        with open(picPath+filename, 'r'):
            pass
    except IOError: # If it doesn't, create a new one and start off with headers
        with open(picPath+filename, 'w') as newfile:
            make_header = csv.writer(newfile, dialect='excel')
            make_header.writerow(["Epoch", "Date", "Time", "GroupID",
            "Rep", "Offset", "CO2_ppm_s", "CO2_R2", "CH4_ppm_s", "CH4_R2",
            "N2O_ppm_s", "N2O_R2", "NH3_ppm_s", "NH3_R2"])
    with open(picPath+filename, 'a') as file:
        thisRow = csv.writer(file, dialect='excel')
        thisRow.writerow(line)

# Load the offsets file to a data frame
offsets = give_offset_df()

for root, dirs, files in os.walk(picPath):
    for datFile in files:
        if datFile.endswith('conc.dat'): # Yes, it's a joined .dat file
            thisDate = "2014-{}".format(datFile[:5])
            if thisDate in list(myLog.Date):
                print("Processing", thisDate)
                myData = pd.read_csv(root+'/'+datFile, header=0,
                                     delim_whitespace=True)
                # The Unix epoch time in the Picarro data files are in UTC;
                # this changes the time stamps to CDT to match the other
                # files (UTC - 5h)
                # Future note: it might be preferable to use UTC
                myData['EPOCH_TIME'] = myData['EPOCH_TIME'] - 18000
                myData = myData.set_index('EPOCH_TIME') # Index by Unix time
                dayLog = myLog[myLog.Date == thisDate]
                pull_fluxes(myData, dayLog)