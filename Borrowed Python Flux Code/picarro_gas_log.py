# picarro_gas_log.py

# This script does manual timestamps of
# the start and end of sampling times
# in the gas collars and announces measurement actions

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

import datetime as dt
import pyttsx # text-to-speech (TTS) module
import csv
import os
from collections import OrderedDict

# Windows path
filepath = "C:\\scripts\\output\\"

# OS X path (for testing only)
#filepath = "scripts/"

filename = "gas_logs.csv"

# Provide a TTS warning at x seconds
warnTimes = [30, 15, 10, 5, 4, 3, 2, 1]
# Number of minutes taken to wait for each action
actionTimes = OrderedDict([("chamber on", 1), 
                           ("restart user log", 1),
                           ("chamber off", 4)])
# Treatment labels
treatments = ["FG", "IG", "CON"]
# Number of collars (5)
reps = range(1,6) # range is from start (inclusive) to end (non-inclusive)

# Change these if you had to interrupt and start where you left off
skipCollars = False
# Last completed collar before interrupt
lastCollar = ""

engine = pyttsx.init() # Initialize text-to-speech engine
engine.setProperty('rate', 150) # Set speech rate to 150 WPM

def get_mode():
    print("Select option:")
    print("1. Measuring all gas collars")
    print("2. Measuring F and CON gas collars")
    print("3. Measuring I gas collars")
    modeCode = raw_input("==> ") # Take input as char
    if modeCode not in ('1', '2', '3'): # Check for valid selection
        print("Invalid input")
        modeCode = get_mode()
    return int(modeCode)
    
def initialize_paths_files():
    try:
        os.makedirs(filepath) # Try to create the folders for the log file
    except OSError:
        pass # The folder already exists
    
    try: # See if the file exists
        with open(filepath+filename, 'r'):
            pass
    except IOError: # If it doesn't, create a new one and start off with headers
        with open(filepath+filename, 'wb') as newfile:
            make_header = csv.writer(newfile, dialect='excel')
            make_header.writerow(["Unix Stamp", "Date", "Time", "CollarID", "Action"])

def send_to_count(treats):
    skip = skipCollars
    for label in treats:
        for collar in reps:
            if skip:
                print "Skipping", label+str(collar)
                if (label + str(collar)) == lastCollar:
                    skip = False
            else:
                for action in actionTimes:
                    print("{0} - Currently on {1}, {2}".format(dt.datetime.now(),
                                                            label+str(collar),
                                                            action))
                    countdown(label+str(collar), action, actionTimes[action])

# Run the coundown to the action prompt
def countdown(collarID, action, timeMins):
    currTime = dt.datetime.now() #The time now
    endTime = currTime + dt.timedelta(minutes=timeMins) #End time
    for warn in warnTimes: # At (End - Warn time), speak the countdown alert
        while dt.datetime.now() < (endTime - dt.timedelta(seconds=warn)):
            pass
        if warn >= 10: # Full alert for 10+ seconds left
            speak_item("{0} seconds to {1} for {2}".format(str(warn),
                                                           action, 
                                                           collarID.lower()))
        else:
            speak_item(str(warn)) # Speak seconds left only when below 10 seconds
    while dt.datetime.now() < endTime:
        pass
    speak_item("Go.")
    write_to_log(collarID, action, dt.datetime.now())        

def write_to_log(measuredID, action, currTime):
# Write a measurement start or end time to the log
# Note: this can be run manually from the console, e.g.
# write_to_log("dry03", "lid on", dt.datetime.now())
    with open(filepath + filename, 'a+b') as logfile:
        log_csv = csv.writer(logfile, dialect='excel')
        # Current timestamp in UNIX epoch
        timestamp = int((currTime - dt.datetime(1970, 1, 1)).total_seconds())
        log_csv.writerow([timestamp, # Seconds into UNIX epoch
                         currTime.date(),
                         currTime.time(),
                         measuredID,
                         action])

def speak_item(string):
    engine.say(string)
    engine.runAndWait()

def main():
    initialize_paths_files()
    
    mode = get_mode()
    
    useTreatments = []
    
    if mode == 1: # All treatments
        useTreatments = treatments

    elif mode == 2: # FG and CON only
        useTreatments = [treatments[0], treatments[2]]

    else: # IG only
        useTreatments = [treatments[1]]
    
    print("Starting mode {}".format(mode))
    send_to_count(useTreatments)

# Run stuff
main()