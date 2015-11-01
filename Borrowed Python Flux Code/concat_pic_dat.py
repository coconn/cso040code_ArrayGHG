# Concatenates (joins end-to-end) Picarro data files from a single
# day--this makes them easier to process

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
import glob

myPath = "/Users/exnihilo/Dropbox/Picarro/09/"

for root, dirs, files in os.walk(myPath):
    print("Root: {}\tDirs: {}\tFiles:{}".format(root, dirs, files))
    # Do we already have concatenated files? Skip if they're found
    if(len(glob.glob('*_conc.dat')) > 0):
        print("Concat file found. Passing directory.")
    else: 
        headerWritten = False
        for datFile in files:
            if datFile.endswith('User.dat'): # Yes, it is an original .dat file
                print("Processing {}".format(datFile))
                # Let's now open a file in the form "MM-DD_conc.dat"
                with open("{0}/{1}-{2}_conc.dat".format(root,
                                                        root[-5:-3],
                                                        root[-2:]), 'a') as fileOut:
                    with open(root+'/'+datFile, 'r') as readDat:
                        if headerWritten:
                            readDat.readline() # Skip header
                        else:
                            headerWritten = True
                        for line in readDat:
                            fileOut.write(line)
                print("Completed processing {}".format(datFile))
        print("Moving to next directory")
                    