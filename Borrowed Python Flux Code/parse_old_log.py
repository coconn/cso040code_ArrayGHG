path = "/Users/exnihilo/Dropbox/Picarro/rpaul-2014-Soil_measure_times/2014-07-22.txt"

type = "id"

with open(path, 'r') as file:
    for line in file:
        line = line.strip()
        line = line.split()
        if type == "id":
            if len(line) == 1 and line[0][0] in "FIC":
                print(line[0]+'\n'+line[0])
        elif type == 'time':
            if len(line) == 3 and line[1][0] == 'o':
                print(line[2])