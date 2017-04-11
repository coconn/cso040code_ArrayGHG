# Edauri-soil-time-series-figures.R
# 
# walk Edauri through making her soil moisture and temp figures
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# output products:
# 

########################################################################
# BRING IN DATA

# where to save outputs
path_edauri = "~/Desktop/Letters of Rec/Edauri Navarro Perez/Edauri initial data/"

# read.csv
df <- read.csv(paste(path_edauri,"edauri_soil_data.csv",sep=""), stringsAsFactors=FALSE)

# info about the data
# temperature and moisture info is in the same file
# vol water content columns are labeled as: VWC_Avg, VWC_2_Avg, VWC_3_Avg, etc. in row 2
# VWC is in units of m^3/m^3
# temperature columns are labeled as: T_Avg, T_2_Avg, T_3_Avg, etc. also in row 2
# temp is in units of Deg C
# sensor 19 = Control
# sensor 20 = Drought


########################################################################
# WIDE TO LONG FORMAT

library(data.table)

df_long_v2 <- melt(setDT(df), measure = patterns("^VWC", "^T"), value.name = c("VWC", "T"), variable.name = "Treatment")

# rename treatment factors
levels(df_long_v2$Treatment) <- c("Control","Drought")

library(lubridate)

# date time
df_long_v2$Date_Time2 <- mdy_hm(df_long_v2$Date_Time)


########################################################################
# PLOT

library(ggplot2)

ggplot(data=df_long_v2, aes(x=Date_Time2, y=VWC, color=Treatment)) + geom_point() + geom_line() + xlab("Date") + ylab("Volumetric Water Content \n(m^3/m^3)") + theme_classic()

ggplot(data=df_long_v2, aes(x=Date_Time2, y=T, color=Treatment)) + geom_point() + geom_line() + xlab("Date") + ylab("Temperature \n(°C)") + theme_classic()

# ggsave("exampleplotggsave.png")






