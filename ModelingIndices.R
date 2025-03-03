# Modelling the eclipse data 
# using GAMs 

# Packages
library(tidyverse)
library(splines)
library(here)
library(mgcv)

# Loading in the full data set 

fullAudio<-rbind(A001_SD001, A002_SD013, A003_SD005, A004_SD012,A005_SD002, A006_SD006, A007_SD017, A008_SD007, A009_SD009, A010_SD014, A011_SD018, A013_SD016)|>
  group_by(folder_name)|>
  mutate(fullACI = sapply(ACI_all,sum))|>
  mutate(fullADI = sapply(ADI_all, sum))

# Creating an eclipse or not variable 

modelEclipse_df<-fullAudio|> mutate(eclipse = ifelse(day == "2024-04-08", 
                                                     "eclipse", 
                                                     "not_eclipse"))|>
  mutate(eclipse = as_factor(eclipse))|>
  mutate(hour_numeric = as.numeric(hour))

# Spline on Time, Least squares for 'eclipse or not', and interaction between the two 

bei_mod<- gam(bei~ s(hour_numeric, by = eclipse) + eclipse, data = modelEclipse_df)
summary(bei_mod)


