library(tidyverse)
library(lubridate)
library(hms)

#Visualizing BEI

ggplot(data = A001_SD001, aes( x = as_hms(time), y = bei))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~date(time))

ggplot(data = A001_SD001, aes( x = as_hms(time), y = bei))+
  geom_line(aes(col = date(time)))+
  theme_minimal()     

ggplot(data = A001_SD001, aes( x = as_hms(time), y = bei))+
  geom_line(aes(col = ifelse(date(time) == "2024-04-08", "orange","pink"),
            alpha = ifelse(date(time) == "2024-04-08", 1, 0.1 )))+
  theme_minimal()

ggplot(data = A001_SD001, aes( x = as_hms(time), y = bei))+
  geom_line(col = ifelse(date(A001_SD001$time) == "2024-04-08", "blue", "lightblue" ))+
  theme_minimal()    




