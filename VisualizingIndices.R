library(tidyverse)
library(lubridate)
library(hms)

#Visualizing BEI
#Full audio files
ggplot(data = A001_SD001, aes( x = as_hms(time), y = bei))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~date(time))

ggplot(data = A001_SD001, aes( x = as_hms(time), y = bei))+
  geom_line(aes(col = ifelse(date(time) == "2024-04-08", "orange","pink"),
            alpha = ifelse(date(time) == "2024-04-08", 0.8, 0.1 )))+
  theme_minimal()

ggplot(data = A001_SD001, aes( x = as_hms(time), y = bei))+
  geom_line(col = ifelse(date(A001_SD001$time) == "2024-04-08", "blue", "lightblue" ))+
  theme_minimal()    

#Only eclipse duration 
eclipse_start<-hms(00,00,14)
eclipse_end<-hms(00,50,16)

separate_hrdy<-A001_SD001|> mutate(day = date(time), hours = as_hms(time))|> 
  filter(hours>= eclipse_start & hours<= eclipse_end)

eclipseonly<-A001_SD001|> mutate(day = date(time), hours = as_hms(time))|> 
  filter(hours>= eclipse_start & hours<= eclipse_end)|> filter((date(time) == "2024-04-08"))

ggplot(data = separate_hrdy, aes(x = hours, y = bei))+
  geom_line(alpha = 0.3)+
  geom_line(data = eclipseonly, aes(x = hours, y = bei), col = "blue", lwd = 1)+
  theme_minimal()

#Biophony 

ggplot(data = A001_SD001, aes(x = as_hms(time), y = biophony))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~date(time))

ggplot(data = separate_hrdy, aes(x = hours, y = biophony))+
  geom_line(alpha = 0.3)+
  geom_line(data = eclipseonly, aes(x = hours, y = biophony), col = "red", lwd = 1)+
  theme_minimal()

ggplot(data = separate_hrdy, aes(x = hours, y = biophony))+
  geom_line()+
  facet_wrap(~day)+
  theme_minimal()
