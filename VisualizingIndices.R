library(tidyverse)
library(lubridate)
library(hms)
library(forecast)
library(soundecology)

#Visualizing BEI
#Full audio files
ggplot(data = A001_SD001, aes( x = hour, y = bei))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~date(time))

eclipse_yesno<-A001_SD001|> mutate( eclipse = ifelse(day == "2024-04-08", "yes", "no"))

ggplot(data = eclipse_yesno, aes( x = hour, y = bei))+
  geom_line(aes(col = eclipse, alpha= eclipse))+
  theme_minimal()+
  labs(title = "Full Duration Comparison of BEI")

#Only eclipse duration 
eclipse_start<-hms(00,00,14)
eclipse_end<-hms(00,50,16)

eclipse_time<-A001_SD001|> filter(hour>= eclipse_start & hour<= eclipse_end) #contains all days through time of eclipse

eclipseonly<-A001_SD001|> filter(hour>= eclipse_start & hour<= eclipse_end)|> filter(day == "2024-04-08") #ONLY 4-8-24 through eclipse

ggplot(data = eclipse_time, aes(x = hour, y = bei, group = day))+
  geom_line(alpha = 0.3)+
  geom_line(data = eclipseonly, aes(x = hours, y = bei), col = "blue", lwd = 1)+
  theme_minimal()+
  labs(title = "Bioacoustic evenness over the Duration of the Eclipse")

#creating similar graph to Gerber, 2017
ggplot(data = eclipse_time, aes(x = hour, y = bei))+
  geom_point(alpha = 0.2, color = "lightblue")+
  geom_point(data = eclipseonly, aes(x = hour, y = bei), col = "blue")+
  theme_minimal()

#Biophony 

ggplot(data = A001_SD001, aes( x = hour, y = biophony))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~date(time))

ggplot(data = eclipse_time, aes(x = hour, y = biophony, group = day))+
  geom_line(alpha = 0.3)+
  geom_line(data = eclipseonly, aes(x = hour, y = biophony), col = "red", lwd = 1)+
  theme_minimal()+
  geom_vline(xintercept = hms(52,23,15), color= "blue", linetype = "dashed")+ #beginning of totality
  labs(title = "Biophony over Eclipse Duration")

ggplot(data = eclipse_time, aes(x = hour, y = biophony))+
  geom_line()+
  geom_vline(xintercept = hms(52,23,15), color = "blue", linetype = "dashed")+
  facet_wrap(~day)+
  theme_minimal()+
  labs(title = "Biophony over the Eclipse Duration")

# Dawn vs. Eclipse 

dawn_start<-hms(00,45,05)
dawn_end<-hms(00,15,07)

dawn<-A001_SD001|> filter(hour>= dawn_start & hour<= dawn_end)

ggplot(data = eclipse_time, aes( x = hour, y = biophony))+
  geom_line()+
  geom_line(data = dawn, aes( x = hour, y = biophony))+
  facet_wrap(~day)+
  theme_minimal()

# AEI

ggplot(data = eclipse_time, aes( x =hour , y = aei))+
  geom_line()+
  facet_wrap(~day)+
  theme_minimal()

ggplot(data = eclipse_time, aes(x = hour, y = aei, group = day))+
  geom_line(alpha = 0.3)+
  geom_line(data = eclipseonly, aes(x = hour, y = aei), col = "red", lwd =1)+
  geom_vline(xintercept = hms(53,23,15), color = "darkred", linetype = "dashed")+
  theme_minimal()+
  labs(title = "Acoustic evenness over the Eclipse Duration")

#Like Gerber, 2017
ggplot(data = eclipse_time, aes(x = hour, y = aei))+
  geom_point(alpha = 0.2, color = "salmon")+
  geom_point(data = eclipseonly, aes(x = hour, y = aei), col = "red")+
  theme_minimal()


### Combining RDS files into one df

fullAudio<-rbind(A001_SD001, A002_SD013, A003_SD005, A004_SD012)|>
  group_by(folder_name)

onlyEclipseTime<-fullAudio|> filter(hour>= eclipse_start & hour<= eclipse_end)
onlyEclipseDAY<-fullAudio|> filter(hour>= eclipse_start & hour<= eclipse_end)|> filter(day == "2024-04-08")

ggplot(onlyEclipseTime, aes(x = hour, y = biophony))+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= biophony), col = "blue")+
  facet_wrap(~folder_name)
  

  




