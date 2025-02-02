library(tidyverse)
library(lubridate)
library(hms)
library(forecast)
library(soundecology)

#Subsets of the larger data frame - only showing one folder at a time 

eclipse_yesno<-A001_SD001|> mutate( eclipse = ifelse(day == "2024-04-08", "yes", "no"))


eclipse_start<-hms(00,00,14)
eclipse_end<-hms(00,50,16)

eclipse_time<-A001_SD001|> filter(hour>= eclipse_start & hour<= eclipse_end) #contains all days through time of eclipse

eclipseonly<-A001_SD001|> filter(hour>= eclipse_start & hour<= eclipse_end)|> filter(day == "2024-04-08") #ONLY 4-8-24 through eclipse


dawn_start<-hms(00,45,05)
dawn_end<-hms(00,15,07)

dawn<-A001_SD001|> filter(hour>= dawn_start & hour<= dawn_end)

#Visualizing BEI
#Full audio files
ggplot(data = A001_SD001, aes( x = hour, y = bei))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~date(time))

ggplot(data = eclipse_yesno, aes( x = hour, y = bei))+
  geom_line(aes(col = eclipse, alpha= eclipse))+
  theme_minimal()+
  labs(title = "Full Duration Comparison of BEI")

#Only duration around time of eclipse 

ggplot(data = eclipse_time, aes(x = hour, y = bei, group = day))+
  geom_line(alpha = 0.3)+
  geom_line(data = eclipseonly, aes(x = hour, y = bei), col = "blue", lwd = 1)+
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
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.3)+ #totality
  geom_line(alpha = 0.3)+
  geom_line(data = eclipseonly, aes(x = hour, y = biophony), col = "blue", lwd = 1)+
  theme_minimal()+
  labs(title = "Biophony over Eclipse Duration")

ggplot(data = eclipse_time, aes(x = hour, y = biophony))+
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.3)+
  geom_line()+
  facet_wrap(~day)+
  theme_minimal()+
  labs(title = "Biophony over the Eclipse Duration")

# AEI

ggplot(data = eclipse_time, aes( x =hour , y = aei))+
  geom_line()+
  facet_wrap(~day)+
  theme_minimal()

ggplot(data = eclipse_time, aes(x = hour, y = aei, group = day))+
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightpink", alpha = 0.3)+
  geom_line(alpha = 0.3)+
  geom_line(data = eclipseonly, aes(x = hour, y = aei), col = "red", lwd =1)+
  theme_minimal()+
  labs(title = "Acoustic evenness over the Eclipse Duration")

#Like Gerber, 2017
ggplot(data = eclipse_time, aes(x = hour, y = aei))+
  geom_point(alpha = 0.2, color = "salmon")+
  geom_point(data = eclipseonly, aes(x = hour, y = aei), col = "red")+
  theme_minimal()




### Combining multiple RDS files into one df
###Displaying ONLY time of eclipse and totality 

fullAudio<-rbind(A001_SD001, A002_SD013, A003_SD005, A004_SD012,A005_SD002)|>
  group_by(folder_name)

onlyEclipseTime<-fullAudio|> filter(hour>= eclipse_start & hour<= eclipse_end)
onlyEclipseDAY<-fullAudio|> filter(hour>= eclipse_start & hour<= eclipse_end)|> filter(day == "2024-04-08")

ggplot(onlyEclipseTime, aes(x = hour, y = biophony, group = day))+
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= biophony), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()
  

#Expanding to show time before eclipse started, rectangle containing beginning to end of "partial eclipse"

ggplot(onlyEclipseTime, aes(x = hour, y = biophony, group = day))+
  geom_rect(xmin =  hms(38,11,14), xmax = hms(38,35,16), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= biophony), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()

#Creating a biophony graphic for dawn 
#Dawn segment records ~30 min before and ~45 min after sunrise 

dawn_start<-hms(00,45,05)
dawn_end<-hms(00,15,07)

onlyDawn<- fullAudio|> filter(hour>= dawn_start & hour<= dawn_end)

ggplot(onlyDawn, aes(x = hour, y = biophony, group = day))+
  geom_rect(xmin =  hms(00,15, 06), xmax = hms(00,30,06), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  facet_wrap(~folder_name)+
  theme_minimal()

# Acoustic evenness 

ggplot(onlyEclipseTime, aes(x = hour, y = aei, group = day))+
  geom_rect(xmin =  hms(38,11,14), xmax = hms(38,35,16), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= aei), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()

ggplot(onlyDawn, aes(x = hour, y = aei, group = day))+
  geom_rect(xmin =  hms(00,15, 06), xmax = hms(00,30,06), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  facet_wrap(~folder_name)+
  theme_minimal()








