library(tidyverse)
library(lubridate)
library(hms)
library(forecast)
library(soundecology)
library(purr)

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

### Combining multiple RDS files into one df

fullAudio<-rbind(A001_SD001, A002_SD013, A003_SD005, A004_SD012,A005_SD002, A006_SD006, A007_SD017, A008_SD007, A009_SD009, A010_SD014)|>
  group_by(folder_name)|>
  mutate(fullACI = sapply(ACI_all,sum))|>
  mutate(fullADI = sapply(ADI_all, sum))



onlyEclipseTime<-fullAudio|> filter(hour>= eclipse_start & hour<= eclipse_end)
onlyEclipseDAY<-fullAudio|> filter(hour>= eclipse_start & hour<= eclipse_end)|> filter(day == "2024-04-08")

#Remove this next plot after finished, used to look at general index patterns between the folders "whats their normal"
ggplot(fullAudio, aes(x = hour, y = fullACI))+
  geom_line()+
  facet_wrap(~folder_name)
# In BEI the 2nd and 5th folders have low BEI in the middle of the day compared to the other
# In the 1st and 7th folders, there is a very sharp dip in the middle of the day for one of the days
# No concerning patterns or differences between the audiomoths 

#First graph: showing partial eclipse
#Second graph: Zooming into the time of totality 

ggplot(onlyEclipseTime, aes(x = hour, y = biophony, group = day))+
  geom_rect(xmin =  hms(38,11,14), xmax = hms(38,35,16), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= biophony), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()


ggplot(onlyEclipseTime, aes(x = hour, y = biophony, group = day))+
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= biophony), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()+
  scale_x_continuous(limits = c(hms(00,00,15),hms(00,00,16))) ### To gain a clearer view of totality 

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

# Very sharp decline in the 8th folder at the time the exlipse began, almost no sound afterwards 
### potentially lower than the other days A003, A004, and A005
## Would like more data to really rule out the lack of pattern for this index
## Interestingly there isn't an obvious increase in biophony during the time attributed to dawn either
# potentially the pattern we may be looking for is not what we would expect.


### Acoustic evenness 
### follows same plot setup, partial eclipse, totality, dawn

ggplot(onlyEclipseTime, aes(x = hour, y = aei, group = day))+
  geom_rect(xmin =  hms(38,11,14), xmax = hms(38,35,16), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= aei), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()

ggplot(onlyEclipseTime, aes(x = hour, y = aei, group = day))+
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= aei), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()+
  scale_x_continuous(limits = c(hms(00,00,15),hms(00,00,16))) 

ggplot(onlyDawn, aes(x = hour, y = aei, group = day))+
  geom_rect(xmin =  hms(00,15, 06), xmax = hms(00,30,06), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  facet_wrap(~folder_name)+
  theme_minimal()

## Not really seeing any obvious patterns in these graphs around the time of eclipse. Patterns seem quite similar to the dawn 


### Bioacoustic Evenness

ggplot(onlyEclipseTime, aes(x = hour, y = bei, group = day))+
  geom_rect(xmin =  hms(38,11,14), xmax = hms(38,35,16), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= bei), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()

ggplot(onlyEclipseTime, aes(x = hour, y = bei, group = day))+
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= bei), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()+
  scale_x_continuous(limits = c(hms(00,00,15),hms(00,00,16))) 

ggplot(onlyDawn, aes(x = hour, y = bei, group = day))+
  geom_rect(xmin =  hms(00,15, 06), xmax = hms(00,30,06), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  facet_wrap(~folder_name)+
  theme_minimal()

#### Potential slight increase in BEI once totality begins for some of the folders 
#but nothing relative to the partial eclipse
## dawn shows variation in this index, but a lot of lines seems to fall around 0.5-1.5 range

### Acoustic Complexity 

ggplot(onlyEclipseTime, aes(x = hour, y = fullACI, group = day))+
  geom_rect(xmin =  hms(38,11,14), xmax = hms(38,35,16), ymin = 0, ymax = 3000, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= fullACI), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()+
  scale_y_continuous(limits = c(1500,2000)) 

ggplot(onlyEclipseTime, aes(x = hour, y = fullACI, group = day))+
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3000, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= fullACI), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()+
  scale_x_continuous(limits = c(hms(00,00,15),hms(00,00,16)))

ggplot(onlyDawn, aes(x = hour, y = fullACI, group = day))+
  geom_rect(xmin =  hms(00,15, 06), xmax = hms(00,30,06), ymin = 0, ymax = 3000, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  facet_wrap(~folder_name)+
  theme_minimal()+
  scale_y_continuous(limits = c(1500,2000)) 

## when y-axis is limited we can see that the day of the eclipse has quite low ACI, is this related to the eclipse or because this is the normal? 
## Pattern in the dawn shows there are a lot of days which stick closely to the 1650 mark at all times
# suggests that likely this is not due to the eclipse, maybe more data would solidify this conclusion 

### Acoustic Diversity 

ggplot(onlyEclipseTime, aes(x = hour, y = fullADI, group = day))+
  geom_rect(xmin =  hms(38,11,14), xmax = hms(38,35,16), ymin = 0, ymax = 12, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= fullADI), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()

ggplot(onlyEclipseTime, aes(x = hour, y = fullADI, group = day))+
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 12, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  geom_line(data = onlyEclipseDAY, aes(x = hour, y= fullADI), col = "blue")+
  facet_wrap(~folder_name)+
  theme_minimal()+
  scale_x_continuous(limits = c(hms(00,00,15),hms(00,00,16))) 

ggplot(onlyDawn, aes(x = hour, y = fullADI, group = day))+
  geom_rect(xmin =  hms(00,15, 06), xmax = hms(00,30,06), ymin = 0, ymax = 12, fill = "lightblue", alpha = 0.2)+
  geom_line(alpha = 0.2)+
  facet_wrap(~folder_name)+
  theme_minimal()

## Doesn't seem like there are any obvious patterns relating to the eclipse in this data
## A reduction in ADI around the beginning of the eclipse in folder 8, aligns with biophony?
## Like AEI it seems like the dawn and time during the eclipse match pretty well in their ranges 





