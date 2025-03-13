# Visualizations of the data

# Packages
library(tidyverse)
library(lubridate)
library(hms)
library(forecast)
library(soundecology)
library(here)

# Using only 1 folder - A001_SD001

eclipse_yesno<-A001_SD001|> mutate( eclipse = ifelse(day == "2024-04-08", "yes", "no"))

eclipse_start<-hms(00,00,14)
eclipse_end<-hms(00,50,16)

eclipse_time<-A001_SD001|> filter(hour>= eclipse_start & hour<= eclipse_end) #contains all days through time of eclipse
eclipseonly<-A001_SD001|> filter(hour>= eclipse_start & hour<= eclipse_end)|> filter(day == "2024-04-08") #ONLY 4-8-24 through eclipse


dawn_start<-hms(00,45,05)
dawn_end<-hms(00,15,07)

dawn<-A001_SD001|> filter(hour>= dawn_start & hour<= dawn_end)

##Visualizing BEI
ggplot(data = A001_SD001, aes( x = hour, y = bei))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~date(time))

ggplot(data = eclipse_yesno, aes( x = hour, y = bei))+
  geom_line(aes(col = eclipse, alpha= eclipse))+
  theme_minimal()+
  labs(title = "Full Duration Comparison of BEI")

## Only duration around time of eclipse 

ggplot(data = eclipse_time, aes(x = hour, y = bei, group = day))+
  geom_line(alpha = 0.3)+
  geom_line(data = eclipseonly, aes(x = hour, y = bei), col = "blue", lwd = 1)+
  theme_minimal()+
  labs(title = "Bioacoustic evenness over the Duration of the Eclipse")

## Biophony 

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

## AEI

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

# Creating the full Data frame of all the soundfiles 

fullAudio<-rbind(A001_SD001, A002_SD013, A003_SD005, A004_SD012,A005_SD002, A006_SD006, A007_SD017, 
                 A008_SD007, A009_SD009, A010_SD014, A011_SD018, A013_SD016,A014_SD021, A015_SD010, A016_SD022,
                 A017_SD024, A018_SD011, A019_SD008)|>
  group_by(folder_name)|>
  mutate(fullACI = sapply(ACI_all,sum))|>
  mutate(fullADI = sapply(ADI_all, sum))


onlyEclipseTime<-fullAudio|> filter(hour>= eclipse_start & hour<= eclipse_end)
onlyEclipseDAY<-fullAudio|> filter(hour>= eclipse_start & hour<= eclipse_end)|> filter(day == "2024-04-08")

# Visualizations 
## First graph: showing partial eclipse
## Second graph: Zooming into the time of totality 

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

## Creating a biophony graphic for dawn 
## Dawn segment records ~30 min before and ~45 min after sunrise 


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


### Bioacoustic 

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

# Graphics using geom_smooth()

# Bioacoustic index  
# facet by day and average over the day from all audiomoths
ggplot(data = onlyEclipseTime, aes(x = hour, y = bei)) +
  geom_line(aes(group = folder_name), alpha = 0.4) +
  geom_smooth() +
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2) +
  theme_minimal() +
  facet_wrap(~ day)

# Clear change in the pattern compared to all days in our data set
# Increase during the time of eclipse, the other days do not show this, the lines are much flatter

# Acoustic Evenness
ggplot(data = onlyEclipseTime, aes(x = hour, y = aei)) +
  geom_line(aes(group = folder_name), alpha = 0.4) +
  geom_smooth() +
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2) +
  theme_minimal() +
  facet_wrap(~ day)

# potentially a decrease during the eclipse with a min at the period of totality
# this pattern does appear to happen in some other days (april 1st and 10th)

# Acoustic Diversity 
ggplot(data = onlyEclipseTime, aes(x = hour, y = fullADI)) +
  geom_line(aes(group = folder_name), alpha = 0.4) +
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 10, fill = "lightblue", alpha = 0.2) +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~ day)

# Increase during the period of the eclipse but this seems to occur on the 10th as well
# Other patterns with increases are "wobbly"

# Biophony

ggplot(data = onlyEclipseTime, aes(x = hour, y = biophony)) +
  geom_line(aes(group = folder_name), alpha = 0.4) +
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 10, fill = "lightblue", alpha = 0.2) +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~ day)

# Does not seem like there is any pattern on the day of the eclipse, it is a very straight line
# Other days have a variety of patterns, but the majority have straight smooth lines 

# Acoustic Complexity

ggplot(data = onlyEclipseTime, aes(x = hour, y = fullACI)) +
  geom_line(aes(group = folder_name), alpha = 0.4) +
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3000, fill = "lightblue", alpha = 0.2) +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~ day)

# pattern from the eclipse day is extremely flat, this is similar to many other days in our data set 
# No pattern here

# Deciding on potential subset of the days
# Weather using:  https://www.wunderground.com/history/daily/us/ny/ogdensburg/KOGS/date/2024-4-10
## March 30 - April 4th: Gusts throughout the day 
## April 5th - small amount of rain and gust (9pm)
## April 6th - 9th:  middle of day clear (small amount of rain on the 7th and 8th after 5pm) 
## April 10th: Gusts in the middle of the day, rain in the morning 
## April 11th: rain throughout the afternoon 
## April 12th - 13th: Lots of wind and some rain during morning and middle of day 
## April 14th: gust in the morning and rain ~2 hours after
## April 15th: lighter rain and some gusts (during the 4-6 pm region)
## April 16th: slight rain and one gust (~2pm)

# Based on the weather potentially use a buffer of 2-3 days. Using the 6th and 7th for before the eclipse and the 9th and 11th for after
# Due to the amount of days that have bad weather after the eclipse I think that there are few days to use after the day of the eclipse.
# Could add in 5th, gusts are after 4-6 region
# potentially 14th and 16th, gusts happen earlier than the eclipse 

# potential 5 day subset 
fiveDaySubset<- onlyEclipseTime|>
  filter(day == "2024-04-06" | day == "2024-04-07"| day == "2024-04-08"| day == "2024-04-09" | day == "2024-04-11")

# bei graph using the subset of 5 days 
ggplot(data = fiveDaySubset, aes(x = hour, y = bei)) +
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2) +
  geom_line(aes(group = folder_name), alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~ day)

# Other potential metrics with patterns? 
ggplot(data = fiveDaySubset, aes(x = hour, y = aei)) +
  geom_line(aes(group = folder_name), alpha = 0.4) +
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2) +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~ day)

ggplot(data = fiveDaySubset, aes(x = hour, y = fullADI)) +
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 10, fill = "lightblue", alpha = 0.2) +
  geom_line(aes(group = folder_name), alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~ day)




