library(tidyverse)
library(hms)
A001_SD001 <- readRDS("~/Desktop/SYE_Heintzman/rds_files/A001_SD001.rds")
A002_SD013 <- readRDS("~/Desktop/SYE_Heintzman/rds_files/A002_SD013.rds")
A003_SD005 <- readRDS("~/Desktop/SYE_Heintzman/rds_files/A003_SD005.rds")
A004_SD012 <- readRDS("~/Desktop/SYE_Heintzman/rds_files/A004_SD012.rds")
A006_SD006 <- readRDS("~/Desktop/SYE_Heintzman/rds_files/A006_SD006.rds")
A007_SD017 <- readRDS("~/Desktop/SYE_Heintzman/rds_files/A007_SD017.rds")
A008_SD007 <- readRDS("~/Desktop/SYE_Heintzman/rds_files/A008_SD007.rds")
A005_SD002 <- readRDS("~/Desktop/SYE_Heintzman/rds_files/A005_SD002.rds")


fullAudio<-rbind(A001_SD001, A002_SD013, A003_SD005, A004_SD012,A005_SD002, A006_SD006, A007_SD017, A008_SD007)|>
  group_by(folder_name)|>
  mutate(fullACI = sapply(ACI_all,sum))|>
  mutate(fullADI = sapply(ADI_all, sum))

eclipse_start<-hms(00,00,14)
eclipse_end<-hms(00,50,16)

eclipse_df <- fullAudio |>
  filter(day %in% c("2024-04-06", "2024-04-07", "2024-04-08",
                    "2024-04-09", "2024-04-10") & hour >= eclipse_start & hour <= eclipse_end)
  
## very gusty on april 10:
## https://www.wunderground.com/history/daily/us/ny/ogdensburg/KOGS/date/2024-4-10
## 
## think about putting on plot when sun was 75% covered by the moon
eclipse_df |> count(folder_name)
ggplot(data = eclipse_df, aes(x = hour, y = bei)) +
  geom_line(aes(group = folder_name), alpha = 0.4) +
  geom_smooth() +
  geom_rect(xmin =  hms(52,23,15), xmax = hms(05,27,15), ymin = 0, ymax = 3, fill = "lightblue", alpha = 0.2) +
  theme_minimal() +
  facet_wrap(~ day)

