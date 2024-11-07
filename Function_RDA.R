library(soundecology)
library(tidyverse)
library(here)
library(hms)
library(tuneR)

eclipse_df<- function(folder) {
  f_list<- list.files(folder, pattern = "\\.WAV$", full.names = TRUE)
  paths_date<- str_remove(f_list, paste0(here(folder)))
  #a<-deparse(substitute(folder))
  #b<-str_remove(a, "here\\(")
  #c<-str_remove(b,"\\)")
  #output<-paste0(c,".rda")
  output<- paste0(deparse(substitute(folder)),".rds")
  
  WAV_f<-map(f_list, readWave)
  AEI<-map(WAV_f, acoustic_evenness)
  BEI<-map(WAV_f, bioacoustic_index)
  ADI<-map(WAV_f, acoustic_diversity)
  ACI<-map(WAV_f, acoustic_complexity)
  ndsi<-map(WAV_f, soundecology::ndsi)
  
  AEI_df<-as.data.frame(AEI)|>
    select(starts_with("aei_left"))|>
    pivot_longer(everything(), names_to = "aei_name", values_to = "aei")|>
    select("aei")
  
  BEI_df<-as.data.frame(BEI)|>
    select(starts_with("left_area"))|>
    pivot_longer(everything(), names_to = "bei_name", values_to = "bei")|>
    select("bei")
  
  Biophony_df<-as.data.frame(ndsi)|>
    select(starts_with("biophony_left"))|>
    pivot_longer(everything(), names_to = "bio_name", values_to = "biophony")|>
    select("biophony")
  
  single<- as_tibble(cbind(paths_date, AEI_df, BEI_df, Biophony_df))
  
  ACI_all<-vector("list", length(folder))
  ADI_all<-vector("list", length(folder))
  n<-length(folder)
  for (i in 1:n){
    ACI_all[i]<-(as.data.frame(ACI[[i]]$aci_fl_left_vals))
    ADI_all[i]<-(as.data.frame(ADI[[i]]$left_band_values))
  } 
  multiple<-as_tibble(cbind( ACI_all, ADI_all))
  full<-bind_cols(single, multiple)

  
  final<- full|> mutate(biophony = as.numeric(biophony),
                        aei = as.numeric(aei),
                        bei = as.numeric(bei))|>
    separate(paths_date, into = c("date","time_hms"), sep = "_")|>
    separate(time_hms, into = c("time", "wav"), sep = "\\.")|>
    separate(time, into = c("hours", "other"), sep = 2)|>
    separate(other, into = c("min", "sec"), sep = 2)|>
    mutate(date = parse_number(date))|>
    unite("time", c("date", "hours", "min", "sec"), sep = ":")|>
    mutate(time= ymd_hms(time))|>
    select(-wav)|>
    mutate(folder_name = deparse(str_remove(folder, here())))|>
    select(folder_name, everything())
  
  saveRDS(final, file = output)
}

eclipse_df(here("test_wavs"))
eclipse_df(here("WAV_5_test"))

folder_group<-c(here("test_wavs"),here("WAV_5_test"))

map(folder_group, eclipse_df)
