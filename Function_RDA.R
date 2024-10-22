test_fun<- function(folder) {
  f_list<- list.files(folder, pattern = "\\.WAV$", full.names = TRUE)
  paths_date <-str_remove(f_list, paste0(here(), "/test_wavs"))
  
  WAV_f<-map(f_list, readWave)
  AEI<-map(WAV_f, acoustic_evenness)
  BEI<-map(WAV_f, bioacoustic_index)
  #ADI<-map(WAV_f, acoustic_diversity)
  #ACI<-map(WAV_f, acoustic_complexity)
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
  
  single<- as.tibble(cbind(paths_date, AEI_df, BEI_df, Biophony_df))
  print(single)
} 

test_fun(here("test_wavs"))
