## birdnet info: https://github.com/kahst/BirdNET-Analyzer#setup-ubuntu
## don't really need the birdnet info right now


## soundecology package info: 
## https://cran.r-project.org/web/packages/soundecology/vignettes/intro.html

install.packages("soundecology")
library(soundecology)

#Call the Wave object into memory
data(tropicalsound)

#Run the function
acoustic_complexity(tropicalsound)

acoustic_complexity(tropicalsound, max_freq = 8000)


## should put .wav files in a /data folder
soundfile <- readWave(here::here("Eclipse-data-for-Matt/20240401_061600.wav"))

soundfile.aci <- acoustic_complexity(soundfile)
result <- ndsi(soundfile)
