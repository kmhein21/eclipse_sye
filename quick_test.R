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
library(tuneR)

## should put .wav files in a /data folder
soundfile <- readWave(here::here("Eclipse-data-for-Matt/20240401_061600.wav"))

soundfile.aci <- acoustic_complexity(soundfile)
summary(soundfile.aci)
result <- ndsi(soundfile) ## not this one

## guessing that right is missing because left is just the total
## all left is amount of biophyny in the entire 50 second clip
## by min divides by (50/60)
## aci_fl_left_vals is amount of biophyny in a tiny interval within the clip