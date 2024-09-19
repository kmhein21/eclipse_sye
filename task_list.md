## Task List

## For September 26

1. Add biophyny.

2. Clean up code a little bit.

3. Get data from erika!

## For September 19

1. Matt: Email erika for more .WAV files.

2. Figure out how to "map" through the 10 .WAV files, pulling ACI and possibly other acoustics.

3. See if the ACI files "make sense" with the .WAV sounds.

Code: 

paths <- list.files(here("Eclipse-data-for-Matt"), pattern = "\\.WAV$",
                    full.names = TRUE)

wav_files <- purrr::map(paths, readWave)

acoustic_complexity(wav_files[[1]])

# tibble::tibble(test_var = c(list(c(1, 2, 3)), list(4, 5))) |>
#   tidyr::unnest(test_var)

## For September 12

1. Create a line plot of ACI for one .wav file.

2. Create a multiple line plots of ACI for a few .wav files.

3. Investigate some of the other options besides ACI to graph from the .wav file.

## For September 5

1. Read Eclipse Paper (Gerber).

2. Familiarize yourself with soundecology package.

