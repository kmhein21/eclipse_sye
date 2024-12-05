## Task List

## For December 12

1. Keep working on write-up

2. Start analyzing output from the single audiomoth (assuming the HPC analysis runs).

## For November 21

1. Get test .run file working on HPC.

2. Upload small folder of .wav files to HPC.

3. Get the script with the function to run on the HPC (and can get rid of the array line in the .run file as well).

## For November 14

0. Install Filezilla (or some other free FTP service).

0.5. Remove here from .rds file (and also make sure that it's working).

1. Run test .run file on HPC.

2. MH: Make sure home directory is set up. Send HPC directions.

2.5. MH: Look more at write-up.

3. Install necessary R packages (to make sure there are no issues).

## For November 7

1. Make a variable that has the folder name.

2. MH: look into naming `final` something else for loading in and getting an HPC account.

3. Start short write-up (introducing the data using erika's methods, talking about the different sound metric, introducing some of the coding work you've done so far, in a .qmd file).

4. MH: Make sure that you have an HPC account and I'll figure out the password for my account.


## For October 31

1. Clean up file structure.

2. Fix up the function to work with other folders and with general n.

3. Restart R (Sesion -> Restart) and run the function to make sure it works on a "clean slate".

4. Use `map()` with a vector of two folder names to run the function "twice", once for each folder.

## For October 24

"Half Week"

1. Clean up file structure.

"Full Week"

2. Write function with folder name as input and with .rda file of cleaned data as output (put in a .R file).

## For October 10

1. Fix up the plot (put both days on one plot).

2. Plot the other indices for each day.

3. Change .csv's to .rda's.

## For October 3

1. Download the two large folders (separately). Write a data frame that has as much info as you can, and then delete the large folders.

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

