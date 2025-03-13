## Task List

## For March 27

1. Continue write up on fitted model. Keep in mind if there is a statisical test showing that the eclipse day has a different pattern.

2. Fit model and make the plots (adding the moon!) for the other metrics (all 5).

3. Keep in mind if there is a statistical test for difference in "wiggliness" across the different days.

4. Make plot of raw eclipse data (time series plot of data on day of eclipse to see natural variability in the metric).

## For March 13

1. Use `scale_x_continuous()`? to change what appears as the tick marks on the plot.

2. Moon phases on plot (set the max y-axis value to be some number, look into different types of points in R, create 5 data frames, each one row, with the hour_numeric and the max y-axis value as variables). Then use 5 geom_points() with different shapes.

3. Write up on fitted model. Keep in mind if there is a statisical test showing that the eclipse day has a different pattern.



## For March 6

1. Read 7.7 and Lab 7.8.3 on GAM models.

2. Fit some gam models (even if they are not going to be our final model) on the eclipse data.

3. Review eclipse 207 paper for how they fit the GAM models to their data.

## For February 27

1. Visualize each metric with all 15 audiomoth data sets.

2. Think more about which dates to include in the plot.

## For February 6

1. Figure out why ACI and ADI values are the same for each file (look at vector/list indexing).

2. Run about 5 more audiomoths on the HPC.

3. Plot all 5 indices and write a short description about each (remind ourselves which one the paper found to be different).

## For January 30

1. Run about 5 or so audiomoths on the HPC.

2. Compile the 5 data sets into one .rds file.

3. Do some visualization with a faceted (by audiomoth) plot.


## For December 19

1. Clean up the visualization code a little bit.

2. Modify function to add time and day.

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

