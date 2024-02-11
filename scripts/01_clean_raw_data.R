library(tidyverse)


# Dance data --------------------------------------------------------------

## Load and clean data ---------------------------------------------------------------

data_loc <- "data/raw/dance/" # Specify raw data folder
data_fn <- list.files(data_loc, pattern = "*.csv", recursive = TRUE) # Get csv file names in raw data folder

dat <- data.frame("file" = data_fn) %>% # Create data frame with csv file names
  mutate(location = str_split(file, pattern="_", simplify = T)[,4]) %>% # Split the file name and keep only 4th element containing location
  mutate(location = str_sub(location, end = -5)) %>%  # Remove the .csv part from the location string
  mutate(data = lapply(paste(data_loc, file, sep=""), read.csv)) %>% # Add location to file name and apply read.csv over all rows
  unnest(data) %>% dplyr::select(-c(file, starts_with("Mean_"))) %>% # Unnest data column and remove file name and other columns
  rename_with(str_to_lower, everything()) %>% # Convert all column names to lower case
  rename("lineage" = "population", "colonyID" = "colony_id", "distance" = "distance.m.", "beeID" = "bee_id", 
         "dance" = "dance_no..", "phase" = "waggle_run_no.", "duration" = "waggle_run_duration.s.") %>% # Rename specific columns
  filter(!is.na(phase)) %>% # Filter out rows where phase is NA
  mutate(duration = frame_difference/60) # Corrected duration calculation to use 60fps instead of 59.9

str(dat)

## Output CSV --------------------------------------------------------------

write.csv(dat, "data/temp/acerana_lineage_dance_duration.csv", row.names = F)


# Morphometric data -------------------------------------------------------


## Load and clean data -----------------------------------------------------

morph <- read.csv("data/raw/morphometric/morph_data_all.csv") %>% 
  rename_with(str_to_lower, everything()) %>% # Convert all column names to lower case
  rename("colonyID" = "colony_id", "beeID" = "bee_id", "location" = "experiment_location") %>% 
  mutate(colonyID = case_when(
    location=="Kullu" & colonyID==4~1,
    location=="Kullu" & colonyID==5~2,
    TRUE~colonyID
  )) %>% # Replace colonyID of Kullu populations with 1 and 2 instead of 4 and 5
  separate(beeID, c("pop", "loc", "col", "beeID")) %>% select(-c(pop, loc, col)) # Keep only ID number for beeID

str(morph)

## Output CSV --------------------------------------------------------------

write.csv(morph, "data/temp/acerana_lineage_morphometric.csv", row.names = F)
