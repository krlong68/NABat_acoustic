# reformat.R
# Usage: see README.md

# Library imports

# general data handling
library(dplyr)
library(readr)

# date-time handling
library(lubridate)

#---------------------
# EDIT THESE VARIABLES
#---------------------

# Set the working directory to the parent directory of this script
setwd("/home/kaelyn/Desktop/Bats_NW/NABat_acoustic")

# Year the survey was performed
year <- "2025"

# ID of the grid the survey was performed in
grid_id <- "113851"

# Species list used for both automatic and manual species identification
spec_list <- "PUGET_SOUND_MOBILE_SONOBAT_PACNW-JEFFERSON_WEST_WA[20250526]"

#-------------
# STOP EDITING
#-------------

# Load in TXT files
sonobat_txt_dir <- file.path(getwd(), "sonobat_txt", year, grid_id)
sonobat_txt_files <- list.files(sonobat_txt_dir, full.names = TRUE)

sbat_list <- lapply(sonobat_txt_files, function(x) {
    read_delim(x, col_types = cols(.default = "c"))
}) |>
    setNames(basename(sonobat_txt_files))

# Clean the timestamp:
# Remove GMT offset
# Confirm that the year matches the supplied year
clean_ts <- function(timestamp, year, verbose = FALSE) {
    # Remove GMT offset
    sub_ts <- gsub("-[0-9]{1,2}?:[0-9]{1,2}?$", "", timestamp)
    
    # Convert value to POSIXct
    parsed <- parse_date_time(sub_ts, "YmdHMS")

    # Check that the year is correct
    if (year(parsed) != year) {
        old_year <- year(parsed)
        year(parsed) <- year
        
        if (verbose) {
            message("Year should be ", year, " but is ", old_year,
                    ". Value will be corrected.")
        }
    }
    
    # Convert POSIXct to character value and return
    char_date <- format_ISO8601(parsed)
    
    return(char_date)
}

# Format metadata according to the NABat-supplied template
create_nabat_data <- function(sonobat_df, year, spec_list, verbose = FALSE) {
    # List all columns present in NABat template
    nabat_cols <- c("| GRTS Cell Id", "Surveyor(s)", "Latitude", "Longitude",
                    "Site Name", "Survey Start Time", "Survey End Time",
                    "Unusual Occurrences", "Significant Weather Event",
                    "Auto Id Software", "Auto Id", "Manual Id",
                    "Manual Id Vetter", "Name of Species List for Auto Id",
                    "Name of Species List for Manual Id", 
                    "Audio Recording Name", "Audio Recording Time",
                    "Detector Model", "Detector Serial Number",
                    "Microphone Model", "Microphone Serial Number", 
                    "Microphone Orientation")
    
    # Fill each column with the appropriate information
    # Most columns are filled from Sonobat TXT files
    # Some columns are filled with predetermined values
        # Auto Id Software, Microphone Model, Microphone Orientation
    # Some columns are filled with user-defined values
        # Name of Species List for Auto Id, Name of Species List for Manual Id 
    # Some columns are purposely left as NA
        # | GRTS Cell Id, Latitude, Longitude, Microphone Serial Number
    nbdf <- sonobat_df %>%
        rowwise() %>%
        mutate(`| GRTS Cell Id` = NA,
               `Surveyor(s)` = gsub(",", " ", `NABat|Surveyor`),
               Latitude = NA,
               Longitude = NA,
               `Site Name` = `NABat|Site Name`,
               `Survey Start Time` = clean_ts(`NABat|Start Time`,
                                              year, verbose),
               `Survey End Time` = clean_ts(`NABat|End Time`,
                                            year, verbose),
               `Unusual Occurrences` = `NABat|Unusual Occurrences`,
               `Significant Weather Event` = `User|Significant Weather Event`,
               `Auto Id Software` = "SonoBat 30.2.x",
               `Auto Id` = `SB|Species Auto ID verbose`,
               `Manual Id` = `Species Manual ID`,
               `Manual Id Vetter` = gsub(",", "_", `NABat|Vetter`),
               `Name of Species List for Auto Id` = if_else(!is.na(`Auto Id`), spec_list, NA),
               `Name of Species List for Manual Id` = if_else(!is.na(`Manual Id`), spec_list, NA),
               `Audio Recording Name` = Filename,
               `Audio Recording Time` = Timestamp,
               `Detector Model` = `NABat|Detector Model`,
               `Detector Serial Number` = `NABat|Detector Serial Number`,
               `Microphone Model` = "generic internal",
               `Microphone Serial Number` = NA,
               `Microphone Orientation` = "backward"
               ) %>%
        select(all_of(nabat_cols))
    
    # Replace NA with the empty string "" and return
    nbdf[is.na(nbdf)] <- ""
    
    return(nbdf)
}

# Format user-inputted year as numeric for timestamp comparison
num_year <- as.numeric(year)

# Run formatting function on all loaded Sonobat data
nb_list <- lapply(sbat_list, function(x) create_nabat_data(x, num_year,
                                                           spec_list,
                                                           verbose = FALSE))

# Save to CSVs
# Set output directory
out_csv_dir <- file.path(getwd(), "out_csv", year, grid_id)

for (i in seq_along(nb_list)) {
    # Set output file name according to NABat standards
    fname <- names(nb_list)[i] |>
        gsub(pattern = "Session", replacement = paste0(grid_id, "_Mobile")) |>
        gsub(pattern = "-Attributed.txt", replacement = ".csv")
    
    # Write output file
    write_csv(nb_list[[i]], file.path(out_csv_dir, fname))
}

