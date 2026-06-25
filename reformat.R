# general data handling
library(dplyr)
library(readr)
#library(tidyr)

# date-time handling
library(lubridate)
#library(hms)

setwd("/home/kaelyn/Desktop/Bats_NW/NABat_acoustic")

# Load in TXT files
sonobat_txt_dir <- file.path(getwd(), "sonobat_txt", "2025")
sonobat_txt_files <- list.files(sonobat_txt_dir, full.names = TRUE)

sbat_list <- lapply(sonobat_txt_files, function(x) {
    read_delim(x, col_types = cols(.default = "c"))
}) |>
    setNames(basename(sonobat_txt_files))

# Load in NABat template
#nabat_temp_file <- file.path(getwd(), "Bulk_Mobile_Acoustic_Full_Template.csv")
#nabat_temp <- read_csv(nabat_temp_file)

# Reformat any fields necessary to conform to NABat template

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

create_nabat_data <- function(sonobat_df, year, verbose = FALSE) {
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
               `Auto Id Software` = `NABat|Auto ID Software`,
               `Auto Id` = `SB|Species Auto ID verbose`,
               `Manual Id` = `Species Manual ID`,
               `Manual Id Vetter` = gsub(",", " ", `NABat|Vetter`),
               `Name of Species List for Auto Id` = `NABat|Name of Species List for Auto ID`,
               `Name of Species List for Manual Id` = `NABat|Name of Species List for Manual ID`,
               `Audio Recording Name` = Filename,
               `Audio Recording Time` = Timestamp,
               `Detector Model` = `NABat|Detector Model`,
               `Detector Serial Number` = `NABat|Detector Serial Number`,
               `Microphone Model` = "generic internal",
               `Microphone Serial Number` = NA,
               `Microphone Orientation` = "backward"
               ) %>%
        select(all_of(nabat_cols))
}
