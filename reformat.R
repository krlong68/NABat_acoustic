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
## Cleanup TODO: allow for no year to be supplied
## Cleanup TODO: replace species list if !is.na for both auto and manual ID

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

create_nabat_data <- function(sonobat_df, year, spec_list, verbose = FALSE) {
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
               `Auto Id Software` = "SonoBat 30.2.x",
               `Auto Id` = `SB|Species Auto ID verbose`,
               `Manual Id` = `Species Manual ID`,
               `Manual Id Vetter` = gsub(",", " ", `NABat|Vetter`),
               `Name of Species List for Auto Id` = spec_list,
               `Name of Species List for Manual Id` = NA,
               `Audio Recording Name` = Filename,
               `Audio Recording Time` = Timestamp,
               `Detector Model` = `NABat|Detector Model`,
               `Detector Serial Number` = `NABat|Detector Serial Number`,
               `Microphone Model` = "generic internal",
               `Microphone Serial Number` = NA,
               `Microphone Orientation` = "backward"
               ) %>%
        select(all_of(nabat_cols))
    
    nbdf[is.na(nbdf)] <- ""
    
    return(nbdf)
}

year <- 2025
grid_id <- "113851"
spec_list <- "PUGET_SOUND_MOBILE_SONOBAT_PACNW-JEFFERSON_WEST_WA[20250526]"

nb_list <- lapply(sbat_list, function(x) create_nabat_data(x, year, spec_list,
                                                           verbose = FALSE))

# Save to CSVs
out_csv_dir <- file.path(getwd(), "out_csv", "2025")

for (i in seq_along(nb_list)) {
    fname <- names(nb_list)[i] |>
        gsub(pattern = "Session", replacement = paste0(grid_id, "_Mobile")) |>
        gsub(pattern = "-Attributed.txt", replacement = ".csv")
    
    write_csv(nb_list[[i]], file.path(out_csv_dir, fname))
}
