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

# Clean the timestamp by removing GMT offset
#clean_ts <- function(timestamp) {
#    # Remove GMT offset
#    sub_ts <- gsub("-[0-9]{1,2}?:[0-9]{1,2}?$", "", timestamp)
#    
#    # Convert value to POSIXct
#    parsed <- parse_date_time(sub_ts, "YmdHMS")
#    
#    # Convert POSIXct to character value and return
#    char_date <- format_ISO8601(parsed)
#    
#    return(char_date)
#}

# Check all date-time columns to confirm they are reasonable
# Survey Start Time, Survey End Time, Audio Recording Time
check_dates <- function(df, year) {
    # Initialize full dataframe error storage
    df_errors <- c()
    
    for (i in seq_len(nrow(df))) {
        cur_row <- df[i,]
        
        # Convert relevant values to POSIXct
        parsed_ts <- parse_date_time(cur_row$`Audio Recording Time`, c("mdYHM", "YmdHMSz"))
        parsed_start <- parse_date_time(cur_row$`Survey Start Time`, "YmdHMS")
        parsed_end <- parse_date_time(cur_row$`Survey End Time`, "YmdHMS")
        
        # Initialize row error storage
        errors <- c()
        
        # Check that all times occur within the provided year
        if (year(parsed_ts) != year) {
            errors <- c(errors,
                        "Recording timestamp does not occur within the provided year")
        }
        
        if (year(parsed_start) != year) {
            errors <- c(errors,
                        "Survey start does not occur within the provided year")
        }
        
        if (year(parsed_end) != year) {
            errors <- c(errors,
                        "Survey end does not occur within the provided year")
        }
        
        # Check that all times are on the same date
        if (date(parsed_end) != date(parsed_start)) {
            errors <- c(errors,
                        "Survey end is not on the same date as survey start")
        }
        
        if (date(parsed_ts) != date(parsed_start)) {
            errors <- c(errors,
                        "Recording timestamp is not on the same date as the survey")
        }
        
        if ((parsed_end - parsed_start) < 0) {
            errors <- c(errors,
                        "Survey end occurs before survey start")
        }
        
        # Check that all times are in the correct order
        survey_interval <- interval(parsed_start, parsed_end)
        
        if (!parsed_ts %within% survey_interval) {
            errors <- c(errors,
                        "Recording timestamp does not occur within the survey interval")
        }
        
        # Save errors
        row_errors <- paste(errors, collapse = ";")
        if (row_errors == "") { row_errors <- NA }
        df_errors <- c(df_errors, row_errors)
    }
    
    # Add errors to dataframe and return
    df$errors <- df_errors
    
    return(df)
}

# Format metadata according to the NABat-supplied template
create_nabat_data <- function(sonobat_df, spec_list) {
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
               Latitude = strsplit(Lat, " ")[[1]][1],
               Longitude = strsplit(Lat, " ")[[1]][2],
               `Site Name` = `NABat|Site Name`,
               `Survey Start Time` = gsub("-[0-9]{1,2}?:[0-9]{1,2}?$", "",
                                          `NABat|Start Time`),
               `Survey End Time` = gsub("-[0-9]{1,2}?:[0-9]{1,2}?$", "",
                                        `NABat|End Time`),
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

# Run formatting function on all loaded Sonobat data
nb_list <- lapply(sbat_list, function(x) create_nabat_data(x, spec_list))

# Format user-inputted year as numeric for timestamp comparison
num_year <- as.numeric(year)

# Check for unreasonable dates
checked_list <- lapply(nb_list, function(x) check_dates(x, num_year))

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

