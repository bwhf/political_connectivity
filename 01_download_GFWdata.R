# Packages ----
library(tidyverse)
library(furrr)
library(lubridate)
library(dlookr)

# Load and combine csv file ----
in_dir <- c("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month") # set input directory
files <- dir(in_dir, full.names = TRUE) # get file names
splist <- basename(files) # create string of file name
dat <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)
all_df <- do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = F)))

# Function to extract year from file name
extract_year <- function(filename) {
  # Extract year from the file name assuming the format "fhYYYY-month-..."
  match <- regexpr("\\d{4}", basename(filename))
  year <- regmatches(basename(filename), match)
  return(year)
}

# Read and combine CSV files by year
combined_data <- files %>%
  # Create a data frame with file names and their corresponding years
  tibble(file = ., year = sapply(., extract_year)) %>%
  # Group by year
  group_by(year) %>%
  # Read and combine CSV files for each year
  do({
    year_files <- .$file
    # Read all files for the current year
    year_data <- lapply(year_files, read_csv)
    # Combine all data frames for the current year
    combined_year_data <- bind_rows(year_data)
    combined_year_data
  }) %>%
  ungroup()

## retain only drifting longline
drifting_longlines_1418 <- combined_data %>%
  filter(geartype == "drifting_longlines") 

# Replace spaces with underscores in column names
names(drifting_longlines_1418) <- gsub(" ", "_", names(drifting_longlines_1418))

## check duplicates
# Identify duplicates, especially at 0, 180, and 360 degrees
duplicates <- drifting_longlines_1418 %>%
  group_by(Lat, Lon, Time_Range, Vessel_IDs, Apparent_Fishing_Hours) %>% # Adjust column names as needed
  filter(n() > 1) %>%
  arrange(Lat, Lon, Time_Range)

# Remove duplicates from combined_data
unique_longlines <- drifting_longlines_1418 %>%
  group_by(Lat, Lon, Time_Range, Vessel_IDs, Apparent_Fishing_Hours) %>% # Adjust column names as needed
  distinct(.keep_all = TRUE)

# export longlines data from 2014 to 2018
write_csv(unique_longlines, "/Users/bwhf/Downloads/fh_month/fishhour_month/fh2014_2018_longlines_res01.csv")
