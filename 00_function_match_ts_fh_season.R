# Function to match bird time spent with fishing effort seasonally

library(dplyr)
library(fuzzyjoin)

# Function to process data for a single season
process_season <- function(season, bird_data, fishing_data) {
  # Filter data for the specific season
  bird_season <- bird_data %>% filter(season == !!season)
  fishing_season <- fishing_data %>% filter(season == !!season)
  
  # Perform the fuzzy left join by cell
  joined_data <- fuzzy_left_join(
    x = bird_season,
    y = fishing_season,
    by = c("cell"),  # Match by cell ID
    match_fun = list(`==`)
  )
  
  # Return the joined data
  return(joined_data)
}

# Main script to process all seasons
process_all_seasons <- function(bird_data, fishing_data) {
  # Get unique seasons
  seasons <- unique(bird_data$season)
  
  # Apply the process_season function to each season and combine results
  all_seasons_data <- lapply(seasons, function(season) {
    process_season(season, bird_data, fishing_data)
  }) %>%
    bind_rows()
  
  return(all_seasons_data)
}

# Example usage
# Assuming `bird_data` and `fishing_data` are preloaded
# bird_data: Bird time spent data with columns `cell`, `season`, etc.
# fishing_data: Fishing effort data with columns `cell`, `season`, etc.
# Example usage
# Assuming `bird_data` and `fishing_data` are preloaded
# bird_data: Bird time spent data with columns `cell`, `season`, etc.
# fishing_data: Fishing effort data with columns `cell`, `season`, etc.

# Define the function
match_front_frequency <- function(grid, front_freq_base_path, output_base_path, seasons) {
  # Initialize an empty list to store the processed data for each season
  season_results <- list()
  
  # Loop through each season
  for (season in seasons) {
    # 1. Read the front frequency file for the current season
    front_freq_file <- paste0(front_freq_base_path, "raster_front_freq_18_", season, ".rds")
    front_freq <- read_rds(front_freq_file)
    front_freq <- front_freq %>% 
      group_by(cell) %>%
      summarise(front_freq = mean(front_freq))
    
    # 2. Filter the grid_ data for the current season
    grid_season <- grid %>% filter(season == !!season)
    
    # 3. Match front frequency with grid for the current season
    matched_data <- merge(grid_season, front_freq, by.x = "cell", all.x = TRUE)
    
    # 4. Save the processed data for this season
    season_results[[season]] <- matched_data
    print(paste0(season))
  }
  
  # 5. Combine all seasons into one dataframe
  final_combined_data <- bind_rows(season_results, .id = "season")
  
  # Return the combined dataframe
  return(final_combined_data)
}

match_fronts <- function(grid, front_data, output_base_path, seasons) {
  # Initialize an empty list to store the processed data for each season
  season_results <- list()
  
  # Loop through each season
  for (season in seasons) {
    # 1. Read the front frequency file for the current season
    front_var <- front_data
    
    # 2. Filter the grid_ data for the current season
    grid_season <- grid %>% filter(season == !!season)
    front_var <- front_var %>% filter(season == !!season)
    
    # 3. Match front var with grid for the current season
#    matched_data <- merge(grid_season, front_var, by.x = "cell", all.x = TRUE)
    matched_data <- fuzzy_left_join(grid_season, front_var, by = c("cell"), match_fun = `==`)
    
    # 4. Save the processed data for this season
    season_results[[season]] <- matched_data
    print(paste0(season))
  }
  
  # 5. Combine all seasons into one dataframe
  final_combined_data <- bind_rows(season_results, .id = "season")
  
  # Return the combined dataframe
  return(final_combined_data)
}
