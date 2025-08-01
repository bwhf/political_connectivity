library(tidyverse)

# Set the path to your folder (change this to your actual folder path)
folder_path <- "/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/figures/sp.site_annual_coverage/"

# List all .png files in the folder
file_names <- list.files(path = folder_path, pattern = "\\.png$", full.names = FALSE)

# Extract species and colony from file names
species_colony_tbl <- tibble(file = file_names) %>%
  mutate(file_no_ext = str_remove(file, "\\.png$")) %>%
  separate(file_no_ext, into = c("Species", "Colony"), sep = "_", extra = "merge")

# View the result
print(species_colony_tbl)

##### 
# create list of scientific name
scientific_lookup <- tribble(
  ~Common, ~Scientific,
  "Amsterdam Albatross", "Diomedea amsterdamensis",
  "Antipodean Albatross", "Diomedea antipodensis",
  "Atlantic Yellow-nosed Albatross", "Thalassarche chlororhynchos",
  "Black-browed Albatross", "Thalassarche melanophris",
  "Black-footed Albatross", "Phoebastria nigripes",
  "Buller's Albatross", "Thalassarche bulleri",
  "Campbell Albatross", "Thalassarche impavida",
  "Chatham Albatross", "Thalassarche eremita",
  "Cory's Shearwater", "Calonectris borealis",
  "Flesh-footed Shearwater", "Ardenna carneipes",
  "Great Shearwater", "Ardenna gravis",
  "Grey Petrel", "Procellaria cinerea",
  "Grey-headed Albatross", "Thalassarche chrysostoma",
  "Indian Yellow-nosed Albatross", "Thalassarche carteri",
  "Laysan Albatross", "Phoebastria immutabilis",
  "Light-mantled Albatross", "Phoebetria palpebrata",
  "Northern Giant Petrel", "Macronectes halli",
  "Northern Royal Albatross", "Diomedea sanfordi",
  "Pink-footed Shearwater", "Ardenna creatopus",
  "Scopoli's Shearwater", "Calonectris diomedea",
  "Short-tailed Albatross", "Phoebastria albatrus",
  "Shy Albatross", "Thalassarche cauta",
  "Sooty Albatross", "Phoebetria fusca",
  "Southern Giant Petrel", "Macronectes giganteus",
  "Spectacled Petrel", "Procellaria conspicillata",
  "Tristan Albatross", "Diomedea dabbenena",
  "Wandering Albatross", "Diomedea exulans",
  "Waved Albatross", "Phoebastria irrorata",
  "White-capped Albatross", "Thalassarche steadi",
  "White-chinned Petrel", "Procellaria aequinoctialis"
)

 

# match scientific name
species_colony <- species_colony_tbl %>%
  left_join(scientific_lookup, by = c("Species" = "Common")) %>%
  rename(scientific_name = "Scientific")

# match iucn status
species_colony <- species_colony %>%
  left_join(seabird_iucn_cat, by = c("scientific_name" = "scientific_name"))

species_colony <- species_colony %>% dplyr::select(-file) %>% select(Species, scientific_name, category, Colony)

#####
species_count <- species_colony %>% group_by(Species, category) %>% summarise(n = n())

write_csv(species_colony, "/Users/bwhf/Documents/GitHub/political_connectivity/data/species_status/species_by_colony.csv")
