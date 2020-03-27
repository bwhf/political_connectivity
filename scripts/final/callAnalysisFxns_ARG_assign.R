#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Script to re-run analysis, assigning disputed breeding areas to Argentina ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#----------------------------------------------------------------------------#

assign <- "ARG"

## ANALYSIS STEPS ## ---------------------------------------------------------

# Step 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Overlay each data set on an EEZ polygon layer ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("scripts/final/source_fxns/overJur_fxn.R")

folder <- "data/analysis/month_filtered/"

files <- list.files(folder)

## select one species to test 
# species <- "Calonectris borealis"
# files <- list.files(folder)
# files <- files[str_detect(files, species)]

# EEZ analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
out <- "data/analysis/ARG_assign/oveez/"
## Unioned land and EEZ dataset ## (http://www.marineregions.org/downloads.php)
eez_cnt <- raster::shapefile("spatial_data/shapefiles_EEZ_countries/union_countries_EEZs/EEZ_land_v2_201410_FIX_wlandlocked.shp")

overJur(inFolder = folder, files=NULL, over_which = "EEZ", spatial = eez_cnt, filter_landlocked=TRUE, assign="ARG", outFolder = out)


# RFMO analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NOTE: must first run EEZ analysis for this to work

# individual RFMO shapefiles (http://www.fao.org/geonetwork/srv/en/main.home?uuid=cc7dbf20-1b8b-11dd-8bbb-0017f293bd28)
shp_folder <- "spatial_data/RFMOs_ofinterest" # folder containing individual RFMO polygon shapefiles
rfmos <- do.call("rbind", str_split(tools::list_files_with_exts(shp_folder, "shp", full.names = F), pattern = fixed(".")))[,1]

# make output folders for each RFMO (need a vector of RFMO names ['rfmos'])
# for(i in 1:length(rfmos)){
#   one <- rfmos[[i]]
#   dir.create(paste0("data/analysis/ARG_assign/ovrfmo/", one))
# }

# filter to High seas points only # ~~~~~~~~~~~~~~~~~~~
ins <- "data/analysis/ARG_assign/oveez/"   
out <- "data/analysis/ARG_assign/oveez_hs/"

files <- list.files(ins, full.names = T)
filename <- list.files(ins, full.names = F)

for(i in 1:length(files)){
  print(i)
  one <- readRDS(files[i])
  one.f <- dplyr::filter(one, jur == "High seas")
  # print(paste(nrow(one), "-->", nrow(one.f), "pnts"))
  saveRDS(one.f, paste(out, filename[i]))
}

# now use High seas points only for RFMO analysis ~~~~~~
ins <- "data/analysis/ARG_assign/oveez_hs/"
main <- "data/analysis/ARG_assign/ovrfmo/"
outs <- paste0(list.files(main, full.names = T, recursive = F), "/") # need to have one folder for each RFMO

# individual RFMO shapefiles (http://www.fao.org/geonetwork/srv/en/main.home?uuid=cc7dbf20-1b8b-11dd-8bbb-0017f293bd28)
shpfiles.list <- tools::list_files_with_exts(shp_folder, "shp")

for(i in 1:length(shpfiles.list)){ # loop through each RFMO
  print(i)
  one <- shapefile(shpfiles.list[i])
  overJur(inFolder = ins, files=NULL, over_which = "RFMO", spatial = one, outFolder = outs[i])
}


# Step 6 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculate bird-days  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("scripts/final/source_fxns/birddays_fxn.R")
# source("scripts/exploration/source_fxns/WIP_birddays_fxn.R")

# EEZ analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
folder <- "data/analysis/ARG_assign/oveez/"
out    <- "data/analysis/ARG_assign/birddays_eez/"


birddays(inFolder = folder, by = "month", outFolder = out)


# RFMO analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make output folders for each RFMO (need a vector of RFMO names ['rfmos'])
for(i in 1:length(rfmos)){
  one <- rfmos[[i]]
  dir.create(paste0("data/analysis/ARG_assign/birddays_rfmo/", one))
}

main_in <- "data/analysis/ARG_assign/ovrfmo/"
ins <- paste0(list.files(main_in, full.names = T, recursive = F), "/")

main_out <- "data/analysis/ARG_assign/birddays_rfmo/"
outs <- paste0(list.files(main_out, full.names = T, recursive = F), "/") # need to have one folder for each RFMO

for(i in 1:length(ins)){ # loop through each RFMO
  print(i)
  birddays(inFolder = ins[i], by = "month", outFolder = outs[i])
}


# Step 7 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sum up time spent by population  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("scripts/final/source_fxns/globCount_fxn.R")

PD <- read.csv('data/population_estimates.csv', stringsAsFactors = F)

if(assign == "ARG"){
  PD$jurisdiction <- ifelse(PD$site_name == "Falkland Islands (Islas Malvinas)" | PD$site_name == "South Georgia (Islas Georgias del Sur)", "Argentina", PD$jurisdiction)
  PD$origin <- ifelse(PD$site_name == "Falkland Islands (Islas Malvinas)" | PD$site_name == "South Georgia (Islas Georgias del Sur)", "Argentina", PD$jurisdiction)
}

# EEZ analysis
folderBD <- "data/analysis/ARG_assign/birddays_eez/"  # indiv. bird-days data set
folderOV <- "data/analysis/ARG_assign/oveez/"         # tracking data, overlain on spatial EEZ data
out      <- "data/analysis/ARG_assign/glob_count/"    # where to save combined summary


# EEZ analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

globCount(brdyFolder = folderBD, overFolder = folderOV, outFolder = out, by="month", lookup = NULL, plotCompare=F, popData=PD)


# RFMO analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## make output folders for each RFMO (need a vector of RFMO names ['rfmos'])
# for(i in 1:length(rfmos)){
#   one <- rfmos[[i]]
#   dir.create(paste0("data/analysis/ARG_assign/glob_count_rfmo/", one))
# }

main_in <- "data/analysis/ARG_assign/birddays_rfmo/"
ins <- paste0(list.files(main_in, full.names = T, recursive = F), "/")

main_out <- "data/analysis/ARG_assign/glob_count_rfmo/"
outs <- paste0(list.files(main_out, full.names = T, recursive = F), "/") # need to have one folder for each RFMO

for(i in 1:length(ins)){ # loop through each RFMO
  print(i)
  globCount(brdyFolder = ins[i], overFolder = NULL, outFolder = outs[i], by="month", lookup = NULL, plotCompare=F, popData=PD)
}


## Extra step(s) ## --------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Make bar plot of country visitation for each species ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("scripts/final/source_fxns/annual_sp_plot_fxn.R")

folder  <- "data/analysis/ARG_assign/glob_count/"
outFolder <- "figures/ARG_assign/species_annual_plots/"


annual_sp_plot(inFolder = folder, plotFolder = outFolder, viewPlot = F)

