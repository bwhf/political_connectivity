## Running analysis with stricter threshold on representativeness ##

pacman::p_load(dplyr, sp, raster, stringr, ggplot2, lubridate, cowplot)


master <- "data/analysis/bird_thresh/"

# Step 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Filter out months with fewer than X10X days from at least e.g. 5 birds tracking data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("scripts/final/source_fxns/filter_badmonths_fxn.R")

inFolder  <- "data/analysis/noeqx_dnsmpl/"
outFolder <-  paste0(master, "month_filtered/")

bad_df <- filter_badmonths(inFolder = inFolder, outFolder = outFolder, n_ind=5, n_days = 10)

### Reporting of filtering ~~~~~~~~~~~~
# how many times were months removed? (spp-site 'times')
n_mon_filtered <- bad_df %>% group_by(month) %>% summarise(n = n())

mo2Num <- function(x) match(tolower(x), tolower(month.abb)) # match month name to month number (for ordering)
n_mon_filtered <- n_mon_filtered %>% mutate(
  num_month = mo2Num( n_mon_filtered$month )
) %>% arrange(num_month) %>% dplyr::select(month, n)

write.csv(bad_df,  paste0(master, "summary_tables/bad_months_filtered.csv"), row.names = F)
write.csv(n_mon_filtered,  paste0(master, "summary_tables/ntimes_bad_months_filtered.csv"), row.names = F)


# Step 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Visualize annual coverage of data, by device (and optionally year)  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("scripts/final/source_fxns/annualCover_plot_fxn.R")

folder <-  paste0(master, "month_filtered/")

files <- list.files(folder)

## run for one species
# species <- "Calonectris diomedea"
# files <- files[str_detect(files, species)]

repo <- "figures/bird_thresh/annual_coverage_spp.site/"

annualCover_plot(files=files, inFolder = folder, byYear = F, savePlot = T, saveFolder = repo)



## ANALYSIS STEPS ## ---------------------------------------------------------

# Step 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Overlay each data set on an EEZ polygon layer ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("scripts/final/source_fxns/overJur_fxn.R")

folder <- paste0(master, "month_filtered/")

files <- list.files(folder)

## select one species to test 
# species <- "Calonectris borealis"
# files <- list.files(folder)
# files <- files[str_detect(files, species)]

# EEZ analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
out <-  paste0(master, "oveez/")
## Unioned land and EEZ dataset ## (http://www.marineregions.org/downloads.php)
eez_cnt <- raster::shapefile("spatial_data/shapefiles_EEZ_countries/union_countries_EEZs/EEZ_Land_v3_202030.shp", use_iconv = TRUE, encoding = "UTF-8")

overJur(inFolder = folder, files=NULL, over_which = "EEZ", spatial = eez_cnt, filter_landlocked=TRUE, assign="A", outFolder = out)


# RFMO analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NOTE: must first run EEZ analysis for this to work

# individual RFMO shapefiles (http://www.fao.org/geonetwork/srv/en/main.home?uuid=cc7dbf20-1b8b-11dd-8bbb-0017f293bd28)
shp_folder <- "spatial_data/RFMOs_ofinterest" # folder containing individual RFMO polygon shapefiles
rfmos <- do.call("rbind", str_split(tools::list_files_with_exts(shp_folder, "shp", full.names = F), pattern = fixed(".")))[,1]

# make output folders for each RFMO (need a vector of RFMO names ['rfmos'])
# for(i in 1:length(rfmos)){
#   one <- rfmos[[i]]
#   dir.create(paste0(paste0(master,"ovrfmo/"), one))
# }

# filter to High seas points only # ~~~~~~~~~~~~~~~~~~~
ins <- paste0(master, "oveez/")
out <- paste0(master, "oveez_hs/")

files <- list.files(ins, full.names = T)
filename <- list.files(ins, full.names = F)

for(i in 1:length(files)){
  print(i)
  one <- readRDS(files[i])
  one.f <- dplyr::filter(one, jur == "High seas")
  # print(paste(nrow(one), "-->", nrow(one.f), "pnts"))
  saveRDS(one.f, paste0(out, filename[i]))
}

# now use High seas points only for RFMO analysis ~~~~~~
ins  <- paste0(master, "oveez_hs/")
main <- paste0(master, "ovrfmo/")
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
folder <- paste0(master, "oveez/")
out    <- paste0(master, "birddays_eez/")


birddays(inFolder = folder, by = "month", outFolder = out)


# RFMO analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make output folders for each RFMO (need a vector of RFMO names ['rfmos'])
# for(i in 1:length(rfmos)){
  # one <- rfmos[[i]]
  # dir.create(paste0(paste0(master,"birddays_rfmo/"), one))
# }

main_in <- paste0(master, "ovrfmo/")
ins <- paste0(list.files(main_in, full.names = T, recursive = F), "/")

main_out <- paste0(master, "birddays_rfmo/")
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

# EEZ analysis
folderBD <- paste0(master, "birddays_eez/")  # indiv. bird-days data set
folderOV <- paste0(master, "oveez/")         # tracking data, overlain on spatial EEZ data
out      <- paste0(master, "glob_count/")    # where to save combined summary


# EEZ analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

globCount(brdyFolder = folderBD, overFolder = folderOV, outFolder = out, by="month", lookup = NULL, plotCompare=F, popData=PD)


## RFMO analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## make output folders for each RFMO (need a vector of RFMO names ['rfmos'])
for(i in 1:length(rfmos)){
  one <- rfmos[[i]]
  dir.create(paste0(paste0(master, "glob_count_rfmo/"), one))
}

main_in <- paste0(master, "birddays_rfmo/")
ins <- paste0(list.files(main_in, full.names = T, recursive = F), "/")

main_out <- paste0(master, "glob_count_rfmo/")
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

folder  <- paste0(master, "glob_count/")
outFolder <- "figures/bird_thresh/species_annual_plots/"


annual_sp_plot(inFolder = folder, plotFolder = outFolder, viewPlot = F)

