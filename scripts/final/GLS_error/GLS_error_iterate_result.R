## Assessing impact of GLS error on results ## --------------------------------

pacman::p_load(tidyverse)
## re-sample the location of GLS points a number of times and calculate n jurisdictions visited and average time spent in each across iterations

source("C:/Users/Martim Bill/Documents/R/source_scripts/MetersToDecimalDegrees_fxn.R")
source("scripts/final/source_fxns/overJur_fxn.R")
source("scripts/final/source_fxns/birddays_fxn.R")
source("scripts/final/source_fxns/globCount_fxn.R")

eez_cnt <- raster::shapefile("spatial_data/shapefiles_EEZ_countries/union_countries_EEZs/EEZ_Land_v3_202030.shp", use_iconv = TRUE, encoding = "UTF-8")

folder <- "data/analysis/month_filtered/" 
files <- list.files(folder)

files_sp <- do.call(rbind, str_split(files, "_"))[, 1]

## select one species to test 
# species <- "Calonectris borealis"
species <- "Phoebastria immutabilis"
files <- files[str_detect(files, species)]

sites <- tools::file_path_sans_ext(do.call(rbind, str_split(files, "_"))[,2])
sites

its <- 100 # number of times to run
full_result_list <- vector("list", its)

n_in_jur_list <- vector("list", its)

before <- Sys.time()
for(i in seq_len(its)){
  
  print(paste("run:", i))
  
  lapply(seq_along(files), function(x) {
    
    species <- files_sp[x]
    TD <- readRDS(paste0(folder, files[[x]]))
    
    other_devices <- subset(TD, device!='GLS')
    GLS <- subset(TD, device=='GLS')
    
    if(nrow(GLS) > 0){ # skip this if no GLS data
      rGLS <- GLS # GLS data, to be randomized
      
      # #### Method 1: draw new lats and longs from normal dist. w/ 186 sd
      # rGLS$longitude <- rnorm(nrow(GLS), mean=GLS$longitude, sd=MetersToDecimalDegrees(186*1000, GLS$latitude)) # longitudinal degree error adjusted for latitude
      # rGLS$latitude <- rnorm(nrow(GLS), mean=GLS$latitude, sd=1.66)
      
      #### Method 2: draw new lats and longs from directions (uniform dist.) and distances  (normal dist. w/ 186 sd)
      directions <- runif(nrow(GLS), 0, 360)
      distances  <- rnorm(nrow(GLS), mean=0, sd=186000)
      
      rGLS[, c(17,16)] <- destPoint(GLS[c(17,16)], directions, distances) # must be long/lat
      
      
      # re-combine with GPS and PTT data
      rTD <- rbind(other_devices, rGLS) %>% group_by(track_id) %>% arrange(date_time)
    } else {  rTD <- TD  }
    
    ## save generated data 
    saveRDS(rTD, paste0("data_test\\GLS_error\\generated\\", files_sp[x], "_", sites[x], ".rds"))
    
  } )
  
  ##-----------------------------------------------------------------------
  overJur(
    inFolder = "data_test\\GLS_error\\generated\\", 
    files=NULL, over_which = "EEZ", 
    spatial = eez_cnt, 
    filter_landlocked=TRUE,
    assign="A", 
    outFolder = "data_test\\GLS_error\\over_eez\\"
  )
  
  
  # EEZ analysis ----------------------------------------------------------
  birddays(
    inFolder = "data_test\\GLS_error\\over_eez\\",
    by = "month", over = "EEZ", 
    outFolder = "data_test\\GLS_error\\birddays_eez\\"
  )
  
  
  PD <- read.csv('data/population_estimates.csv', stringsAsFactors = F)
  
  # EEZ analysis
  folderBD <- "data_test\\GLS_error\\birddays_eez\\"  # indiv. bird-days data set
  folderOV <- "data_test\\GLS_error\\over_eez\\"        # tracking data, overlain on spatial EEZ data
  out      <- "data_test\\GLS_error\\glob_count\\"    # where to save combined summary
  
  # EEZ analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  globCount(
    brdyFolder = folderBD, overFolder = folderOV, outFolder = out, by="month", lookup = NULL, plotCompare=F, popData=PD
    )
  
  
  #----------------------------------------------------------------------------
  # run global_patterns calculations of richness and annual time spent ~~~~~~~~
  
  globCount_files <- list.files(out, full.names=T)
  # files <- files[1:2]
  alltimes <- do.call("rbind", lapply(globCount_files, function(x) readRDS(x)))
  
  alltimes$is_origin <- ifelse(alltimes$jurisdiction == alltimes$origin, "Breeding", "Visiting")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # calculate RICHNESS: breeding, visiting, only breeding/visiting (species from pops outside country)
  
  # what is relationship(s) between species and a jurisdiction?
  relations <- alltimes %>% group_by(jurisdiction, scientific_name) %>% summarise(
    n_relations = n_distinct(is_origin), 
    relation = if_else(n_relations == 2, "Both", first(is_origin))
  ) 
  
  tot <- relations %>% group_by(jurisdiction) %>% summarise(                                      # total species count
    richness  = n_distinct(scientific_name)
  )
  
  visit <- relations %>% group_by(jurisdiction) %>% dplyr::filter(relation == "Visiting") %>% summarise( # count of species visiting only
    visitonly_rich  = n_distinct(scientific_name)
  )
  
  both <- relations %>% group_by(jurisdiction) %>% dplyr::filter(relation == "Both") %>% summarise(  # both visiting and breeding spp count
    both_rich  = n_distinct(scientific_name)
  )
  
  breed <- relations %>% group_by(jurisdiction) %>% dplyr::filter(relation == "Breeding") %>% summarise( # count of species only breeding
    breedonly_rich  = n_distinct(scientific_name)
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### Use population data to calculate FULL breeding richness 
  
  PD$spp_site <- paste(PD$scientific_name, PD$standard_site_name)
  alltimes$spp_site <- paste(alltimes$scientific_name, alltimes$adj_site_name)
  
  #filter to only spp-site combos which we have tracking data for 
  PD <- PD[which(PD$spp_site %in% unique(alltimes$spp_site)),]
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # global, species-site estimates from tracked populations
  pop_by_jur_spp <- PD %>% group_by(scientific_name, jurisdiction) %>% summarise( 
    glob_pop_IND   =  first(global_pop_estimate_IND),
    tot_breeders = sum(na.omit(pop_estimate_IND)),
    breed_rich   = n_distinct(scientific_name)
  )
  
  # total number of birds, covered, and not, by tracking 
  glob_cover <- pop_by_jur_spp %>% group_by(scientific_name) %>% summarise(
    glob_pop_IND = first(glob_pop_IND),
    tot_track_pops = sum(tot_breeders)
  ) %>% ungroup() %>% summarise(
    glob_tot = sum(glob_pop_IND),
    glob_tot_track_pops = sum(tot_track_pops),
    not_covered = glob_tot - glob_tot_track_pops
  ) ## Number of birds not covered by our tracking (roughly)
  
  # sum totals for host countries (of tracked populations) and breeding richnesses
  pop_by_jur <- pop_by_jur_spp %>% group_by(jurisdiction) %>% summarise(
    tot_breeders = sum(na.omit(tot_breeders)),
    breed_rich   = sum(breed_rich)
  )
  
  ## DF of all angles on richness
  rich <- full_join(tot, visit, by="jurisdiction", all=T) %>% 
    left_join(both, by="jurisdiction", all=T) %>% 
    left_join(breed, by="jurisdiction", all=T) %>% 
    left_join(pop_by_jur, by="jurisdiction") %>% 
    mutate(
      visit_rich = visitonly_rich + ifelse(is.na(both_rich), 0, both_rich)
    ) %>%
    dplyr::select(jurisdiction, richness, breed_rich, visit_rich, both_rich, breedonly_rich, visitonly_rich, tot_breeders) %>% 
    arrange(desc(richness))
  
  rich <- rich %>% mutate(
    breed_rich = ifelse(is.na(breed_rich), 0, breed_rich),
    visit_rich = ifelse(is.na(visit_rich), 0, visit_rich),
    both_rich = ifelse(is.na(both_rich), 0, both_rich),
    breedonly_rich = ifelse(is.na(breedonly_rich), 0, breedonly_rich),
    visitonly_rich = ifelse(is.na(visitonly_rich), 0, visitonly_rich)
  )
  
  #----------------------------------------------------------------------------
  ## Calculate annual time spent per jurisdiction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # aggregate time spent by ORIGIN 
  timespent_sp <- alltimes %>% group_by(scientific_name, adj_site_name, month, is_origin, jurisdiction) %>% summarise(
    tot_atatime = first(tot_atatime)
  ) %>% group_by(is_origin, jurisdiction) %>% summarise( 
    tot_staying = sum(na.omit(tot_atatime)) / 12
  )
  
  sum(timespent_sp$tot_staying) ## total bird years estimated by our tracking datas (pop coverage - annual time coverage)
  
  timespentsum <- timespent_sp %>% group_by(jurisdiction) %>% summarise(sum_tot_staying = sum(na.omit(tot_staying))) %>% left_join(timespent_sp)
  
  #----------------------------------------------------------------------------
  ## Create table summarizing richness and timespent values per jurisdiction
  
  sum2save <- timespentsum %>% spread(is_origin, tot_staying) %>% 
    full_join(rich, by="jurisdiction") %>% 
    rename(breed_time=Breeding, visit_time=Visiting) %>% 
    mutate(is_origin = if_else(breed_rich==0, TRUE, FALSE)) %>% 
    arrange(is_origin) %>% 
    dplyr::select(
      jurisdiction, richness, breed_rich, visit_rich, both_rich, breedonly_rich, 
      visitonly_rich, tot_breeders, sum_tot_staying, breed_time, visit_time
    )
  sum2save
  
  
  full_result_list[[i]] <- sum2save
  
  
}

duration <- Sys.time() - before
duration

result <- do.call(rbind, full_result_list)


# Summarize ! #

result <- readRDS("data_test//GLS_error//result//full_result.rds")


summ <- result %>% mutate(
  richness  = ifelse(is.na(richness), 0, richness),
  visit_rich  = ifelse(is.na(visit_rich ), 0, visit_rich),
  sum_tot_staying = ifelse(is.na(sum_tot_staying), 0, sum_tot_staying),
  breed_time  = ifelse(is.na(breed_time), 0, breed_time),
  visit_time  = ifelse(is.na(visit_time), 0, visit_time)
) %>% group_by(jurisdiction) %>% summarise(
  mn_richness = mean(richness ),
  sd_richness = sd(richness ),
  md_richness = median(richness ),
  min_richness = min(richness ),
  max_richness = max(richness ),
  
  mn_visit_rich = mean(visit_rich ),
  sd_visit_rich = sd(visit_rich ),
  md_visit_rich = median(visit_rich ),
  min_visit_rich = min(visit_rich ),
  max_visit_rich = max(visit_rich ),

  mn_tot_atatime = mean(sum_tot_staying ),
  sd_tot_atatime = sd(sum_tot_staying ),
  max_tot_atatime = max(sum_tot_staying),
  min_tot_atatime = min(sum_tot_staying),
  mn_visit_time = mean(visit_time ),
  sd_visit_time = sd(visit_time ),
  mn_breed_time = mean(breed_time ),
  sd_breed_time = sd(breed_time ),
  appearances   = n()
)


### Save 

# saveRDS(result, "data_test//GLS_error//result//full_result.rds")
# saveRDS(summ, "data_test//GLS_error//result//summary.rds")

### Plot ## 

# summ <- readRDS("data_test//GLS_error//result//full_summary1.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combine 'result' with original result ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

master <- "data/analysis/"
master_figs <- "figures/"

## Choose whether to analyze UK-assigned or Argentina-assigned data ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

assign <- "A"   #UK and Spain

PD <- read.csv('data/population_estimates.csv', stringsAsFactors = F) # population data 

folder <- paste0(master, "glob_count/")

## Run analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

files <- list.files(folder, full.names = T)
# files <- files[1:2]
alltimes <- do.call("rbind", lapply(files, function(x) readRDS(x)))

alltimes$is_origin <- ifelse(alltimes$jurisdiction == alltimes$origin, "Breeding", "Visiting")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate RICHNESS: breeding, visiting, only breeding/visiting (species from pops outside country)

# what is relationship(s) between species and a jurisdiction?
relations <- alltimes %>% group_by(jurisdiction, scientific_name) %>% summarise(
  n_relations = n_distinct(is_origin), 
  relation = if_else(n_relations == 2, "Both", first(is_origin))
) 
# total species count
tot <- relations %>% group_by(jurisdiction) %>% summarise(                                      
  richness  = n_distinct(scientific_name)
)
# count of species visiting only
visit <- relations %>% group_by(jurisdiction) %>% dplyr::filter(relation == "Visiting") %>% summarise( 
  visitonly_rich  = n_distinct(scientific_name)
)
# both visiting and breeding spp count
both <- relations %>% group_by(jurisdiction) %>% dplyr::filter(relation == "Both") %>% summarise(     
  both_rich  = n_distinct(scientific_name)
)
# count of species only breeding
breed <- relations %>% group_by(jurisdiction) %>% dplyr::filter(relation == "Breeding") %>% summarise(
  breedonly_rich  = n_distinct(scientific_name)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Use population data to calculate FULL breeding richness 

PD$spp_site <- paste(PD$scientific_name, PD$standard_site_name)
alltimes$spp_site <- paste(alltimes$scientific_name, alltimes$adj_site_name)

#filter to only spp-site combos which we have tracking data for 
PD <- PD[which(PD$spp_site %in% unique(alltimes$spp_site)),]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# global, species-site estimates from tracked populations
pop_by_jur_spp <- PD %>% group_by(scientific_name, jurisdiction) %>% summarise( 
  glob_pop_IND   =  first(global_pop_estimate_IND),
  tot_breeders = sum(na.omit(pop_estimate_IND)),
  breed_rich   = n_distinct(scientific_name)
)

# total number of birds, covered, and not, by tracking 
glob_cover <- pop_by_jur_spp %>% group_by(scientific_name) %>% summarise(
  glob_pop_IND = first(glob_pop_IND),
  tot_track_pops = sum(tot_breeders)
) %>% ungroup() %>% summarise(
  glob_tot = sum(glob_pop_IND),
  glob_tot_track_pops = sum(tot_track_pops),
  not_covered = glob_tot - glob_tot_track_pops
) ## Number of birds not covered by our tracking (roughly)

# sum totals for host countries (of tracked populations) and breeding richnesses
pop_by_jur <- pop_by_jur_spp %>% group_by(jurisdiction) %>% summarise(
  tot_breeders = sum(na.omit(tot_breeders)),
  breed_rich   = sum(breed_rich)
)

## DF of all angles on richness
rich <- full_join(tot, visit, by="jurisdiction", all=T) %>% left_join(both, by="jurisdiction", all=T) %>% 
  left_join(breed, by="jurisdiction", all=T) %>% 
  left_join(pop_by_jur, by="jurisdiction") %>% 
  mutate(
    visit_rich = visitonly_rich + ifelse(is.na(both_rich), 0, both_rich)
  ) %>%
  dplyr::select(jurisdiction, richness, breed_rich, visit_rich, both_rich, breedonly_rich, visitonly_rich, tot_breeders) %>% arrange(desc(richness))

rich <- rich %>% mutate(
  breed_rich = ifelse(is.na(breed_rich), 0, breed_rich),
  visit_rich = ifelse(is.na(visit_rich), 0, visit_rich),
  both_rich = ifelse(is.na(both_rich), 0, both_rich),
  breedonly_rich = ifelse(is.na(breedonly_rich), 0, breedonly_rich),
  visitonly_rich = ifelse(is.na(visitonly_rich), 0, visitonly_rich)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculate annual time spent per jurisdiction

# aggregate time spent by ORIGIN 
timespent_sp <- alltimes %>% group_by(scientific_name, adj_site_name, month, is_origin, jurisdiction) %>% summarise(
  tot_atatime = first(tot_atatime)
) %>% group_by(is_origin, jurisdiction) %>% summarise( 
  tot_staying = sum(na.omit(tot_atatime)) / 12
)

sum(timespent_sp$tot_staying) ## total bird years estimated by our tracking datas (pop coverage - annual time coverage)

timespentsum <- timespent_sp %>% group_by(jurisdiction) %>% summarise(sum_tot_staying = sum(na.omit(tot_staying))) %>% left_join(x)

## Create table summarizing richness and timespent values per jurisdiction
sum2save <- timespentsum %>% spread(is_origin, tot_staying) %>% 
  full_join(rich, by="jurisdiction") %>% 
  rename(breed_time=Breeding, visit_time=Visiting) %>% 
  mutate(is_origin = if_else(breed_rich==0, TRUE, FALSE)) %>% 
  arrange(is_origin) %>% 
  dplyr::select(
    jurisdiction, richness, breed_rich, visit_rich, both_rich, breedonly_rich, 
    visitonly_rich, tot_breeders, sum_tot_staying, breed_time, visit_time
  )

result_obs <- result %>% bind_rows(sum2save) # combine observed result w/ re-sampled results

saveRDS(result_obs, "data_test//GLS_error//result//full_result_plus_obs.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summarize ! #

result_obs <- readRDS("data_test//GLS_error//result//full_result_plus_obs.rds")


summ_obs <- result_obs %>% mutate(
  richness  = ifelse(is.na(richness), 0, richness),
  visit_rich  = ifelse(is.na(visit_rich ), 0, visit_rich),
  sum_tot_staying = ifelse(is.na(sum_tot_staying), 0, sum_tot_staying),
  breed_time  = ifelse(is.na(breed_time), 0, breed_time),
  visit_time  = ifelse(is.na(visit_time), 0, visit_time)
) %>% group_by(jurisdiction) %>% summarise(
  mn_richness = mean(richness ),
  sd_richness = sd(richness ),
  md_richness = median(richness ),
  min_richness = min(richness ),
  max_richness = max(richness ),
  
  mn_visit_rich = mean(visit_rich ),
  sd_visit_rich = sd(visit_rich ),
  md_visit_rich = median(visit_rich ),
  min_visit_rich = min(visit_rich ),
  max_visit_rich = max(visit_rich ),
  
  mn_tot_atatime = mean(sum_tot_staying ),
  sd_tot_atatime = sd(sum_tot_staying ),
  max_tot_atatime = max(sum_tot_staying),
  min_tot_atatime = min(sum_tot_staying),
  mn_visit_time = mean(visit_time ),
  sd_visit_time = sd(visit_time ),
  mn_breed_time = mean(breed_time ),
  sd_breed_time = sd(breed_time ),
  appearances   = n()
)


### Save 

saveRDS(summ_obs, "data_test//GLS_error//result//summary_plus_obs.rds")

