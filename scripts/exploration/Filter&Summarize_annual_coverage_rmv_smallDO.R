####### Script to prepare metadata files for gap analysis, data requests etc. ##########

pacman::p_load(tidyverse, lubridate, readxl)

#### Load data ####


# STD metadata file (by track)
MD <- read.csv('C:\\Users\\Martim Bill\\Documents\\phd\\data\\STD_data.summary\\Metadata_2018-10-25_badGPSdates.csv') # bad GPS dates rmvd
# MD <- read.csv('C:\\Users\\Martim Bill\\Documents\\phd\\data\\STD_data.summary\\Metadata_inc_original_track_id_2018-10-25.csv') # full

# Data Owner metadata
DO <- read.csv('C:\\Users\\Martim Bill\\Documents\\phd\\data\\STD_data.summary\\users_joined_dataset_owners_2018-10-25.csv')
# population data
PD <- as.data.frame(read_excel('C:/Users/Martim Bill/Documents/political_connectivity/data_summaries/pop_data/AllSpp_IslGroup_Pops.xlsx'))

##### Calculate (maximum) duration of each track #########
MD$date_max <- as.Date(MD$date_max)
MD$date_min <- as.Date(MD$date_min)

MD$track_duration <- as.numeric(difftime(MD$date_max, MD$date_min, units='days')) # calculate number of days of tracking data

##### Filtering data ######################################################

# subset to families/ genera of interest
tubes <- subset(MD, family=='Diomedeidae'| str_detect(scientific_name, 'Procellaria|Calonectris|Ardenna|Macronectes|Puffinus mauretanicus'))

# remove non-adult data (juveniles won't be classed as 'unknown')
tubes <- droplevels(subset(tubes, age=='adult' | age=='unknown'))

# remove tracks w/out date information
tubes <- tubes[complete.cases(tubes$date_min),]

# remove at-sea data (where breeding site unknown)
tubes <- subset(tubes, site_name != 'At-Sea' & site_name != 'Non-breeding, site unknown')

# remove non-published data
tubes <- subset(tubes, published==T)

########## Population data (*****this is missing for non-ACAP species) ############

# standardize shared columns
colnames(PD)[c(1,2,4)] <- c('scientific_name', 'common_name','site_name')

# retain only Species of Interest 
SoI <- PD[which(PD$scientific_name %in% tubes$scientific_name),]

# write_excel_csv(SoI, 'C:/Users/Martim Bill/Documents/political_connectivity/data_summaries/pop_data/ACAP_IslGroups_w_TrackingData.csv') 

# **BROKEN since nonACAPs added to PD** for now, keep only ACAP species (since pop data only available for these species)
# acap <- tubes[which(tubes$scientific_name %in% SoI$scientific_name),]
# acap <- droplevels(acap)

# unique(acap$common_name)
# unique(SoI$common_name)

### join pop. data to tracks dataset ###
# combo <- merge.data.frame(acap, SoI, by = c('scientific_name', 'common_name', 'site_name'), all.x=T)
combo <- merge.data.frame(tubes, SoI, by = c('scientific_name', 'common_name', 'site_name'), all.x=T)

# unique(combo$common_name)

############ Combine Data Owner info with Tracks and population metadata #######################

tracks <- tubes  # or acap

# retain only dataowner info for datasets of interest
DOoI <- DO[which(DO$dataset_id %in% tracks$dataset_id),]
# only primary owner (so there's only one line per dataset)
DOoI <- droplevels(subset(DOoI, role_id=='P'))

# merge track AND pop data with dataowner info
comboDO <- merge.data.frame(combo, DOoI, by = c('dataset_id'), all.x=T)

# w/out population data 
# comboDO <- merge.data.frame(tubes, DOoI, by = c('dataset_id'), all.x=T)

###### Remove dataowners with insignificant contributions ###### 

# Load dataset w/ names of owners and a field indicating whether or not to include them
DOyn <- as.data.frame(read_excel('C:/Users/Martim Bill/Documents/political_connectivity/data_summaries/DataOwners_worthwhileness.xlsx'))

head(DOyn)

PDO <- subset(DOyn, worthwhile=='Y' | worthwhile=='M')

comboDO <- comboDO[which(comboDO$contact_name %in% PDO$contact_name),]

###### Remove GPS tracks with probably erroneous min/max dates ######

comboDO$faulty_GPS_dates <- ifelse(is.na(comboDO$faulty_GPS_dates), 0, 1)
comboDO <- subset(comboDO, faulty_GPS_dates==0)

######## Loops to estimate the days covered by each track, and produce summary graphs for each island group, showing the number of tracks per day across the year **** and eventually to display population data on graphs too #############

species <- unique(comboDO$common_name)
speciescover <- list()
comboDO$`% global population` <- round(comboDO$`% global population`, digits=1)

for(j in 1:length(species)){ # by species
  one <- comboDO[comboDO$scientific_name==unique(comboDO$scientific_name)[j],]
  # one <- subset(comboDO, common_name=="Campbell Albatross") ## use to inspect particular species
  
  sites <- droplevels(unique(one$site_name))
  # dev.new()
  path <- 'data_summaries/annual coverage/species_sites_tracks_per_DOY_wPop'  # set plot path and parameters
  png(file.path(path, paste(first(one$common_name), ".png", sep="")), width=600, height = 500, res=100)
  par(mar=c(3,3,1,1))
  par(mfrow = n2mfrow(length(sites)))
  
  site.list <- list()
  
  for(k in 1:length(sites)){ # by site w/in a species
    onesite <- one[one$site_name==unique(one$site_name)[k],]
    # onesite <- subset(one, site_name=='Campbell Islands') ## use to inspect particular site
    
    track.list <- list()
    
    for(i in 1:nrow(onesite)) { # by track w/in a site
      # create sequence of days from min to max ( what is 'covered') by the track
      track_cover <- yday(seq.Date(na.omit(onesite$date_min[i]), na.omit(onesite$date_max[i]), by='day'))
      
      scientific_name <- onesite$scientific_name[i]
      dataset_id <- onesite$dataset_id[i]
      track_id <- onesite$track_id[i]
      owner_id <- onesite$contact_name[i]
      
      track.list[[i]] <- data.frame(scientific_name=scientific_name, dataset_id=dataset_id, track_id=track_id, days.cover=track_cover, contact_id=owner_id) 
      
      
    }
    
    site.df <- do.call('bind_rows', track.list) # year-days covered by each track (w/in a site)
    site.df$site_name <- rep(first(onesite$site_name))
    
    # # PLOT # # 
    with(site.df,
         hist(days.cover, breaks=seq(1,366,1), xlim=c(1,366), col='firebrick', border='firebrick',xaxt = 'n', xlab=NULL, ylab='Number of Tracks', main=paste(unique(one$site_name)[k], sep=" - ")), cex.main=.8)
    axis(side = 1, at=c(32,91,152,213,274,335),  month.abb[c(2,4,6,8,10,12)] )
    # add text indicating % of global pop represented in island group
    if(is.na(first(onesite$`% global population`))==F){
          legend('bottomright', legend=paste(first(onesite$`% global population`),'% of global population', sep=""),bg=scales::alpha('white',.8))
    } else {
      legend('bottomright', legend='NO population data', bg=scales::alpha('white',.8))
    }
    
    site.list[[k]] <- site.df
    
  }
  dev.off()
  
  species.df <- do.call('bind_rows', site.list)
  
  speciescover[[j]] <- species.df
  
}


cover.df <- do.call('bind_rows', speciescover)

# saveRDS(cover.df, 'data_summaries/annual coverage/days.covered/days.covered.rds')
