## evaluate monthly coverage of tracking data for each population and when the number of unique days with tracking data don't exceed a certain threshold, scrap that month as 'unrepresentative' ## 

# inFolder:  path to folder containing data sets to be assessed and filtered
# inFolder:  path to folder where processed data sets ought be saved
# n_days:    minimum threshold number of unique tracking days in a month to consider a month representative
# n_ind:     minimum threshold number of unique tracks (i.e. individuals) in a month to consider it representative

filter_badmonths <- function(inFolder, outFolder, n_ind=1, n_days=NULL){
  
  pacman::p_load(ggplot2, RColorBrewer, lubridate, stringr)
  
  files <- list.files(inFolder)
  
  
  spp.list <- lapply(strsplit(files, "_"), function(x) x[[1]])  # get species names
  site.list <- lapply(strsplit(files, "_"), function(x) x[[2]])  # get site names
  # sites <- unique(as.vector(do.call("rbind", site.list)))
  spp.sites <- unique(paste(spp.list, site.list, sep="_"))
  
  bad_list <- vector(mode = "list", length = length(spp.sites))
  
  for(f in 1:length(spp.sites)){             ## Loop through each site
    print(f)
    spp.site.files <- files[str_detect(files, fixed(spp.sites[f]))]  # fixed() needed to deal with punctuation marks in names (i.e. "(")
    
    TD <- do.call( "rbind", lapply( spp.site.files, function(x) as.data.frame(readRDS(paste(inFolder, x, sep="")))))
    
    TD$month <- month(TD$date_time)
    TD$mday <- lubridate::mday(TD$date_time)
    
    # get number of unique days in each month with at least one individual tracked
    month_sum1 <- TD %>% group_by(scientific_name, site_name, month) %>% summarise(
      n_mday = n_distinct(mday)
    )
    month_sum2 <- TD %>% group_by(scientific_name, site_name, month) %>% summarise(
      n_inds = n_distinct(track_id)
    )
    
    month_sum <- merge(month_sum1, month_sum2, by=c("scientific_name", "site_name", "month"))
    
    # which months are they? filter them out and summarise
    if(n_ind == 1){
      bad_months <- month_sum$month[month_sum$n_mday < n_days]
    } else {
      bad_months <- month_sum$month[(month_sum$n_mday < n_days | month_sum$n_inds < n_ind)]
    }

    if(length(bad_months) > 0){
      # filter out data from those months (for the site)
      TD_filtered <- TD %>% dplyr::filter(!month %in% bad_months)
      
      bad_month_sum <- month_sum %>% filter( month %in% bad_months) %>% 
        mutate( month = month.abb[month] )
      
      bad_list[[f]] <- bad_month_sum
      
    } else { TD_filtered <- TD}

    if(nrow(TD_filtered) > 0){
      saveRDS(TD_filtered, file = paste0(outFolder, paste0(spp.sites[f], ".rds")))
    } else {next}

  }
  
  bad_df <- do.call("rbind", bad_list)
  return(bad_df)
}
