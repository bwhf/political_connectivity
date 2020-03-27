## Function to calculate the proportion of time spent by the greater population in each jurisdiction, by multiplying sample level proportions with population estimates. Proportions of time are calculated both at island group level (poptime), as well as at global breeding population level ('globprop'). "tot_atatime" is the number of bird days spent by that island group pop in a jurisdiction in a given month. ##

# brdyFolder: folder containing data frames for each species, with data on the proportion of time each individual track spent in different jurisdictions
# outFolder: folder in which to save summary tables
# overFolder: folder containing jurisdiction-overlain tracking data, for counting number proportional occurrence (BIASED)
# by: "month" or "season"
# popData: table of population estimates. Must contain fields named "global_pop_estimate_IND", "pop_estimate_IND", and "site_name". Site names must match those in bird-days data.


globCount <- function(brdyFolder, outFolder, by=NULL, overFolder=NULL, lookup=NULL, brdyFiles=NULL, overFiles=NULL, plotCompare=FALSE, popData=NULL){
  
  brdyFiles <- list.files(brdyFolder, pattern=".rds")
  # files <- brdyfiles[str_detect(brdyfiles, "Diomedea exulans")]
  
  for(i in 1:length(brdyFiles)){
    print(i)
    ## for Saving
    brdy <- readRDS(paste(brdyFolder, brdyFiles[i], sep=""))

    sp <- first(brdy$scientific_name)
    
    
    ######### Compare bird-days estimate of birds visiting Jurisdictions to point based visitation estimate #########
    
    if(by == "season"){    
      brdysum <- brdy %>% group_by(adj_site_name, breed_status, jur) %>% summarise(
        scientific_name = first(scientific_name),
        site_name = first(site_name),
        n_tracks  = first(n_tracks),
        pop_time  = sum(ppts), # sum of time spent across individuals (within season, jur, population)
        samp_est  = pop_time*first(n_tracks) # est. of number of sample birds in jur at any given time (in each month)
      )
    } else if(by == "month"){
      brdysum <- brdy %>% group_by(adj_site_name, month, jur) %>% summarise(
        scientific_name = first(scientific_name),
        site_name = first(site_name),
        n_tracks  = first(n_tracks),
        pop_time  = sum(ppts), # sum of time spent across individuals (within month, jur, population)
        samp_est  = pop_time*first(n_tracks) # est. of number of sample birds in jur at any given time (in each month)
      )
    }
    
    
    
    if(!is.null(popData)){
      
      PDsp      <- popData[popData$scientific_name %in% unique(brdysum$scientific_name), ]
      PDsp.site <- PDsp[PDsp$standard_site_name %in% unique(brdysum$adj_site_name), ]      # HERE, if adj_site_names > 1, problem
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # combine population data and time spent/visitation summary
      cntsum <- merge(
        brdysum, PDsp.site[c("standard_site_name", "pop_estimate_IND", "global_pop_estimate_IND", "origin")], 
        by.x = c("adj_site_name"), by.y = c("standard_site_name"), 
        all = TRUE
      )
      
      # normal prop. based on prop.time.spent * population_size
      cntsum$tot_atatime  <- cntsum$pop_time * cntsum$pop_estimate_IND
      
    } else { cntsum <- brdysum }
    
    # print(brdy %>% group_by(breed_status) %>% summarise(sum(ppts))) ## should all be 1 !
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Use unique occurrences of individuals (tracks) within jurs to estimate minimum number of birds visiting an jur
    if(!is.null(overFolder)){ 
      
      if(is.null(overFiles)){
        overFiles <- list.files(overFolder, pattern=".rds")
      }
      # all species
      spp <- unique(do.call("rbind", str_split(overFiles, n=4, pattern="_"))[, 1])
      sp <- spp[i]
      sp_files <- str_subset(overFiles, pattern = fixed(sp))
      
      ovjur <- do.call( "rbind", lapply( sp_files, function(x) readRDS(paste(overFolder, x, sep = "")) ) )
      
      # need to add adj_site_name from lookup table ~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # * adj_site_name is name to be used when calculating population-level time spent, 
      # * site_name now identifies breeding site for all species
      if(!is.null(lookup)){
        ovjur <- left_join(ovjur, lookup, by=c("breed_status","scientific_name", "site_name"))
      } else { 
        warning("No lookup table to adjust site_name by phenology!")
        ovjur$adj_site_name <- ovjur$site_name
      }
      
      ovjur$year <- lubridate::year(ovjur$date_time)
      
      if(by == "season"){        ### Count number of birds visiting jurs based on points alone ###
        ovjur_sum <- ovjur %>% group_by(adj_site_name, breed_status, jur) %>% summarise(
          n_visitors = n_distinct(track_id),
          site_name  = first(site_name) 
        )
      } else if(by == "month"){
        ovjur$month <- lubridate::month(ovjur$date_time)
        
        ### Count number of birds visiting EEZs based on points alone -- within years ###
        ovjur <- ovjur %>% group_by(adj_site_name, year, month) %>% mutate(
          n_tracks_yr = n_distinct(track_id)
        )
        
        ovjur_yr_sum <- ovjur %>% group_by(adj_site_name, year, month, jur) %>% summarise(
          n_tracks_yr = first(n_tracks_yr),
          n_visitors = n_distinct(track_id),
          site_name  = first(site_name) 
        )
        
      }
      
      cntsum_yr <- merge(cntsum, ovjur_yr_sum)
      
    
      ### compare number of birds visiting EEZs to the estimate of number of birds at in EEZs at any given time ###
      if(plotCompare == TRUE){
        print(plot(cntsum$n_visitors, cntsum$samp_est))
        
      }
      
      final_sum <- cntsum_yr %>% mutate(
        prop_visiting = n_visitors / n_tracks_yr,
        min_visiting  = prop_visiting * pop_estimate_IND,
        jur = ifelse(jur=="French Southern & Antarctic Lands" | jur=="Reunion", "France", 
            ifelse(jur=="Saint Helena, Ascension en Tristan da Cunha" , "United Kingdom", jur)),
          globprop = tot_atatime/global_pop_estimate_IND
        ) %>% 
        dplyr::rename(jurisdiction = jur)
      
      saveRDS(final_sum, paste(outFolder, sp, "_globsum", ".rds", sep=""))
      
    } else { 
      
      final_sum <- cntsum %>%  # rename jurisdiction to area for plotting
        mutate(
          jur = ifelse(jur=="French Southern & Antarctic Lands" | jur=="Reunion", "France", 
            ifelse(jur=="Saint Helena, Ascension en Tristan da Cunha" , "United Kingdom", jur)),
          globprop =tot_atatime/global_pop_estimate_IND
        ) %>% 
        dplyr::rename(jurisdiction = jur)
      
      saveRDS(final_sum, paste(outFolder, sp, "_globsum", ".rds", sep=""))
    }
    

  }
}