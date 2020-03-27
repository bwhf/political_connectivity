## summarize number of birds tracked, and combine with summary of tracks (calculated later in analysis) ##

folder <- "data/analysis/month_filtered/"

files <- list.files(folder, full.names = T)

n_birds_list <- vector("list", length=length(files))

for(i in 1:length(files)){
  
  one <- readRDS(files[i])
  
  nNAs <- one %>% summarise(spp_site = paste(first(scientific_name), first(site_name)), nNAs = sum(is.na(bird_id)))
  if(nNAs$nNAs > 0) print(nNAs)
  n_birds_list[[i]] <- one %>% group_by(scientific_name, site_name, device) %>% summarise(
    n_birds = n_distinct(bird_id),
    n_tracks = n_distinct(track_id)
    )
  
}

n_birds <- do.call(rbind, n_birds_list)

sum(n_birds$n_birds)
sum(n_birds$n_tracks)


## by year (sum results in more, because some tracks span across 2 years)

n_birds_list <- vector("list", length=length(files))

for(i in 1:length(files)){
  
  one <- readRDS(files[i])
  
  one$year <- lubridate::year(one$date_time)
  
  nNAs <- one %>% summarise(spp_site = paste(first(scientific_name), first(site_name)), nNAs = sum(is.na(bird_id)))
  if(nNAs$nNAs > 0) print(nNAs)
  n_birds_list[[i]] <- one %>% group_by(scientific_name, site_name, year) %>% summarise(
    n_birds = n_distinct(bird_id),
    n_tracks = n_distinct(track_id)
  )
  
}

n_birds <- do.call(rbind, n_birds_list)

n_birds %>% group_by(scientific_name, site_name) %>% summarise(
  n_tracks = first(n_tracks),
  n_birds  = first(n_birds)
  ) %>% ungroup() %>%  summarise(
    n_tracks = sum(n_tracks),
    n_birds  = sum(n_birds)
  )


# combine with later-calculated tracking summary table #
track_summary <- read.csv("data/analysis/summary_tables/track_summary_sp_site_device.csv", stringsAsFactors = F)

track_summaryB <- merge(track_summary, n_birds[c(1,2,3, 4)], by=c("scientific_name", "device", "site_name"))
View(track_summaryB)
write.csv(track_summaryB, "data/analysis/summary_tables/track_summary_sp_site_device.csv", row.names = F)
