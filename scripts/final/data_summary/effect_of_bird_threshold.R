#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Compare low threshold to high threshold results ##

pacman::p_load(dplyr)

sum_hi <- read.csv("data/analysis/bird_thresh/summary_tables/richness_visit_per_jur.csv")
sum_lo <- read.csv("data/analysis/summary_tables/richness_visit_per_jur.csv")

# difference in species richness # 
richdiff <- sum_lo$richness - sum_hi$richness

# number of fewer bird years estimated in each country #
timediff <- sum_lo$sum_tot_staying - sum_hi$sum_tot_staying
diff_df_ju <- data.frame(jurisdiction=sum_lo$jurisdiction, 
  richness_low=sum_lo$richness, richness_hi=sum_hi$richness, richdiff=richdiff, 
  timespent_low=sum_lo$sum_tot_staying, timespent_hi=sum_hi$sum_tot_staying, timediff=round(timediff, 0))

diff_df_ju

# difference in total annual time estimated for global population of large petrels
paste(sum(sum_lo$sum_tot_staying) , "vs", sum(sum_lo$sum_tot_staying) - sum(diff_df_ju$timediff), "bird-years")


# difference in annual coverage for each species #
cover_hi <- read.csv("data/analysis/bird_thresh/summary_tables/sp_time_spent.csv", stringsAsFactors = F)
cover_lo <- read.csv("data/analysis/summary_tables/sp_time_spent.csv", stringsAsFactors=F)

cover_diff <- cover_lo$glob_ann_cov - cover_hi$glob_ann_cov
diff_df_sp <- data.frame(scientific_name=cover_lo$scientific_name, 
  ann_cover_low=cover_lo$glob_ann_cov, ann_cover_high=cover_hi$glob_ann_cov, 
  ann_cover_diff=cover_diff)

diff_df_sp

# average change in annual species coverage 
diff_df_sp %>% summarise(
  med_lo = median(ann_cover_low),
  med_hi = median(ann_cover_high),
  mean_lo = mean(ann_cover_low),
  mean_hi = mean(ann_cover_high),
  mean_diff = mean(ann_cover_diff),
  sd_diff   = sd(ann_cover_diff),
  min_diff = min(ann_cover_diff),
  max_diff = max(ann_cover_diff)
)
