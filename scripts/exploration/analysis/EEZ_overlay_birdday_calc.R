####### intial script for identifying the overlap of individual birds with different EEZs and calculating bird-days spent in different EEZs per month #######

#MB# largely follows Maria's script 'analysis_Atlantic Migrants_AUX' script with a few changes to the bird-days calculation, the object names, as well as translation to the tidyverse 'language'

pacman::p_load(tidyverse, lubridate, maps, sp, maptools, rgdal, devtools, plotrix, geosphere, igraph, raster, scales, Hmisc)

### ANALYSIS PER SPECIES: OVERLAP WITH EEZS

### 
# setwd('C:/Users/Martim Bill/Documents/political_connectivity/')
# tab <- read.csv('data_test/raw_tracking/Calonectris borealis_GLS.csv') # data compiled by MD
tab <- TDdown ## data from 'downsample_track.R' script


#### MB # read in Tracking Data (TD) ####

#unique(tab$colony_name)
unique(tab$site_name)

# sname <- c("Madeira")
sname <- first(tab$site_name)

tab1 <- tab[tab$site_name %in% sname,] # subset one island group

unique(tab1$colony_name)
unique(tab1$bird_id)
n_distinct(tab1$track_id)


################ MB # Data Prep #######

tab1$date_gmt <- as.POSIXct(strptime(tab1$date_gmt, "%Y-%m-%d"), "GMT")
#tab1$dategmt=as.POSIXct(strptime(tab1$date_gmt, "%m/%d/%Y"), "GMT")
tab1$month <- month(tab1$date_gmt)
tab1$yday <- yday(tab1$date_gmt)

head(tab1)

tab1 <- tab1[!is.na(tab1$latitude),]
tab1 <- tab1[!is.na(tab1$longitude),]

## Basic overview of monthly datasets for a species-site combo
# res <- tab1 %>% group_by(month) %>% summarise(
#   n_pnts = n(), 
#   nbirds = n_distinct(bird_id),
#   nperbird = (n_pnts/nbirds) #MB# points per month / birds per month = monthly points per bird 
# )
# res
## track_id instead of bird_id
res <- tab1 %>% group_by(month) %>% summarise(
  n_days = n_distinct(yday),       # n days tracked in each month
  n_pnts = n(),                    # n points per month
  n_birds = n_distinct(track_id),  # n birds
  pntsper = (n_pnts/n_birds)       # points per month / birds per month = monthly points per bird 
)
res


######## #MB# create SPntsDF with TD ######
wgs84 <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
tabsp <- SpatialPointsDataFrame(SpatialPoints(data.frame(tab1$longitude, tab1$latitude), proj4string=wgs84), data = tab1)

# load spatial data # 
world <-  readOGR(dsn="Atlantic_migrants_MD/analysis/shapefiles_martin", layer="mundo")
ocean <- readOGR(dsn="Atlantic_migrants_MD/analysis/shapefiles_martin", layer="Ocean")
eez <- readOGR(dsn="Atlantic_migrants_MD/analysis/shapefiles_martin", layer="EEZ")
eez_4plot <- readOGR(dsn='data_test/geodata/World_EEZ_v10_20180221', layer='eez_boundaries_v10') #MB# use lines dataset for plots b/c it's smaller 
cnt <- readOGR(dsn="Atlantic_migrants_MD/analysis/shapefiles_martin", layer="country")


# overlay points with eez
oveez <- over(tabsp, eez)

tabsp$eez <- as.character(oveez$Country) #MB# add overlay results to SPntsDF
str(tabsp)
tabsp$eez[is.na(tabsp$eez)] <- 0 #MB# NAs to 0s (land points or high seas?)

# overlap with land
ovcnt <- over(tabsp, cnt)
tabsp$land <- as.character(ovcnt$POLITICNAM)
tabsp$land[is.na(tabsp$land)] <- 0

tabsp$eez[tabsp$eez==0] <- tabsp$land[tabsp$eez==0]  ## assumes that birds are in the country's eez when over land
#MB# if a point is (erroneously) over land (w/in the border) of a country it is assigned to its EEZ


#### #### #### plot points and EEZs #### #### ####

plot(tabsp, pch=20, col=alpha('black', 0.1))
plot(world, add=T, border='dark grey')
plot(eez_4plot, add=T, col=2)                    # changed for SPLinesDF
# points(tabsp[tabsp$eez=="Mauritania",], col=3)
# points(tabsp[tabsp$eez=="0",], col=5)          # high seas in light blue
points(tabsp[tabsp$device=="GPS",], pch=20, col=alpha(4, 0.1))       # device type
# plot(cnt, add=T, border=3)


#####

tabf <- tabsp@data
head(tabf)


############ calculate (weighted) bird-days ############################

tabf$month.dur <- monthDays(tabf$date_time) 

ndays <- tabf %>% group_by(month, bird_id) %>% summarise(dmi=n_distinct(yday)) # dmi = day-month-individual

# calculate simple weights from the proportion of the day spent in each EEZ, based on number of EEZs per day
weights <- tabf %>% group_by(month, bird_id, yday) %>% summarise(
  n_eez=n_distinct(eez), # number of eezs visited by each individual on each day tracked 
  pp_eez = 1/n_eez # basic proportion of time spent by a bird in each eez
)

tabwei <- merge(tabf, weights)

tabwei$eez <- ifelse(tabwei$eez=='0', 'High seas', tabwei$eez)

weighted <- tabwei %>% group_by(month, eez, bird_id, yday) %>% summarise(weights = first(pp_eez))
head(weighted)

weighted <- merge(weighted, ndays)
head(weighted)

# bird-days dataset
brdy <- weighted %>% group_by(month, eez, bird_id) %>% summarise(dmi = first(dmi),
                                                                   dmei = sum(weights), # dmei = day-month-eez-individual
                                                                   ppt = dmei/dmi # ppt = proportion of time (days/month)
)

head(brdy) 

### check that the sums of the relative time in an eez per month (by an individual) is 1!
# sumppt <- brdy %>% group_by(month, bird_id) %>% summarise(fullpp=sum(ppt))

brdy2 <- subset(brdy, dmi>9)

nrow(brdy2)
nrow(brdy)

## n birds with good samples per month
nperm <- brdy2 %>% group_by(month) %>% summarise(n_birds = n_distinct(bird_id))
nperm

brdy3 <- merge(brdy2, nperm)
head(brdy3)

# contribution of that individual's relative time in an eez during a given month adjusted for the contribution of that individual to the sample/population's 'story'
brdy3$ppts <- brdy3$ppt*(1/brdy3$n_birds) #MB# individual's relative time spent * (1/number of birds tracked per month)

sumppts <- brdy3 %>% group_by(month, eez) %>% summarise(fullpp=sum(ppts))


### plot #####

sum4plot <- sumppts
# rename insignificant countries to 'Other'
sum4plot$eez <- ifelse(sumppts$fullpp<0.01, 'Other', sumppts$eez)
sum4plot <- sum4plot %>% group_by(month, eez) %>% summarise(fullpp=sum(fullpp))

months <- sort(unique(sum4plot$month)) #, decreasing=T
path <- 'figures/test'  # set plot path and parameters

for (g in months){
  # g=1
  jpeg(file.path(path, paste(g, month.abb[g], ".jpeg", sep="")), width=12, height = 6, res=100, units = 'in')
  summ <- sum4plot[sum4plot$month==g,]
  summ <- summ[order(summ$fullpp),]
  
  # win.graph(12,6)
  par(mfrow=c(1,2))
  with(summ, pie(fullpp, labels=eez, main=month.abb[g]))
  plot(tabsp[tabsp$month==g,])
  plot(world, add=T, border=8)
  plot(eez_4plot, add=T, col=2)
  box()
  dev.off()
}

#### Save species-site dataset ####

# sp <- unique(tabf$scientific_name)
# 
# tabffname <- paste(sp, "_", sname, "_tabf.csv", sep="")
# path <- 'data_test/tabf_tracking/'
# 
# write.csv(tabf, paste(path, tabffname, sep=""), row.names=F)
# 
## Save bird-days datasets ##
# ## 1 ##
# brdyfname <- paste(sp, "_", sname, "_brdy.csv", sep="")
# path <- 'data_test/birddays/'
# 
# write.csv(brdy, paste(path, brdyfname, sep=""), row.names=F)
# 
# ## 2 ##
# brdy2fname <- paste(sp, "_", sname, "_brdy2.csv", sep="")
# path <- 'data_test/birddays/'
# 
# write.csv(brdy2, paste(path, brdy2fname, sep=""), row.names=F)
# 
# ## 3 ##
# brdy3fname <- paste(sp, "_", sname, "_brdy3.csv", sep="")
# path <- 'data_test/birddays/'
# 
# write.csv(brdy3, paste(path, brdy3fname, sep=""), row.names=F)
