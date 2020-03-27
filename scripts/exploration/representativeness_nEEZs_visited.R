## Basic assessment of data coverage and monthly representativeness ##

pacman::p_load(dplyr, arrangements, scales)


## tabf comes from 'EEZ_overlay_birdday_calc.R' script

tabf <- readRDS("data_test/tabs/Calonectris borealis_Madeira_tabf.rds")
# tabf <- readRDS("data_test/tabs/Calonectris diomedea_Balearic Archipelago_tabf.rds")


####### Basic plot of annual coverage ######
sp <- first(tabf$scientific_name)
site <- first(tabf$site_name)
folder <- "figures/test/annual_coverage_spp.site/"

png(paste(folder, paste(sp, site, sep = "_"), ".png"), width=10, height = 10, res=200, units = 'in')
# dev.new()

with(tabf,
  hist(yday, breaks=seq(0,366,1), xlim=c(1,366), col='grey40', border='grey40', xaxt = 'n', xlab=NULL, ylab='Number of Tracks', main=paste(first(tabf$common_name), first(tabf$site_name), sep=' - '), cex.main=1))
axis(side = 1, at=c(32,91,152,213,274,335), month.abb[c(2,4,6,8,10,12)] )

dev.off()

sumeez <- tabf %>% group_by(month) %>% summarise(
                                        n_birds = n_distinct(track_id), 
                                        n_eezs  = n_distinct(eez))
dev.new()
plot(sumeez$n_eezs ~ sumeez$n_birds)


#### Plot saturation curve of the number of EEZs visited by varying sizes of tracked sample of birds #####

sp <- first(tabf$scientific_name)
site <- first(tabf$site_name)
folder <- "figures/test/"

png(paste(folder, paste(sp, site, sep = "_"), ".png"), width=12, height = 8, res=200, units = 'in')
# dev.new()
par(mfrow=c(3,4))

neez_list <- list()

for(m in 1:n_distinct(tabf$month)){ # iterate over months w/ tracking data
  # one month
  one <- subset(tabf, month==m)
  
  ids <- unique(one$track_id)
  
  eez.vec <- vector()
  df.list <- list()
  
  if(length(ids) > 2) {
  
  for(i in 1:(n_distinct(one$track_id) - 1)) { # iterate through number of individals tracked in a month
    
    n_combs <- choose(n_distinct(one$track_id), i) # n combinations
    
    if(n_combs>100) {n_combs <- 100} # limit number of combinations to 100
    
    combos <- combinations(x=ids, k=length(ids) - i, nsample=n_combs, replace=F) # calculate each possible combination w/out replacement
    
      for(j in 1:nrow(combos)){  
        
        samp <- one[which(one$track_id %in% combos[j, ]), ] # remove un-sampled tracks
        
        eez.vec[j] <- n_distinct(samp$eez)
        
      }
    
    df <- data.frame(n_ids=rep(n_distinct(one$track_id) - i), n_eez=eez.vec) 
    df.list[[i]] <- df
    
  } 
  
  eez.df <- do.call('rbind', df.list)
  
  eez.sum <- eez.df %>% group_by(n_ids) %>% summarise(month    = rep(m),
                                                      n_eez_mn = mean(n_eez), 
                                                      n_eez_md = median(n_eez),
                                                      n_eez_mx = max(n_eez),
                                                      n_eez_mi = min(n_eez)
                                                      )
  
  with(eez.sum, # plot by MEAN
    plot(n_ids, n_eez_mn,
      ylim=c(0, max(na.omit(n_eez_md))),
      main=month.abb[first(one$month)], 
      xlab='Sample size (n)',
      ylab='EEZs visited (count)', pch=16))
  
  # with(eez.sum, # plot by MEDIAN
  #   plot(n_ids, n_eez_md,
  #     ylim=c(0, max(na.omit(n_eez_md))),
  #     main=paste(month.abb[first(one$month)],
  #       paste('n =', n_distinct(one$track_id), sep=' '), sep=' - ')))

  with(eez.sum, # plot by MEAN
    points(n_ids, n_eez_mi, col=alpha('blue', 0.5), pch=20))
  with(eez.sum, # plot by MEAN
    points(n_ids, n_eez_mx, col=alpha('blue', 0.5), pch=20))
  
  # visualize fit of logarithmic function
  # fit <- with(eez.sum,
  #   lm(n_eez_mn ~ log(n_ids)))
  x = eez.sum$n_ids
  y = eez.sum$n_eez_mn
  
  fit <- nls(y ~ (a*x)/(1+b*x), start=list(a=1, b=0.1)) # model used in Lascelles et al. 2016
  
  with(eez.sum, 
    points(n_ids, predict(fit),col='red', type='l'))
  
  } else {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, "n < 3", 
      cex = 2, col = "black")
  }
  
  neez_list[[m]] <- eez.sum
  
}

dev.off()

neez_df <- do.call('rbind', neez_list)

# saveRDS(neez_df, 'data_test/representativeness/COSH_Madeira_GLS_represent.rds')


## function

