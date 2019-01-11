## Basic assessment of monthly representativeness ##

library(arrangements)


## tabf comes from 'EEZ_overlay_birdday_calc.R' script
####### How many EEZs visited per month? ######

sumeez <- tabf %>% group_by(month) %>% summarize(
                                        n_birds = n_distinct(track_id), 
                                        n_eezs  = n_distinct(eez))

plot(sumeez$n_eezs ~ sumeez$n_birds)


#### Plot saturation curve of the number of EEZs visited by varying sizes of tracked sample of birds #####

dev.new()

par(mfrow=c(3,4))

neez_list <- list()

for(m in 1:n_distinct(tabf$month)){ # iterate over months w/ tracking data
  # one month
  one <- subset(tabf, month==m)
  
  ids <- unique(one$track_id)
  
  eez.vec <- vector()
  df.list <- list()
  
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
  
  eez.sum <- eez.df %>% group_by(n_ids) %>% summarize(month    = rep(m),
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
  fitlog <- with(eez.sum,
    lm(n_eez_mn ~ log(n_ids)))
  with(eez.sum, 
    points(n_ids, predict(fitlog),col='red', type='l'))
  
  
  neez_list[[m]] <- eez.sum
  
}

neez_df <- do.call('rbind', neez_list)

saveRDS(neez_df, 'data/representativeness/COSH_Madeira_represent.rds')

