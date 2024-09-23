### Function to down-sample time-series (i.e. tracking) data to match a desired time interval as close as possible to an ideal interval, centered on a desired time of day (e.g. 12hours interval starting at 00:00) ###

## **** Original function written by Emiel van Loon (2016) ##

# Emiel edit (17-07-17) - to allow function to run just one individual
# MB edit (21-03-17) - the following are copies with just the year changed
# MB edit (08-01-19) - added data-specific min/max for target interval 
#  - switched unit from 'mins' to 'hours' (but fxn accepts either)

# TD is dataframe of Tracking Data set 
# dt = time step in chosen unit
# onlyone = TRUE/FALSE - does data set only contain one individual?

# individuals/track IDs must be in column named "track_id"
# date time must be in column named "date_time

# Other potential edits to make: could set the ideal interval timing to the start of the track time, instead of a preset ideal time (e.g. time of first point instead of 00:00)

SubSamp <- function(TD,dt=1,unit='hours',onlyone=FALSE)
{
  require(lubridate)
  ## MB add ## 
  if(!"date_time" %in% names(TD)) stop("date_time field does not exist")
  if(!"track_id" %in% names(TD)) stop("track_id field does not exist")
  
  # print( paste("dataset:", first(TD$dataset_id)) ) # for troubleshooting, to know which data_set is being processed
  
  minyear  <- min(na.omit(year(TD$date_time)))
  maxyear  <- max(na.omit(year(TD$date_time)))
  
  minmonth <- min(na.omit(month(TD$date_time)))
  maxmonth <- max(na.omit(month(TD$date_time)))
  
  minmday  <- min(na.omit(day(TD[month(TD$date_time)==minmonth,]$date_time))) # min/max day in the min/max month of tracking data
  maxmday  <- max(na.omit(day(TD[month(TD$date_time)==maxmonth,]$date_time)))
  
  # make the desired time sequence
  timestep <- paste(dt,unit)
  dati_start <- as.POSIXct(paste(paste(minmday, minmonth, minyear, sep='/'), '00:00:00', sep=' '),'%d/%m/%Y %H:%M:%S',tz='UTC')
  dati_end <- as.POSIXct(paste(paste(maxmday, maxmonth, maxyear, sep='/'), '00:00:00', sep=' '),'%d/%m/%Y %H:%M:%S',tz='UTC')
  datiseq <- seq(from=dati_start, to=dati_end, by=timestep)

  
  # breakdown to datasets per bird, if needed
  if(!onlyone){
    unid <- unique(TD$track_id)
    nrid <- length(unid)
    if(nrid==1){onlyone <- TRUE}
  }
  
  if(onlyone){
    
    idx = sapply(datiseq, function(x) which.min( abs( difftime( TD$date_time, x, units='hours') ) ) )
    TDr <- unique( TD[idx,] )     
    
  } else { 
    
    TDred <- list(nrid)
    
    for (i in 1:nrid)
    {
      # print(paste("data set: ", unid[i], sep = "")) # print ID for troubleshooting 
      Dtemp = TD[TD$track_id==unid[i],]
      idx = sapply(datiseq, function( x) which.min( abs( difftime( Dtemp$date_time, x, units='hours') ) ) )
      TDred[[i]] = unique( Dtemp[idx,] )     
      # the function unique makes sure that the rows in Dtemp[idx,] are unique - so no duplicate points
    }
    
    # this is probably redundant - for now left in the function
    TDr = TDred[[1]]
    for(i in 2:nrid) {TDr = rbind(TDr,TDred[[i]])}
  }
  
  return(TDr)
}