
# inFolder <- "data_test/popsum/"
# files <- list.files(folder)
# # files <- paste(folder, list.files(folder), sep="")
# files

## Species files
# spfiles <- files[str_detect(files, "Calonectris borealis")]
# spfiles
#### filter out a chosen population - ####
# spfiles <- spfiles[!str_detect(spfiles, "Prince Edward Islands")]

annual_sp_plot <- function(inFolder, plotFolder, viewPlot=TRUE) {
  
  pacman::p_load(stringr, ggplot2, dplyr)
  
  files <- list.files(inFolder)
  
  spp <- unique(do.call("rbind", str_split(files, pattern = "_"))[, 1])
  
  for(i in 1:length(spp)){
    
    one <- spp[i] # one species
    # spfiles <- paste(inFolder, files[str_detect(files, fixed(one))], sep = "") # all files for that species 
    spfile <- paste(inFolder, files[str_detect(files, fixed(one))], sep = "")
    
    ## load and combine all species' datasets
    spcnt <- readRDS(spfile)
    
    #####
    
    ### HERE SAVE TABLE FOR Maria ### (by special request!)
    
    # forMaria <- spcnt %>% dplyr::select(scientific_name, adj_site_name, month, jurisdiction, globprop) %>% arrange(adj_site_name, month)
    # 
    # write.csv(forMaria, paste("C:/Users/Martim Bill/Desktop/forMaria/", one, ".csv", sep=""), rownames=F)
    # 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## removing data from different years 
    spcnt <- spcnt %>% group_by(scientific_name, adj_site_name, month, jurisdiction) %>% 
      summarise(      
        origin        = first(origin),
        n_tracks      = first(n_tracks),
        globprop      = first(globprop),
        pop_estimate_IND = first(pop_estimate_IND),
        global_pop_estimate_IND = first(global_pop_estimate_IND),
        pop_p_known   = pop_estimate_IND / global_pop_estimate_IND,
        tot_atatime   = first(tot_atatime),
        prop_visiting = first(prop_visiting)
      )
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## calculate perc. of global population for which our data do not cover, and bind that to the estimates from tracking data
    glob_cnt <- spcnt %>% group_by(adj_site_name, month) %>%
      summarise(
        pop_estimate_IND = first(pop_estimate_IND),
        global_pop_estimate_IND = first(global_pop_estimate_IND),
        pop_p_known = pop_estimate_IND / global_pop_estimate_IND
      ) %>%
      ungroup() %>% group_by(month) %>%
      summarise(
        scientific_name = first(spcnt$scientific_name),
        eez = NA,
        adj_site_name = first(adj_site_name),
        n_tracks = NA,
        # pop_time = NA,
        # birds_est = NA,
        # n_visiting = NA,
        prop_visiting = NA,
        pop_estimate_IND = first(pop_estimate_IND),
        global_pop_estimate_IND = first(global_pop_estimate_IND),
        origin = NA,
        tot_atatime = NA,
        # min_visitors = NA,
        globprop = 1 - (sum(unique(pop_p_known))),  ### calc. percentage of global pop. for which story is unknown (in a given month)
        # political.area = "Unknown",
        jurisdiction   = "Unknown"
      ) %>% bind_rows(spcnt) %>% arrange(month)
    
    
    
    ###### plot prep
    
    plotsum <- glob_cnt %>% group_by(month, jurisdiction, adj_site_name) %>% summarise(
      n_tracks      = first(n_tracks),
      origin        = first(origin),
      prop_visiting = sum(prop_visiting),
      globprop      = sum(globprop),
      tot_atatime   = sum(tot_atatime)
    )
    plotsum
    
    # add unknown row for months missing data 
    miss_months <- setdiff(1:12, unique(plotsum$month))

    if(length(miss_months) > 0){
      for(k in 1:length(miss_months)){
        
        missed_month <- miss_months[k]
        
        plotsum <- tibble(
          month = missed_month,
          adj_site_name = first(plotsum$adj_site_name),
          jurisdiction  = "Unknown",
          n_tracks      = NA,
          origin        = NA,
          prop_visiting = NA,
          globprop      = 1,
          tot_atatime   = NA
        ) %>% bind_rows(plotsum) %>% arrange(month)
        
      } 
    }
    
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ####### Plot ####
    ## color by origin/host/high seas/unknown
    
    library(cowplot) ## for simple formatting of ggplot
    library(RColorBrewer)
    
    # Assigning variable political.area names origin status
    # plotsum$is_origin <- ifelse(plotsum$origin==plotsum$political.area, T, F)
    plotsum$is_origin <- ifelse(plotsum$origin==plotsum$jurisdiction, T, F)
    
    known <- plotsum %>% filter(jurisdiction != "Unknown") %>% ungroup() %>% summarise(sum = sum(na.omit(globprop))/12) %>% pull(sum)
    
    if(known > 0.5) {
      plotsum <- plotsum %>% group_by(jurisdiction) %>% mutate( insignificant = all(globprop < 0.05) ) %>% ungroup() %>% mutate(
        jurisdiction = if_else( (is_origin==TRUE) | (jurisdiction == "Unknown"), jurisdiction,
          if_else(insignificant==TRUE, "Other", jurisdiction)
        ) )
    } else {
      plotsum <- plotsum %>% group_by(jurisdiction) %>% mutate( insignificant = all(globprop < 0.005) ) %>% ungroup() %>% mutate(
        jurisdiction = if_else( (is_origin==TRUE) | (jurisdiction == "Unknown"), jurisdiction,
          if_else(insignificant==TRUE, "Other", jurisdiction)
        ) )
    }
    
    n_cats <- n_distinct(plotsum$jurisdiction)
    n_cats ## number of distinct political area categories (i.e. total)
    
    
    #### Unknown
    u.clr <- "grey"
    names(u.clr) <- "Unknown"
    
    #### High seas
    hs.clr <- "#af8dc3" #lavender
    names(hs.clr) <- "High seas"
    
    #### number of pol.area categories which are also origins
    origin   <- sort(unique(plotsum[plotsum$is_origin=="TRUE", ]$jurisdiction))
    n_origin <- n_distinct(na.omit(plotsum[plotsum$is_origin=="TRUE", ]$jurisdiction))
    
    ##### colors for breeding countries
    clr.func <- colorRampPalette(c('#ffeda0', "#fd8d3c", '#e31a1c')) #light yellow to red
    
    o.clrs <- clr.func(n_origin)
    # plot(rep(1, n_origin), col=o.clrs, pch=19, cex=3)
    
    names(o.clrs) <- levels(as.factor(origin))
    
    #### number of pol.area categories which are NOT also origins
    n_distinct(plotsum[plotsum$is_origin=="FALSE", ]$jurisdiction) ## total
    
    non_origin <- sort(unique(plotsum[plotsum$is_origin=="FALSE", ]$jurisdiction))
    non_origin <- non_origin[!non_origin=="High seas"] ## remove high seas category
    strict_non <- non_origin[!non_origin %in% origin]  ## remove origin countries (which were visited by other populations)
    length(strict_non)
    
    n_non_origin <- n_distinct(strict_non)
    
    
    ### Sort non_origin countries, with "Other" category last
    N_O_sorted <- sort(strict_non)
    if("Other" %in% N_O_sorted){
      N_O_sorted <- c(N_O_sorted[N_O_sorted!="Other"], "Other") ## HERE could paste the number of countries in "Other" i.e. (n=10)
    }
    
    ##### colors for non-breeding countries
    clr.func <- colorRampPalette(c('#ece2f0', '#08519c', '#0009A3'))
    #08519c '#B2EEFF'
    N_O_clrs <- clr.func(n_non_origin)
    # plot(rep(1, n_non_origin), col=N_O_clrs, pch=19, cex=3)
    
    names(N_O_clrs) <- N_O_sorted
    
    ### Combine into a color vector
    # allclrs <- c(o.clrs, no.clrs, u.clr, hs.clr)
    
    ## https://stackoverflow.com/questions/27803710/ggplot2-divide-legend-into-two-columns-each-with-its-own-title
    allclrs <- c(u.clr, hs.clr, "white", o.clrs, "white", N_O_clrs, "white") ## whites added to make spaces between origin/non categories
    
    colScale <- scale_fill_manual(name = "Jurisdiction", values = allclrs, drop=F)
    
    # Customize the order of factor levels for plot legend
    lvls <- c("Unknown", "High seas", "", sort(origin), " ", N_O_sorted) ## "" are empty levels to make spaces in plot legend
    
    plotsum$jurisdiction <- factor(plotsum$jurisdiction, levels=lvls)
    
    ###
    
    # ssizes <- plotsum %>% group_by(month, adj_site_name) %>% summarise(n_tracks = first(na.omit(n_tracks))) %>% group_by(month) %>% summarise(n_tracks = sum(na.omit(n_tracks))) %>% mutate(y_pos = rep(100))  # trying to add sample size labels, doesnt work
    
    p <- ggplot(plotsum, aes(x=factor(month, levels = 1:12), y=globprop * 100, fill=jurisdiction)) +
      geom_col() +
      scale_x_discrete(breaks=c(1:12), labels=month.abb[1:12], expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_bw() +
      xlab("") + ylab("Time spent (as % of global population)") +
      ggtitle(one) +
      theme(axis.text=element_text(size=15, color="black"), axis.title=element_text(size=18)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
      theme(legend.title = element_text(size=15)) +
      theme(legend.text = element_text(size=13)) +
      theme(plot.title = element_text(size=22)) +
      guides(fill=guide_legend(ncol=1)) +
      colScale
    
    if(viewPlot==TRUE){
      dev.new()
      
      print(p)
        # + geom_text(data=ssizes, aes(label = n_tracks, y=y_pos), position = position_dodge(0.9)) )## to view change in colorScale
      
    }
    ### save ###
    # ggsave("figures/test/species_annual_cycle_barcharts/Diomedea exulans.png", device="png")
    print(paste(plotFolder, one, ".png", sep=""))
    ggsave(paste(plotFolder, one, ".png", sep=""), width = 10, height = 7)
  }
  
}
