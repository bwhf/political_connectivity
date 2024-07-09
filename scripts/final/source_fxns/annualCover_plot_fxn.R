## function to visualize or save plots of coverage of tracking data (by device type) over annual cycle for each species, site ## 

## files: the specific files to be used (i.e. .rds formatted files of tracking data), to be used for subsetting a folder
## 
## inFolder: path to folder containing tracking data (formatted for Seabird Tracking Database). Files must be .rds format
## byYear: if FALSE, plots will be aggregates of unique tracks per day, if TRUE, split by year
## savePlot: if TRUE, ggsave plots to desired folder
## saveFolder: folder to which plots will be saved. Plots will be saved within saveFolder, in species-specific folders, created and named based on the column $common_name in the data.frame

# annualCover_plot(files, savePlot = T, saveFolder="C:/Users/Martim Bill/Desktop/test/")
# annualCover_plot(files, byYear=T, savePlot = T, saveFolder="C:/Users/Martim Bill/Desktop/test/")

annualCover_plot <- function(files, inFolder, byYear=FALSE, pheno=NULL, savePlot = FALSE, saveFolder) { # byYear=FALSE, savePlot=FALSE
  
  pacman::p_load(ggplot2, RColorBrewer, lubridate, stringr)
  
  spp.list <- lapply(strsplit(files, "_"), function(x) x[[1]])  # get species names
  site.list <- lapply(strsplit(files, "_"), function(x) x[[2]])  # get site names
  # sites <- unique(as.vector(do.call("rbind", site.list)))
  spp.sites <- unique(paste(spp.list, site.list, sep="_"))

  for(f in 1:length(spp.sites)){             ## Loop through each site
    
    spp.site.files <- files[str_detect(files, fixed(spp.sites[f]))]  # fixed() needed to deal with punctuation marks in names (i.e. "(")
    print(f)
    # tab <- readRDS(paste(folder, files[f], sep = ""))
    tab <- do.call( "rbind", lapply( spp.site.files, function(x) as.data.frame(readRDS(paste(inFolder, x, sep=""))) ))
      
    tab$device <- factor(tab$device, levels=c("GLS", "GPS", "PTT"))
    
    sp     <- first(tab$scientific_name)  # species (sci. name)
    sp_com <- first(tab$common_name)      # species (common name[for folder/file])
    sname  <- unique(tab$site_name)[1]       # island group (site name)
    
    tab$year <- year(tab$date_time)      # add year column
    tab$yday <- yday(tab$date_time)
    deviceColors <- brewer.pal(3,"Dark2")
    
    names(deviceColors) <- levels(tab$device)
    colScale <- scale_fill_manual(name = "device", values = deviceColors)
    
    tab <- tab %>%
      group_by(yday, device, track_id) %>%
      summarise()
    
    if(byYear==FALSE) {
      
    aplot <- ggplot(tab, aes(x=yday, fill=device)) +
      coord_cartesian(xlim=c(0,366)) +
      geom_histogram(binwidth = 1) +
      labs(x="",y="Number of tracks") +
      scale_x_continuous(breaks=c(32,91,152,213,274,335), labels=month.abb[c(2,4,6,8,10,12)], expand=c(0,0)) +
      scale_y_continuous(expand = expansion(mult = c(0, .05))) +
      ggtitle(paste(sp, sname, sep=" - ")) +
      colScale +  # device colors
      theme_bw()
    
    if(!is.null(pheno)) {
      
      sp_pheno <- pheno[pheno$scientific_name == sp, ]
      # if multiple sites, get the specific one
      sp_site_pheno <- sp_pheno[sp_pheno$site_name == sname, ]
      
      
      if( nrow(sp_site_pheno) > 0 & !any(is.na(sp_site_pheno$mean_yday)) ){
      
        dates <- tidyr::spread(sp_site_pheno, breed_stage, mean_yday) %>% summarise(
          spp.site = first(sp.site),
          pre_lay = first(na.omit(`pre-laying`)),
          egg_lay = first(na.omit(incubation)),
          hatch = first(na.omit(brood)),
          post_brood = first(na.omit(`post-brood`)),
          nonbreed = first(na.omit(`Total NB`))
        )
        aplot <- aplot + geom_vline(data=dates, aes(xintercept = pre_lay), color="blue", size=1) + 
          geom_vline(data=dates, aes(xintercept = egg_lay), color="purple", linetype="dashed", size=1) +  
          geom_vline(data=dates, aes(xintercept = hatch), color="deeppink", linetype="dashed", size=1) + 
          geom_vline(data=dates, aes(xintercept = nonbreed), color="red", size=1)
        }
      } else { warning("No phenological data set specified to pheno argument")}
    
    suppressWarnings(ggsave(file.path(paste(saveFolder, paste(sp_com, sname, sep = "_"), ".png", sep="")), plot = aplot, width = 10, height = 7, dpi=100))
    
      # if(savePlot==TRUE){
      #   if(dir.exists(paste(saveFolder, sp_com, sep=""))) {  # if species-specific folder doesn't exist, make one
      #     newSaveFolder <- paste(saveFolder, sp_com, "/", sep="")
      #     suppressWarnings(ggsave(file.path(paste(newSaveFolder, paste(sp_com, sname, sep = "_"), ".png", sep="")), plot = aplot, width = 10, height = 10, dpi=100))
      #   } else {
      #     dir.create(paste(saveFolder, sp_com, sep=""))
      #     newSaveFolder <- paste(saveFolder, sp_com, "/", sep="")
      #     suppressWarnings(ggsave(file.path(paste(newSaveFolder, paste(sp_com, sname, sep = "_"), ".png", sep="")), plot = aplot, width = 10, height = 10, dpi=100))
      #   }
      # } else { print(aplot) }
    
    } else {
      
      aplot2 <- ggplot(tab, aes(x=yday, fill=device)) +
        coord_cartesian(xlim=c(0,366)) +
        geom_histogram(binwidth = 1) +
        labs(x="",y="Number of tracks") +
        scale_x_continuous(breaks=c(32,91,152,213,274,335), labels=month.abb[c(2,4,6,8,10,12)], expand=c(0,0)) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        facet_wrap(~year, scales="free_y", dir="v") +
        ggtitle(paste(first(tab$scientific_name), first(tab$site_name), sep=" - ")) +
        colScale +  # device colors
        theme_bw()
      
      # Save plot
      suppressWarnings(ggsave(file.path(paste(saveFolder, paste(sp_com, sname, sep = "_"), ".png", sep="")), plot = aplot2, width = 10, height = 7, dpi=100))
      
      # if(savePlot==TRUE){
      #   if(dir.exists(paste(saveFolder, sp_com, sep=""))) {  # if species-specific folder doesn't exist, make one
      #     newSaveFolder <- paste(saveFolder, sp_com, "/", sep="")
      #     suppressWarnings(ggsave(file.path(paste(newSaveFolder, paste(sp_com, sname, sep = "_"), ".png", sep="")), plot = aplot2, width = 10, height = 7, dpi=100))
      #   } else {
      #     dir.create(paste(saveFolder, sp_com, sep=""))
      #     newSaveFolder <- paste(saveFolder, sp_com, "/", sep="")
      #     suppressWarnings(ggsave(file.path(paste(newSaveFolder, paste(sp_com, sname, sep = "_"), ".png", sep="")), plot = aplot2, width = 10, height = 7, dpi=100))
      #   }
      # } else { print(aplot2) }
    }
  }
}


