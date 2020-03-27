## DELETE ALL FILES IN THESE FOLDERS ## 

## FILTER STEPS ##
#1#
do.call(file.remove, list(list.files("data/analysis/all_TD", full.names=T)))
#2#
# do.call(file.remove, list(list.files("data/analysis/first_year", full.names=T)))
#3#
do.call(file.remove, list(list.files("data/analysis/noeqx_dnsmpl", full.names=T)))
# do.call(file.remove, list(list.files("data/analysis//pheno_classed", full.names=T)))

do.call(file.remove, list(list.files("data/analysis/month_filtered", full.names=T)))

## CALCULATION STEPS ## 
# EEZ #
do.call(file.remove, list(list.files("data/analysis/oveez", full.names=T)))

do.call(file.remove, list(list.files("data/analysis/birddays_eez", full.names=T)))

do.call(file.remove, list(list.files("data/analysis/glob_count", full.names=T)))

# RFMO #
do.call(file.remove, lapply(as.list( paste0(list.files("data/analysis/ovrfmo", full.names=T), "/") ), function(x) list.files(x, full.names = T))) # rfmo
"data/analysis/oveez_hs/"
do.call(file.remove, lapply(as.list( paste0(list.files("data/analysis/oveez_hs/", full.names=T), "/") ), function(x) list.files(x, full.names = T)))
do.call(file.remove, lapply(as.list( paste0(list.files("data/analysis/birddays_rfmo", full.names=T), "/") ), function(x) list.files(x, full.names = T))) # rfmo
do.call(file.remove, lapply(as.list( paste0(list.files("data/analysis/glob_count_rfmo", full.names=T), "/") ), function(x) list.files(x, full.names = T))) # rfmo
