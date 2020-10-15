## Calculate intermediate time spent values for reporting in Methods exmaples of equation steps ## 

# Selvagens # 
madeira <- alltimes %>% filter(scientific_name == "Calonectris borealis" & site_name == "Madeira") %>% 
  group_by(site_name, month, jurisdiction) %>% summarise_all(
  list( ~first(.) )
)

hs_mad <- madeira %>% group_by(month, jurisdiction) %>% summarise(
  tspme = sum(tot_atatime)
) %>% ungroup() %>% filter(jurisdiction == "High seas")

bm_mad <- hs_mad %>% summarise( total=sum(tspme) )
by_mad <- bm_mad$total / 12
by_mad/madeira$global_pop_estimate_IND[1] # prop. species' time

# Azores # 
azores <- alltimes %>% filter(scientific_name == "Calonectris borealis" & site_name == "Azores") %>% 
  group_by(site_name, month, jurisdiction) %>% summarise_all(
    list( ~first(.) )
  )

## Azores ## 
hs_azo <- azores %>% group_by(month, jurisdiction) %>% summarise(
  tspme = sum(tot_atatime)
) %>% ungroup() %>% filter(jurisdiction == "High seas")

bm_az <- hs_azo %>% summarise( total=sum(tspme) )
bm_az
by_az <- bm_az$total / 12
by_az
by_az/azores$global_pop_estimate_IND[1] # prop. species' time

# Berlengas # 
berl <- alltimes %>% filter(scientific_name == "Calonectris borealis" & site_name == "Portugal") %>% 
  group_by(site_name, month, jurisdiction) %>% summarise_all(
    list( ~first(.) )
  )

hs_berl <- berl %>% group_by(month, jurisdiction) %>% summarise(
  tspme = sum(tot_atatime)
) %>% ungroup() %>% filter(jurisdiction == "High seas")

bm_be <- hs_berl %>% summarise( total=sum(tspme) )
bm_be
by_be <- bm_be$total / 12
by_be
by_be/berl$global_pop_estimate_IND[1] # prop. species' time


totals <- data.frame(
  pop = c("Madeira", "Azores", "Berlengas"),
  b_month = c(bm_mad$total, bm_az$total, bm_be$total),
  b_year = c(by_mad, by_az, by_be)
  )
totals %>% mutate(
  b_month = round(b_month),
  b_year  = round(b_year)
)
