# Simulate changes from House-President models, using 
#year = 2020

simChange_FUNCTION = function(year){
# Needs mdiAll, mdiHouseAll, mdiHouseCompet (use as argument?)
# Needs r_hp_2020 and r_hp_2022 (use as argument?)
  
results         = kappa_eachYear_House_FUNCTION(year,
                                                compare = "Pres", 
                                                measure = "avgText")$compet_covar
results_hiSpend = kappa_eachYear_House_FUNCTION(year,
                                                compare = "Pres", 
                                                measure = "avgText")$hispend_covar

# Assess with hispending
#results = results_hiSpend

# Orig: path = "../4_results/"
path = "Data/"
house_dist <- read_csv(paste0(path, "generalElecResults_withIdeal_", year, "_labelFish.csv"), 
                       show_col_types = FALSE) %>% 
  filter(senate==0 & stateDist != "US-00") %>% 
  filter(!duplicated(stateDist)) %>% #Eliminate rows with repeated house - districts (will fix in .csv) 
  mutate(trump2016Pct2Party = trump2016Pct/( trump2016Pct+ clinton2016Pct), 
         trump2020Pct2Party = trump2020Pct/( trump2020Pct+ biden2020Pct), 
         house_dem_receipt  = dem_receipt,
         house_rep_receipt  = rep_receipt,
         house_dem_inc      = dem_inc,
         house_rep_inc      = rep_inc,
         kappaHouse = 0.5*(dem_avgIdeal + rep_avgIdeal))

if(year == 2020) house_dist  = house_dist %>%
  mutate(demWin = case_when(is.na(demCong2020_count)==1 ~ 0,
                   is.na(repCong2020_count)==1 ~ 1,
                   TRUE ~ demCong2020_count > repCong2020_count)) %>%
  dplyr::select(stateDist, demPct, demPct2Party, demWin, 
              dem_avgIdeal, rep_avgIdeal, kappaHouse, house_dem_receipt, house_rep_receipt,
              house_dem_inc,house_rep_inc, trump2016Pct, trump2020Pct)

if(year == 2022) house_dist  = house_dist %>%
  mutate(demWin = case_when(is.na(demCong2022_count)==1 ~ 0,
                            is.na(repCong2022_count)==1 ~ 1,
                            TRUE ~ demCong2022_count > repCong2022_count)) %>%
           dplyr::select(stateDist, demPct, demPct2Party, demWin, 
                         dem_avgIdeal, rep_avgIdeal, kappaHouse, house_dem_receipt, house_rep_receipt,
                         house_dem_inc,house_rep_inc, trump2016Pct, trump2020Pct)

house_dist_compet <- house_dist %>% filter(trump2020Pct > 41 & trump2020Pct < 59 & 
                                           (house_dem_receipt > 0.1|house_dem_inc==1) & 
                                             (house_rep_receipt>0.1|house_rep_inc==1))

# Fitted values for models
## Create moderate/extremes based on percentiles of partisan distributions
dem_mod = quantile(house_dist$dem_avgIdeal, 0.95, na.rm=T)
dem_ext = quantile(house_dist$dem_avgIdeal, 0.05, na.rm=T)
rep_mod = quantile(house_dist$rep_avgIdeal, 0.05, na.rm=T)
rep_ext = quantile(house_dist$rep_avgIdeal, 0.95, na.rm=T)

# Calculate bump associated with changing to moderate/extreme candidate based on estimated effect
kappa_tmp    = coefficients(results)["kappaDiff_HousePres"]
kappa_tmp_hs = coefficients(results_hiSpend)["kappaDiff_HousePres"]

bumpCalc = house_dist_compet %>%   # want house_dist_compet but note NAs in dem_avgIdeal, rep_avgIdeal
  mutate(kappaHouse        = 0.5*(dem_avgIdeal + rep_avgIdeal),
         dem_mod.use = case_when(dem_avgIdeal>dem_mod~ dem_avgIdeal,
                                  TRUE ~ dem_mod),
         rep_mod.use = case_when(rep_avgIdeal<rep_mod~ rep_avgIdeal,
                                 TRUE ~ rep_mod),
         changeY_modDem_actRep = kappa_tmp*(0.5*(dem_mod.use - dem_avgIdeal)),       # Dem moderate & actual Republican
         changeY_modDem_extRep = kappa_tmp*(0.5*(dem_mod.use + rep_ext) - kappaHouse), # Dem moderate & extreme Republican
         changeY_actDem_modRep = kappa_tmp*(0.5*(rep_mod.use - rep_avgIdeal)),       # Dem actual   & moderate Republican
         changeY_extDem_modRep = kappa_tmp*(0.5*(dem_ext + rep_mod.use) - kappaHouse),
         fit_modDem_actRep  = demPct2Party + changeY_modDem_actRep,
         fit_modDem_extRep  = demPct2Party + changeY_modDem_extRep,
         fit_actDem_modRep  = demPct2Party + changeY_actDem_modRep,
         fit_extDem_modRep  = demPct2Party + changeY_extDem_modRep,
         changeY_modDem_actRep_hs = kappa_tmp_hs*(0.5*(dem_mod.use - dem_avgIdeal)),       # Dem moderate & actual Republican
         changeY_modDem_extRep_hs = kappa_tmp_hs*(0.5*(dem_mod.use + rep_ext) - kappaHouse), # Dem moderate & extreme Republican
         changeY_actDem_modRep_hs = kappa_tmp_hs*(0.5*(rep_mod.use - rep_avgIdeal)),       # Dem actual   & moderate Republican
         changeY_extDem_modRep_hs = kappa_tmp_hs*(0.5*(dem_ext + rep_mod.use) - kappaHouse),
         fit_modDem_actRep_hs  = demPct2Party + changeY_modDem_actRep_hs,
         fit_modDem_extRep_hs  = demPct2Party + changeY_modDem_extRep_hs,
         fit_actDem_modRep_hs  = demPct2Party + changeY_actDem_modRep_hs,
         fit_extDem_modRep_hs  = demPct2Party + changeY_extDem_modRep_hs)%>% 
  # Dem extreme  & moderate Republican
  dplyr::select(stateDist, b_demPct2Party= demPct2Party, contains(c("fit_","changeY")))

# 435 districts: mdiAll %>% filter(senate==0 & stateDist != "US-00") %>% dplyr::select(stateDist) %>% unique()
house_dist  = house_dist %>% 
  left_join(bumpCalc, by = "stateDist") %>%
  mutate(demWin_modDem_actRep = case_when(is.na(fit_modDem_actRep)==1 ~ demWin,
                                          TRUE ~ 1*(fit_modDem_actRep>50)),
         demWin_modDem_extRep = case_when(is.na(fit_modDem_extRep)==1 ~ demWin,
                                          TRUE ~ 1*(fit_modDem_extRep>50)),
         demWin_actDem_modRep = case_when(is.na(fit_actDem_modRep)==1 ~ demWin,
                                          TRUE ~ 1*(fit_actDem_modRep>50)),
         demWin_extDem_modRep = case_when(is.na(fit_extDem_modRep)==1 ~ demWin,
                                          TRUE ~ 1*(fit_extDem_modRep>50)),
         demWin_modDem_actRep_hs = case_when(is.na(fit_modDem_actRep_hs)==1 ~ demWin,
                                          TRUE ~ 1*(fit_modDem_actRep_hs>50)),
         demWin_modDem_extRep_hs = case_when(is.na(fit_modDem_extRep_hs)==1 ~ demWin,
                                          TRUE ~ 1*(fit_modDem_extRep_hs>50)),
         demWin_actDem_modRep_hs = case_when(is.na(fit_actDem_modRep_hs)==1 ~ demWin,
                                          TRUE ~ 1*(fit_actDem_modRep_hs>50)),
         demWin_extDem_modRep_hs = case_when(is.na(fit_extDem_modRep_hs)==1 ~ demWin,
                                          TRUE ~ 1*(fit_extDem_modRep_hs>50)))

return = list("demWin_count" = sum(house_dist$demWin), 
              "demWin_count_modDem_actRep" = sum(house_dist$demWin_modDem_actRep),
              "demWin_count_modDem_extRep" = sum(house_dist$demWin_modDem_extRep),
              "demWin_count_actDem_modRep" = sum(house_dist$demWin_actDem_modRep),
              "demWin_count_extDem_modRep" = sum(house_dist$demWin_extDem_modRep),
              "demWin_count_modDem_actRep_hs" = sum(house_dist$demWin_modDem_actRep_hs),
              "demWin_count_modDem_extRep_hs" = sum(house_dist$demWin_modDem_extRep_hs),
              "demWin_count_actDem_modRep_hs" = sum(house_dist$demWin_actDem_modRep_hs),
              "demWin_count_extDem_modRep_hs" = sum(house_dist$demWin_extDem_modRep_hs))
}              
