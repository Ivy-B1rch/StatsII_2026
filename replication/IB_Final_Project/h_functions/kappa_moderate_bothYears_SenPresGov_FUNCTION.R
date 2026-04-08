  # geography: county, state
  # compare: sp [Sen v Pres], sg [Sen v Gov], pg [Pres v Gov]

## QUESTIONS ##
# OK - exclude a Senate race (Markwayne Mullin)?
# Look at merge of county data for "sg" --> looks like many to one

## WORK
geography = "State" # County State
compare = "sg" #"sp"  "sg" "pg"
pathExtra = "NA" #../" #"NA" "../"
measure= "avgText" #"avgText" "cf", dwdime"
weights = 1
## END

kappa_moderate_bothYears_SenPresGov_FUNCTION = function(geography = "County", 
                                               weights = 1, # 0 no weights or 1 weights
                                               compare = "sp", #"sp"  "sg" "pg"
                                               measure = "avgText", # #"cf" "dwdime"
                                               pathExtra = "NA"){
  # Orig: path = "../4_results/"
  path = "Data/"
  if(pathExtra !="NA") path = paste0(pathExtra, path)
  
  if(measure == "avgText"){
    ideal = read.csv(paste0(path, "IdealPointsAll2020_labelFish.csv"))
    dem_pres = ideal %>% filter(candStem == "INFL-JoeBiden") %>%
      dplyr::select("avgIdeal") %>% unlist()
    rep_pres = ideal %>% filter(candStem == "INFL-realDonaldTrump") %>%
      dplyr::select("avgIdeal") %>% unlist()}
  if(measure == "cf"){
    #SOURCE: dime data
    #cands %>% filter(cycle == YEAR & grepl("trump, donald j", name))%>% 
    #  dplyr::select(name, party, recipient.cfscore, recipient.cfscore.dyn, dwdime)
    #cands %>% filter(cycle == YEAR & grepl("biden, joseph r jr", name)) %>% 
    #  dplyr::select(name, recipient.cfscore, recipient.cfscore.dyn, dwdime)
    dem_pres = -1.044
    rep_pres = 1.57 }
  if(measure == "dwdime"){
    dem_pres = -0.916
    # Trump dwdime = NA but use cf based on model showing that cf scores essentially perfectly predict dwdime
    rep_pres = 1.57 }
  kappaPres = (dem_pres + rep_pres)/2
  
  # Orig: pathData = "../3_data/"
  pathData = "Data/"
  if(pathExtra !="NA") pathData = paste0(pathExtra, pathData)
  
  if(compare == "sp"){
    sp_2020 <- read_csv(paste0(pathData, "sen_pres_2020.csv"), show_col_types = FALSE) %>% mutate(year = 2020)
    sp_2022 <- read_csv(paste0(pathData, "sen_pres_2022.csv"), show_col_types = FALSE) %>% mutate(year = 2022)
    sp_dta <- rbind(sp_2020, sp_2022) %>%
      mutate('2020' = if_else(year == 2020, 1, 0)) %>%
      mutate(dem_sen_inc = if_else(dem_sen_inc == "Incumbent", 1, 0),
             rep_sen_inc = if_else(rep_sen_inc == "Incumbent", 1, 0)) 

    if(measure == "cf" | measure == "dwdime"){
    mgMCI_2020 = read.csv(file = "Data/mergeBonica_2020.csv")  #"../3_data/Raw_d_Bonica_DIME/mergeBonica_2020.csv") 
    mgMCI_2022 = read.csv(file = "Data/mergeBonica_2022.csv") %>%
      filter(stateDist != "AK-Sen03") # Cut Alaska Senate in 2022 b/c final stage ("RCV runoff") was between 2 Republicans
    mgMCI <- rbind(mgMCI_2020, mgMCI_2022) 
    
    # For Dem values of Bonica ideology estimates: limit to generalElec and party 
    mgMCI_dem = mgMCI %>% filter(generalElec == 1 & party == "D") %>% 
      dplyr::select(stateDist, year,
                    dem_mg_avgIdeal= avgIdeal, 
                    dem_recipient.cfscore = recipient.cfscore, 
                    dem_dwdime = dwdime, 
                    dem_recipient.cfscore.dyn = recipient.cfscore.dyn)
    # For Rep values of Bonica ideology estimates: limit to generalElec and party 
    mgMCI_rep = mgMCI %>% filter(generalElec == 1 & party == "R") %>% 
      dplyr::select(stateDist, year,
                    rep_mg_avgIdeal = avgIdeal, 
                    rep_recipient.cfscore = recipient.cfscore, 
                    rep_dwdime = dwdime, 
                    rep_recipient.cfscore.dyn = recipient.cfscore.dyn)
    
    sp_dta = sp_dta %>%
      left_join(mgMCI_dem %>% 
                  rename("senState"= "stateDist") %>% 
                  dplyr::select(senState, 
                                dem_sen.avgIdeal2 = dem_mg_avgIdeal, 
                                dem_sen.cf        = dem_recipient.cfscore, 
                                dem_sen.dwdime    = dem_dwdime, 
                                dem_sen.cf.dyn    = dem_recipient.cfscore.dyn),
                by = "senState", "year") %>%
      left_join(mgMCI_rep %>% 
                  rename("senState"= "stateDist") %>% 
                  dplyr::select(senState, 
                                rep_sen.avgIdeal2 = rep_mg_avgIdeal, 
                                rep_sen.cf        = rep_recipient.cfscore, 
                                rep_sen.dwdime    = rep_dwdime, 
                                rep_sen.cf.dyn    = rep_recipient.cfscore.dyn),
                by = "senState", "year") %>%
      filter(dem_sen_percent > 0, 
             rep_sen_percent > 0, 
             dem_pres_percent > 0, 
             rep_pres_percent > 0,
             state != "Louisiana",
             state != "Alaska")
    }
    
    if(measure == "avgText") sp_dta = sp_dta %>% mutate(kappaSen = (dem_sen_avgIdeal + rep_sen_avgIdeal )/ 2)
    if(measure == "cf")      sp_dta = sp_dta %>% mutate(kappaSen = (dem_sen.cf       + rep_sen.cf )/ 2)
    if(measure == "dwdime")  sp_dta = sp_dta %>% mutate(kappaSen = (dem_sen.dwdime   + rep_sen.dwdime )/ 2)
    
    # Create dta object (use same signifier for all comparison options)
    dta = sp_dta %>%
      mutate(dem_pres,
             rep_pres,
             cutpoint_diff = kappaSen- kappaPres,
             dem_diff      = dem_sen_percent - dem_pres_percent) 
    
    #ivy birchall additional text
    dta <- dta %>%
      filter(
        abs(dem_sen_avgIdeal) < 1.2 |
          abs(rep_sen_avgIdeal) < 1.2
      )
    
    kappa.inc_covar <- as.formula("dem_diff*100 ~ cutpoint_diff + dem_sen_inc + rep_sen_inc + `2020`")
    kappa.all_covar <- as.formula("dem_diff*100 ~ cutpoint_diff + dem_sen_inc + rep_sen_inc + `2020` +
                                  `Ethnicities.Black Alone`  + `Ethnicities.Hispanic or Latino` +
                                  `Income.Median Houseold Income`+ `Education.Bachelor's Degree or Higher`")
    
    if(geography=="State"){
      dta  = dta  %>%
        filter(state != "Louisiana",
               state != "Alaska") %>%
        group_by(senState, year) %>%   
        summarize(`2020` = mean(`2020`, na.rm = TRUE),
                  state           =  first(state), 
                  dem_pres        =  first(dem_pres),
                  rep_pres        =  first(rep_pres),
                  sen_cand_dem    =  first(sen_cand_dem), 
                  sen_cand_rep    =  first(sen_cand_rep), 
                  county_votes       = sum(county_votes, na.rm = TRUE),
                  state_blackPct     = mean(state_blackPct),
                  state_hispPct      = mean(state_hispPct),
                  state_medianIncome = mean(state_medianIncome),
                  state_baPct        = mean(state_baPct),
                  dem_sen_votes      = sum(dem_sen_votes, na.rm = TRUE),
                  rep_sen_votes      = sum(rep_sen_votes, na.rm = TRUE),
                  sen_votes          = dem_sen_votes + rep_sen_votes,
                  dem_sen_inc        = mean(dem_sen_inc, na.rm = TRUE),
                  rep_sen_inc        = mean(rep_sen_inc, na.rm = TRUE),
                  dem_sen_percent    = dem_sen_votes / sen_votes,
                  dem_pres_votes     = sum(dem_pres_votes, na.rm = TRUE),
                  rep_pres_votes     = sum(rep_pres_votes, na.rm = TRUE),
                  pres_votes         = dem_pres_votes + rep_pres_votes,
                  dem_pres_percent   = dem_pres_votes/pres_votes,
                  dem_pres_percent   = dem_pres_votes/pres_votes,
                  sen_dem_receipt    = mean(Total_Receipt_Democrat),
                  sen_rep_receipt    = mean(Total_Receipt_Republican),
                  dem_sen_avgIdeal   = mean(dem_sen_avgIdeal, na.rm=T),
                  rep_sen_avgIdeal   = mean(rep_sen_avgIdeal, na.rm=T),
                  cutpoint_diff      = mean(cutpoint_diff),
                  #((dem_sen_avgIdeal  + rep_sen_avgIdeal )/ 2) -((dem_pres_avgIdeal + rep_pres_avgIdeal)/ 2),
                  dem_diff           =  dem_sen_percent - dem_pres_percent)
      
      kappa.all_covar <- as.formula("dem_diff*100 ~ cutpoint_diff + dem_sen_inc + rep_sen_inc + `2020` + state_blackPct  + state_hispPct + state_medianIncome + state_baPct")
    }  }

  if(compare == "sg"){
    # Use gov-pres data to get governor results by county by year and state level data
    gp_2020 <- read_csv(paste0(pathData,"gov_pres_2020.csv"), show_col_types = FALSE) %>% mutate(year = 2020)
    gp_2022 <- read_csv(paste0(pathData,"gov_pres_2022.csv"), show_col_types = FALSE) %>% mutate(year = 2022)
    gp <- rbind(gp_2020, gp_2022) %>%
      mutate('2020' = if_else(year == 2020, 1, 0),
             Utah   = if_else(state == "Utah", 1, 0))

    # Use sen-pres data to get senate results by county by year
    sp_2020 <- read_csv(paste0(pathData,"sen_pres_2020.csv"), show_col_types = FALSE) %>% mutate(year = 2020)
    sp_2022 <- read_csv(paste0(pathData,"sen_pres_2022.csv"), show_col_types = FALSE) %>% mutate(year = 2022)
    sp <- rbind(sp_2020, sp_2022) %>% #    read_csv("../3_data/sen_pres_combo.csv", show_col_types = FALSE) %>%
      filter(sen_cand_rep != "Markwayne Mullin",
             sen_cand_dem != "Kendra Horn",
             state != "Oklahoma") %>%
      dplyr::select(!contains("state_")) %>%
      dplyr::select(state, "sen_county_name" = county_name, year, 
                    sen_cand_dem, sen_cand_rep,
                    dem_sen_avgIdeal, dem_sen_percent, dem_sen_votes, dem_sen_inc, 
                    rep_sen_avgIdeal, rep_sen_percent, rep_sen_votes, rep_sen_inc, 
                    "county_votes" = county_votes, "sen_county_name" = county_name)
                    #,sen_dem_receipt, sen_rep_receipt
  
    dta <- sp %>%
      # Pull in state level covariates plus
      left_join(gp,     
                by = c("state" = "state", "sen_county_name" = "county_name", "year" = "year")) 
    dta <- dta %>%
      filter(state != "Louisiana",
             state != "Alaska") %>%
      mutate(dem_sen_inc = if_else(dem_sen_inc == "Incumbent", 1, 0),
             rep_sen_inc = if_else(rep_sen_inc == "Incumbent", 1, 0),
             dem_gov_inc = if_else(dem_gov_inc == "Incumbent", 1, 0),
             rep_gov_inc = if_else(rep_gov_inc == "Incumbent", 1, 0)) %>%
      mutate(cutpoint_diff = ((dem_sen_avgIdeal + rep_sen_avgIdeal )/ 2) - 
                             ((dem_gov_avgIdeal + rep_gov_avgIdeal)/ 2),
             dem_diff   =   dem_sen_percent  - dem_gov_percent) 
    kappa.inc_covar <- as.formula("dem_diff*100 ~ cutpoint_diff + dem_sen_inc + rep_sen_inc + dem_gov_inc + rep_gov_inc +`2020`")
    kappa.all_covar <- as.formula("dem_diff*100 ~ cutpoint_diff + dem_sen_inc + rep_sen_inc + dem_gov_inc + rep_gov_inc + `2020` + 
                    rep_pres_percent + 
                    `Ethnicities.Black Alone` + `Ethnicities.Hispanic or Latino` + 
                    `Income.Median Houseold Income` + `Education.Bachelor's Degree or Higher`")
    
    if(geography=="State"){
      dta  = dta  %>%
        filter(state != "Louisiana",
               state != "Alaska") %>% 
        group_by(state, year) %>%
        summarize(`2020` = mean(`2020`, na.rm = TRUE),
                  dem_pres        =  first(dem_pres),
                  rep_pres        =  first(rep_pres),
                  sen_cand_dem    =  first(sen_cand_dem), 
                  sen_cand_rep    =  first(sen_cand_rep), 
                  gov_cand_dem    =  first(gov_cand_dem), 
                  gov_cand_rep    =  first(gov_cand_rep), 
                  rep_pres_percent   = mean(state_trumppct2020),
                  state_blackPct     = mean(state_blackPct),
                  state_hispPct      = mean(state_hispPct),
                  state_medianIncome = mean(state_medianIncome),
                  state_baPct        = mean(state_baPct),
                  county_votes       = sum(county_votes, na.rm = TRUE),
                  dem_sen_votes      = sum(dem_sen_votes, na.rm = TRUE),
                  rep_sen_votes      = sum(rep_sen_votes, na.rm = TRUE),
                  sen_votes          = dem_sen_votes + rep_sen_votes,
                  dem_sen_avgIdeal = mean(dem_sen_avgIdeal, na.rm = TRUE),
                  rep_sen_avgIdeal = mean(rep_sen_avgIdeal, na.rm = TRUE),
                  dem_sen_inc      = mean(dem_sen_inc, na.rm = TRUE),
                  rep_sen_inc      = mean(rep_sen_inc, na.rm = TRUE),
                  dem_sen_percent  = dem_sen_votes / sen_votes,
                  dem_gov_votes = sum(dem_gov_votes, na.rm = TRUE),
                  rep_gov_votes = sum(rep_gov_votes, na.rm = TRUE),
                  gov_votes       = dem_gov_votes + rep_gov_votes,
                  dem_gov_percent = dem_gov_votes/gov_votes,
                  #sen_dem_receipt    = mean(Total_Receipt_Democrat),
                  #sen_rep_receipt    = mean(Total_Receipt_Republican),
                  dem_gov_avgIdeal = mean(dem_gov_avgIdeal, na.rm = TRUE),
                  rep_gov_avgIdeal = mean(rep_gov_avgIdeal, na.rm = TRUE),
                  dem_gov_percent = dem_gov_votes/gov_votes,
                  dem_gov_inc      = mean(dem_gov_inc, na.rm = TRUE),
                  rep_gov_inc      = mean(rep_gov_inc, na.rm = TRUE),
                  cutpoint_diff = ((dem_sen_avgIdeal  + rep_sen_avgIdeal )/ 2) -
                    ((dem_gov_avgIdeal + rep_gov_avgIdeal)/ 2),
                  dem_diff   =   dem_sen_percent - dem_gov_percent) 
      kappa.all_covar <- as.formula("dem_diff*100 ~ cutpoint_diff + dem_sen_inc + rep_sen_inc + dem_gov_inc + rep_gov_inc + `2020` + 
                                    rep_pres_percent + state_blackPct  + state_hispPct + state_medianIncome + state_baPct")
    }  }

  if(compare == "pg"){
    gp_2020 <- read_csv(paste0(pathData,"gov_pres_2020.csv"), show_col_types = FALSE) %>% mutate(year = 2020)
    gp_2022 <- read_csv(paste0(pathData,"gov_pres_2022.csv"), show_col_types = FALSE) %>% mutate(year = 2022)
    dta <- rbind(gp_2020, gp_2022) %>%
      filter(state != "Louisiana",
             state != "Alaska") %>%
      mutate('2020' = if_else(year == 2020, 1, 0),
             Utah = if_else(state == "Utah", 1, 0)) %>%
      mutate(dem_gov_inc  = if_else(dem_gov_inc == "Incumbent", 1, 0),
             rep_gov_inc  = if_else(rep_gov_inc == "Incumbent", 1, 0),
             county_votes = dem_gov_votes+ rep_gov_votes) %>%
      mutate(cutpoint_diff = ((dem_pres_avgIdeal  + rep_pres_avgIdeal )/ 2) - 
                             ((dem_gov_avgIdeal + rep_gov_avgIdeal)/ 2),
             dem_diff   =   dem_pres_percent - dem_gov_percent) 
    kappa.inc_covar <- as.formula("dem_diff*100 ~ cutpoint_diff + dem_gov_inc + rep_gov_inc +`2020`")
    kappa.all_covar <- as.formula("dem_diff*100 ~ cutpoint_diff + dem_gov_inc + rep_gov_inc + `2020` + 
                    `Ethnicities.Black Alone` + `Ethnicities.Hispanic or Latino` + 
                    `Income.Median Houseold Income` +`Education.Bachelor's Degree or Higher`")
    if(geography=="State"){
      dta  = dta  %>%
        filter(state != "Louisiana",
               state != "Alaska") %>% 
        group_by(state, year) %>%
        summarize(`2020` = mean(`2020`, na.rm = TRUE),
                  dem_pres        =  first(dem_pres),
                  rep_pres        =  first(rep_pres),
                  gov_cand_dem    =  first(gov_cand_dem), 
                  gov_cand_rep    =  first(gov_cand_rep), 
                  state_blackPct     = mean(state_blackPct),
                  state_hispPct      = mean(state_hispPct),
                  state_medianIncome = mean(state_medianIncome),
                  state_baPct        = mean(state_baPct),
                  dem_pres_votes     = sum(dem_pres_votes, na.rm = TRUE),
                  rep_pres_votes     = sum(rep_pres_votes, na.rm = TRUE),
                  pres_votes         = dem_pres_votes + rep_pres_votes,
                  county_votes       = sum(county_votes),
                  dem_pres_avgIdeal = mean(dem_pres_avgIdeal, na.rm = TRUE),
                  rep_pres_avgIdeal = mean(rep_pres_avgIdeal, na.rm = TRUE),
                  dem_pres_percent  = dem_pres_votes / pres_votes,
                  dem_gov_votes = sum(dem_gov_votes, na.rm = TRUE),
                  rep_gov_votes = sum(rep_gov_votes, na.rm = TRUE),
                  gov_votes       = dem_gov_votes + rep_gov_votes,
                  dem_gov_percent = dem_gov_votes/gov_votes,
                  #sen_dem_receipt = mean(sen_dem_receipt), 
                  #sen_rep_receipt = mean(sen_rep_receipt),
                  dem_gov_avgIdeal = mean(dem_gov_avgIdeal, na.rm = TRUE),
                  rep_gov_avgIdeal = mean(rep_gov_avgIdeal, na.rm = TRUE),
                  dem_gov_percent = dem_gov_votes/gov_votes,
                  dem_gov_inc      = mean(dem_gov_inc, na.rm = TRUE),
                  rep_gov_inc      = mean(rep_gov_inc, na.rm = TRUE),
                  cutpoint_diff = ((dem_pres_avgIdeal  + rep_pres_avgIdeal )/ 2) -
                    ((dem_gov_avgIdeal + rep_gov_avgIdeal)/ 2),
                  dem_diff   =   dem_pres_percent - dem_gov_percent) 
      kappa.all_covar <- as.formula("dem_diff*100 ~ cutpoint_diff + dem_gov_inc + rep_gov_inc + `2020` + state_blackPct  + state_hispPct + state_medianIncome + state_baPct")
    }  }

# CHECK results w/o weights or Vermont  
# lm(kappa.all_covar, data = sp_dta) %>% summary()
#   sp_dta  %>% fixest::feols(kappa.all_covar, data = ., weights = ~ county_votes)   %>% summary()
#   dtaX = sp_dta %>% filter(state!="Vermont")
#   dtaX = sp_dta %>% filter(state!="Utah")
#   dtaX %>% fixest::feols(kappa.all_covar, data = ., weights = ~ county_votes)   %>% summary()

  
## Models
  # Put median income on scale of $100,000
  if(geography=="County") dta$'Income.Median Houseold Income' = dta$'Income.Median Houseold Income'/100000
  if(geography=="State")  dta$state_medianIncome              = dta$state_medianIncome/100000
  kappa.no_covar  <- as.formula("dem_diff*100 ~ cutpoint_diff")
 
  if(geography=="County"){
    if(weights == 1) {
      both_no_covar <- dta %>%
        fixest::feols(kappa.no_covar, data = ., cluster ~ state, weights = ~ county_votes)
      both_inc_covar <- dta %>%
        fixest::feols(kappa.inc_covar, data = ., cluster ~ state, weights = ~ county_votes)
      both_all_covar <- dta %>%
        fixest::feols(kappa.all_covar, data = ., cluster ~ state, weights = ~ county_votes) }
    if(weights == 0) {
      both_no_covar  <- dta %>% fixest::feols(kappa.no_covar,  data = ., cluster ~ state)
      both_inc_covar <- dta %>% fixest::feols(kappa.inc_covar, data = ., cluster ~ state)
      both_all_covar <- dta %>% fixest::feols(kappa.all_covar, data = ., cluster ~ state)  }
    }

  if(geography=="State"){
    if(weights == 1) {
      both_no_covar  <- dta %>% fixest::feols(kappa.no_covar,  data = ., weights = ~ county_votes)
      both_inc_covar <- dta %>% fixest::feols(kappa.inc_covar, data = ., weights = ~ county_votes)
      both_all_covar <- dta %>% fixest::feols(kappa.all_covar, data = ., weights = ~ county_votes)  }
    if(weights == 0) {
      both_no_covar  <- dta %>% fixest::feols(kappa.no_covar,  data = .)
      both_inc_covar <- dta %>% fixest::feols(kappa.inc_covar, data = .)
      both_all_covar <- dta %>% fixest::feols(kappa.all_covar, data = .)  }
    }
  
  # summary(both_no_covar )
  # summary(both_inc_covar)
  # summary(both_all_covar)
  c(summary(both_no_covar )$nobs, summary(both_inc_covar )$nobs, summary(both_all_covar )$nobs)

beta_no_covar  = summary(both_no_covar)$ coeftable["cutpoint_diff", 1]
  se_no_covar  = summary(both_no_covar)$ coeftable["cutpoint_diff", 2]
beta_inc_covar = summary(both_inc_covar)$coeftable["cutpoint_diff", 1]
  se_inc_covar = summary(both_inc_covar)$coeftable["cutpoint_diff", 2]
beta_all_covar = summary(both_all_covar)$coeftable["cutpoint_diff", 1]
  se_all_covar = summary(both_all_covar)$coeftable["cutpoint_diff", 2]

  # CHECK results w/o weights 
  #   sp_dta  %>% fixest::feols(kappa.all_covar, data = .)   %>% summary()
  
  
return = list("beta_no_covar"  = beta_no_covar, 
              "se_no_covar"    =   se_no_covar,
              "res_no_covar"   = both_no_covar,
              "beta_inc_covar" = beta_inc_covar, 
              "se_inc_covar"   =   se_inc_covar,
              "res_inc_covar"  = both_inc_covar,
              "beta_all_covar" = beta_all_covar, 
              "se_all_covar"   =   se_all_covar,
              "res_all_covar"  = both_all_covar,
              "dta" = dta,
              "nobs" = c(summary(both_no_covar )$nobs, summary(both_inc_covar )$nobs, summary(both_all_covar )$nobs)  )
}