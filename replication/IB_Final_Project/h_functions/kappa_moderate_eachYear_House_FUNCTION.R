## WORK

 # measure = "avgText"
 # compare = "Pres" # "Pres" "Sen"
 #  year = 2020
 #  pathExtra = "NA" #"../"

## END

kappa_moderate_eachYear_House_FUNCTION = function(year, 
                                         compare = "Pres", # "Pres" or "Sen"
                                         measure = "avgText",
                                         pathExtra="NA",
                                         clusterCode = 0){

## Constant changes if we include kappaPresident (do this to keep things similar across all models even tho no change to beta)
  # Orig: path = "../4_results/"
  path = "Data/"
if(pathExtra !="NA") path = paste0(pathExtra,path)

  ideal = read.csv(paste0(path, "IdealPointsAll", year, "_labelFish.csv"))
  #ideal = read.csv(paste0(path, "IdealPointsAll2020_labelFish.csv"))
  if(measure == "avgText"){
    # dem_pres = ideal %>% filter(candStem == "INFL-JoeBiden") %>%
    #   dplyr::select("avgIdeal") %>% unlist()
    # rep_pres = ideal %>% filter(candStem == "INFL-realDonaldTrump") %>%
    #   dplyr::select("avgIdeal") %>% unlist()
    dem_pres = -1.42
    rep_pres = 1.62 }
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

  # Compare Democratic House outcomes to "Pres" OR "Senate" by year
  # For compare to Senate: only available for 2020
  saveName.tmp = paste0(year, "_labelFish")

  # Orig: pathData = "../3_data/"
  pathData = "Data/"
  if(pathExtra !="NA") pathData = paste0(pathExtra, pathData)

mdiAll <- read_csv(paste0(path, "generalElecResults_withIdeal_", saveName.tmp, ".csv"), 
                   show_col_types = FALSE)%>%
  filter(!duplicated(stateDist)) %>% #Eliminate rows with repeated house - districts (will fix in .csv) 
  mutate(senate = (district ==99),
         dem_pres,
         rep_pres,
         probCon_dem = pnorm(dem_avgIdeal),
         probCon_rep = pnorm(rep_avgIdeal),
         UtahDum = 1*(state=="Utah"),
         medianIncome = medianIncome/10, # Data is in 10,000s; want in 100,000 
         demWin = 1*(demPct2Party >50)) %>%
  mutate(trump2016Pct2Party = trump2016Pct/( trump2016Pct+ clinton2016Pct), 
         house_dem_receipt  = dem_receipt,
         house_rep_receipt  = rep_receipt,
         log_house_dem_receipt  = case_when(house_dem_receipt >0 ~ log(house_dem_receipt),
                                            TRUE~ NA),
         log_house_rep_receipt  = case_when(house_rep_receipt >0 ~ log(house_rep_receipt),
                                            TRUE~ NA),
         house_dem_inc      = dem_inc,
         house_rep_inc      = rep_inc,
         house_demMoneyAdv     = (dem_receipt)/(dem_receipt + rep_receipt),
         log_house_demMoneyAdv = log((dem_receipt)/(dem_receipt + rep_receipt)),
         trump2020Pct2Party = trump2020Pct/( trump2020Pct+ biden2020Pct), 
         dem_followersCampaign = replace_na(dem_followersCampaign, 0),
         rep_followersCampaign = replace_na(rep_followersCampaign, 0),
         dem_followersOfficial = replace_na(dem_followersOfficial, 0),
         rep_followersOfficial = replace_na(rep_followersOfficial, 0),
         dem_followers = dem_followersCampaign+ dem_followersOfficial,
         rep_followers = rep_followersCampaign+ rep_followersOfficial,
         moneyOutlier = (dem_Individual_Unitemized_Contribution > 8|
                           rep_Individual_Unitemized_Contribution > 8))

# Create subsets of data - see QA_Summary.Rmd that details which observations lost
mdiHouseAll    <- mdiAll%>% 
  filter(district !=99) %>%
  filter(is.na(rep_avgIdeal)==0 & is.na(dem_avgIdeal)==0) %>%
  mutate(rep_followers = case_when(is.na(rep_followersOfficial) ~ rep_followersCampaign,
                                   (is.na(rep_followersOfficial)==0) ~ rep_followersCampaign + rep_followersOfficial),
         dem_followers = case_when(is.na(dem_followersOfficial) ~ dem_followersCampaign,
                                   (is.na(dem_followersOfficial)==0) ~ dem_followersCampaign + dem_followersOfficial))

if(measure == "avgText"){
  #IVY BIRCHALL INSERTED TEXT
  mdiHouseAll = mdiHouseAll %>%
    filter(
      dem_avgIdeal > -1.2 & dem_avgIdeal < 1.2 |
        rep_avgIdeal > -1.2 & rep_avgIdeal < 1.2
    )
  mdiHouseAll = mdiHouseAll %>% mutate(kappaHouse = 0.5*(dem_avgIdeal + rep_avgIdeal))}

# Load Bonica data/measures
  # Orig: "../3_data/Raw_d_Bonica_DIME/mergeBonica_", year, ".csv")
  mgMCI = read.csv(file = paste0("Data/mergeBonica_", year, ".csv")) 
  
  # For Dem values of ideology estimates: limit to generalElec and party 
  mgMCI_dem = mgMCI %>% filter(generalElec == 1 & party == "D") %>% 
    dplyr::select(stateDist, 
                  dem_mg_avgIdeal= avgIdeal, 
                  dem_recipient.cfscore = recipient.cfscore, 
                  dem_dwdime = dwdime, 
                  dem_recipient.cfscore.dyn = recipient.cfscore.dyn)
  # For Rep values of ideology estimates: limit to generalElec and party 
  mgMCI_rep = mgMCI %>% filter(generalElec == 1 & party == "R") %>% 
    dplyr::select(stateDist, 
                  rep_mg_avgIdeal = avgIdeal, 
                  rep_recipient.cfscore = recipient.cfscore, 
                  rep_dwdime = dwdime, 
                  rep_recipient.cfscore.dyn = recipient.cfscore.dyn)

  mdiHouseAll= mdiHouseAll %>% 
    left_join(mgMCI_dem, by = "stateDist")  %>% 
    left_join(mgMCI_rep, by = "stateDist")
  if(measure == "cf")     mdiHouseAll = mdiHouseAll %>% mutate(kappaHouse = 0.5*(dem_recipient.cfscore + rep_recipient.cfscore))
  if(measure == "dwdime") mdiHouseAll = mdiHouseAll %>% mutate(kappaHouse = 0.5*(dem_dwdime + rep_dwdime))


mdiHouseAll = mdiHouseAll %>% 
  mutate(kappaDiff_HousePres = kappaHouse - kappaPres,
         # kappaDiff_HousePres_web = 0.5*(dem_webIdeal + rep_webIdeal)- kappaPres,
         # kappaDiff_HousePres_tw  = 0.5*( dem_twIdeal + rep_twIdeal )- kappaPres,
         demDiff_HousePres  = demPct2Party - 100*(1-trump2020Pct2Party))


if(compare == "Pres"){
  formula_no_covar   <- as.formula("demDiff_HousePres ~ kappaDiff_HousePres")
  formula_inc_covar  <- as.formula("demDiff_HousePres ~ kappaDiff_HousePres + dem_inc + rep_inc")
  formula_covar      <- as.formula("demDiff_HousePres ~ kappaDiff_HousePres + dem_inc + rep_inc + 
                                blackPct + hispPct  + medianIncome + baPct")
  formula_covar_money<- as.formula("demDiff_HousePres ~ kappaDiff_HousePres + dem_inc + rep_inc + 
                                blackPct + hispPct  + medianIncome + baPct + log_house_dem_receipt + log_house_rep_receipt")}

if(compare == "Sen"){
  formula_no_covar    <- as.formula("demDiff_HouseSen ~ kappaDiff_HouseSen")
  formula_inc_covar   <- as.formula("demDiff_HouseSen ~ kappaDiff_HouseSen + dem_house_inc + dem_sen_inc + rep_house_inc  + rep_sen_inc")
  formula_covar       <- as.formula("demDiff_HouseSen ~ kappaDiff_HouseSen + dem_house_inc + dem_sen_inc + rep_house_inc + rep_sen_inc + 
                                trump2016Pct2Party + blackPct + hispPct  + medianIncome + baPct")
  formula_covar_money <- as.formula("demDiff_HouseSen ~ kappaDiff_HouseSen + dem_house_inc + dem_sen_inc + rep_house_inc + rep_sen_inc + 
                                trump2016Pct2Party + blackPct + hispPct  + medianIncome + baPct +
                                log_house_dem_receipt + log_house_rep_receipt + 
                                log_sen_dem_receipt + log_sen_rep_receipt")

  
  # Orig: pathData not Data/
  house_sen_2020 <- read_csv(paste0("Data/", "house_sen_2020.csv"), 
                             show_col_types = FALSE) %>% 
    mutate("stateDist" = houseDist,
           "stateAbb" = substring(stateDist, 1, 2)) %>% relocate(state, stateAbb, stateDist) %>% 
    left_join(mgMCI_dem, by = "stateDist") %>% 
    left_join(mgMCI_rep, by = "stateDist") 

  # Create Senate only scores: 34 rows wth Senate candidates cf, dwdime
  dem_sen = mgMCI_dem %>% 
    filter(grepl("Sen", stateDist)) %>%
    mutate(senState = stateDist,
           stateAbb = substring(stateDist, 1, 2),
           dem_sen.avgIdeal = dem_mg_avgIdeal,
           dem_sen.cf = dem_recipient.cfscore,
           dem_sen.dwdime = dem_dwdime) %>%
    dplyr::select(-c(stateDist, dem_mg_avgIdeal, dem_recipient.cfscore, dem_dwdime, dem_recipient.cfscore.dyn)) %>%
    relocate(stateAbb)
  
  rep_sen = mgMCI_rep %>% 
    filter(grepl("Sen", stateDist)) %>%
    mutate(senState = stateDist,
           stateAbb = substring(stateDist, 1, 2),
           rep_sen.avgIdeal = rep_mg_avgIdeal,
           rep_sen.cf       = rep_recipient.cfscore,
           rep_sen.dwdime   = rep_dwdime) %>%
    dplyr::select(-c(stateDist, rep_mg_avgIdeal, rep_recipient.cfscore, rep_dwdime, rep_recipient.cfscore.dyn)) %>%
    relocate(stateAbb)
  
  senOnlyMoney = mdiAll %>% 
    filter(grepl("Sen", stateDist)) %>%
    dplyr::select(stateDist, sen_demPct2Party = demPct2Party, 
                  sen_dem_receipt = dem_receipt, sen_rep_receipt = rep_receipt) %>%
    filter(sen_demPct2Party>0) %>%
    mutate(sen_demMoneyAdv = sen_dem_receipt/(sen_dem_receipt + sen_rep_receipt))

  house_sen_2020 = house_sen_2020 %>% 
    left_join(dem_sen, by = "senState") %>%
    left_join(rep_sen, by = "senState") %>% 
    left_join(mdiHouseAll %>% dplyr::select(stateDist, state, kappaDiff_HousePres, demDiff_HousePres, 
                                            demPct2Party, trump2016Pct, trump2020Pct, trump2016Pct2Party, trump2020Pct2Party,
                                            blackPct, hispPct, medianIncome, baPct, 
                                            house_dem_inc, house_rep_inc,
                                            house_dem_receipt, house_rep_receipt, log_house_dem_receipt, log_house_rep_receipt, house_demMoneyAdv), 
              by = c("houseDist" = "stateDist", "state" = "state")) %>%
    mutate(demDiff_HouseSen   = demPct2Party - 100*dem_sen_percent) %>% 
    left_join(senOnlyMoney, by = c("senState" = "stateDist")) %>%
    mutate(log_sen_dem_receipt  = log(sen_dem_receipt),
           log_sen_rep_receipt  = log(sen_rep_receipt))
  
  if(measure == "avgText"){
    mdiHouseAll <- house_sen_2020 %>% 
      mutate(kappaDiff_HouseSen = ((dem_house_avgIdeal + rep_house_avgIdeal) / 2) - 
               ((dem_sen_avgIdeal + rep_sen_avgIdeal) / 2)) }
  
  if(measure == "cf"){
    mdiHouseAll <- house_sen_2020 %>% 
      mutate(kappaDiff_HouseSen = ((dem_recipient.cfscore + rep_recipient.cfscore) / 2) - 
               ((dem_sen.cf + rep_sen.cf) / 2)) }
  
  
  if(measure == "dwdime"){
    mdiHouseAll <- house_sen_2020 %>% 
      mutate(kappaDiff_HouseSen = ((dem_dwdime + rep_dwdime) / 2) - 
               ((dem_sen.dwdime + rep_sen.dwdime) / 2)) }
  
  mdiHouseAll = mdiHouseAll %>% 
    mutate(dem_sen_inc = 1*(dem_sen_inc=="Incumbent"),
           rep_sen_inc = 1*(rep_sen_inc=="Incumbent"))
  } # END if(compare=="Sen")...

## WORK
#names(mdiAll)
#table(mdiHouseAll$dem_sen_inc)

## END

# Competitive districts only
mdiHouseCompet <- mdiHouseAll %>% filter(trump2020Pct > 40 & trump2020Pct < 57 & 
                                         (house_dem_receipt > 0.1|house_dem_inc==1) & (house_rep_receipt>0.1|house_rep_inc==1))
  # Non competitive districts: t=1.2 for HP2020; t = 1.8 for HP2022; HS;
  # mdiHouseNotCompet <- mdiHouseAll %>% filter(trump2016Pct < 41 | trump2016Pct > 59)

# Models
if(clusterCode == 0){
  all_no_covar    <- fixest::feols(formula_no_covar,    data = mdiHouseAll)
  all_inc_covar   <- fixest::feols(formula_inc_covar,   data = mdiHouseAll)
  all_covar       <- fixest::feols(formula_covar,       data = mdiHouseAll)
  all_covar_money <- fixest::feols(formula_covar_money, data = mdiHouseAll)
  compet_no_covar    <- fixest::feols(formula_no_covar,    data = mdiHouseCompet)
  compet_inc_covar   <- fixest::feols(formula_inc_covar,   data = mdiHouseCompet)
  compet_covar       <- fixest::feols(formula_covar,       data = mdiHouseCompet)
  hispend_no_covar      <- fixest::feols(formula_no_covar,       data = mdiHouseAll %>% 
                                        filter(house_dem_receipt>1 & house_rep_receipt>1))
  hispend_covar      <- fixest::feols(formula_covar,       data = mdiHouseAll %>% 
                                        filter(house_dem_receipt>1 & house_rep_receipt>1))
  compet_covar_money <- fixest::feols(formula_covar_money, data = mdiHouseCompet)
  } # END if(clusterCode == 0){


if(clusterCode == 1){
  all_no_covar    <- fixest::feols(formula_no_covar,    data = mdiHouseAll, cluster ~ state)
  all_inc_covar   <- fixest::feols(formula_inc_covar,   data = mdiHouseAll, cluster ~ state)
  all_covar       <- fixest::feols(formula_covar,       data = mdiHouseAll, cluster ~ state)
  all_covar_money <- fixest::feols(formula_covar_money, data = mdiHouseAll, cluster ~ state)
  compet_no_covar    <- fixest::feols(formula_no_covar,    data = mdiHouseCompet, cluster ~ state)
  compet_inc_covar   <- fixest::feols(formula_inc_covar,   data = mdiHouseCompet, cluster ~ state)
  compet_covar       <- fixest::feols(formula_covar,       data = mdiHouseCompet, cluster ~ state)
  hispend_no_covar      <- fixest::feols(formula_no_covar,       data = mdiHouseAll %>% 
                                        filter(house_dem_receipt>1 & house_rep_receipt>1),
                                      cluster ~ state)
  hispend_covar      <- fixest::feols(formula_covar,       data = mdiHouseAll %>% 
                                      filter(house_dem_receipt>1 & house_rep_receipt>1),
                                      cluster ~ state)
  compet_covar_money <- fixest::feols(formula_covar_money, data = mdiHouseCompet, cluster ~ state)
  } # END if(clusterCode == 1){


if(compare == "Pres") cutpointName = "kappaDiff_HousePres"
if(compare == "Sen")  cutpointName = "kappaDiff_HouseSen"

beta_all    = summary(all_covar)$   coeftable[cutpointName, 1]
se_all      = summary(all_covar)$   coeftable[cutpointName,2]
beta_compet = summary(compet_covar)$coeftable[cutpointName,1]
se_compet   = summary(compet_covar)$coeftable[cutpointName,2]
beta_hispend = summary(hispend_covar)$coeftable[cutpointName,1]
se_hispend   = summary(hispend_covar)$coeftable[cutpointName,2]



return = list("beta_all" = beta_all, 
              "se_all"   = se_all,
              "beta_compet" = beta_compet, 
              "se_compet"   = se_compet,
              "beta_hispend" = beta_hispend, 
              "se_hispend"   = se_hispend,
              "all_no_covar"    = all_no_covar,
              "all_inc_covar"   = all_inc_covar,
              "all_covar"       = all_covar,
              "all_covar_money" = all_covar_money,
              "compet_no_covar"    = compet_no_covar,
              "compet_inc_covar"   = compet_inc_covar,
              "compet_covar"       = compet_covar,
              "hispend_no_covar"       = hispend_no_covar,
              "hispend_covar"       = hispend_covar,
              "compet_covar_money" = compet_covar_money,
              "mdiHouseAll"    = mdiHouseAll,
              "mdiHouseCompet" = mdiHouseCompet)
}

