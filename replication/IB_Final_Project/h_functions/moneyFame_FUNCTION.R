## Draft Section 4 function
# See below for moneyFame_table_FUNCTION

# Party leaders 2020
  # dem_partyLeader: Pelosi, Hoyer, Clyburn, Bustos (IL)
  # rep_partyLeader: McCarthy, Scalise, Emmer

# Party leaders 2022
  # dem_partyLeader: Jeffries, Clark, Sean Patrick Maloney (NY) [Pelosi, Hoyer, Clyburn]
  # rep_partyLeader: McCarthy, Scalise, Emmer [Mike Johnson not selected until Oct 2023]

## WORK
year = 2022
depVar = "small_donations" # "total_donations" "small_donations" "followers"
pathExtra = "../"
## END WORK

moneyFame_FUNCTION = function(year, depVar, pathExtra = "NA"){

saveName.tmp = paste0(year) 
labelFish = 1
if(labelFish == 1) saveName.tmp = paste0(saveName.tmp, "_labelFish")

path = "../4_results/"
if(pathExtra !="NA") path = paste0(pathExtra, path)

mdiAll <- read_csv(paste0(path, "generalElecResults_withIdeal_", saveName.tmp, ".csv"), 
                   show_col_types = FALSE)%>%
  mutate(senate = (district ==99),
         probCon_dem = pnorm(dem_avgIdeal),
         probCon_rep = pnorm(rep_avgIdeal),
         UtahDum = (state=="Utah"),
         medianIncome = medianIncome/100000) %>% 
  mutate(trump2016Pct2Party = trump2016Pct/( trump2016Pct+ clinton2016Pct), 
         trump2020Pct2Party = trump2020Pct/( trump2020Pct+ biden2020Pct), 
         dem_followersCampaign = replace_na(dem_followersCampaign, 0),
         rep_followersCampaign = replace_na(rep_followersCampaign, 0),
         dem_followersOfficial = replace_na(dem_followersOfficial, 0),
         rep_followersOfficial = replace_na(rep_followersOfficial, 0),
         dem_followers = dem_followersCampaign+ dem_followersOfficial,
         rep_followers = rep_followersCampaign+ rep_followersOfficial,
         moneyOutlier = (dem_Individual_Unitemized_Contribution > 8|
                           rep_Individual_Unitemized_Contribution > 8))

# Add incivility
pathData = "../3_data/"
if(pathExtra !="NA") pathData = paste0(pathExtra, pathData)

incivil <- read_csv(paste0(pathData, "Raw_f_Civility/civilityIndex", year, ".csv"), 
                    show_col_types = FALSE) %>%
  rename(candStem=candidate)

mdiAll  = mdiAll %>%
  left_join(incivil, by = c("rep_candStem" = "candStem")) %>%
  rename(rep_totUncivil  = totalUncivilTweets,
         rep_pctUncivil  = percentUncivilTweets) %>%
  left_join(incivil, by = c("dem_candStem" = "candStem")) %>%
  rename(dem_totUncivil  = totalUncivilTweets,
         dem_pctUncivil  = percentUncivilTweets) %>%
  mutate(dem_totUncivil = replace_na(dem_totUncivil, 0),
         dem_pctUncivil = replace_na(dem_pctUncivil, 0),
         rep_totUncivil = replace_na(rep_totUncivil, 0),
         rep_pctUncivil = replace_na(rep_pctUncivil, 0),
         dem_chal = 1 - dem_inc,
         rep_chal = 1 - rep_inc)

# Create subsets of data - see QA_Summary.Rmd that details which observations lost
mdiHouseAll    <- mdiAll%>% 
  filter(district !=99) %>%
  filter(is.na(rep_avgIdeal)==0 & is.na(dem_avgIdeal)==0) %>%
  mutate(rep_followers = case_when(is.na(rep_followersOfficial) ~ rep_followersCampaign,
                                   (is.na(rep_followersOfficial)==0) ~ rep_followersCampaign + rep_followersOfficial),
         dem_followers = case_when(is.na(dem_followersOfficial) ~ dem_followersCampaign,
                                   (is.na(dem_followersOfficial)==0) ~ dem_followersCampaign + dem_followersOfficial))

mdiHouseAll = mdiHouseAll %>% 
  mutate(kappa_House     = 0.5*(dem_avgIdeal + rep_avgIdeal),
         demDiff_HousePres  = demPct2Party - 100*(1-trump2020Pct2Party)) %>%
  filter(district !=99) %>%
  filter(is.na(rep_avgIdeal)==0 & is.na(dem_avgIdeal)==0) 

### Dependent variable
if(depVar == "total_donations") {
  mdiHouseAll$dv_dem = log(mdiHouseAll$dem_receipt + 0.001)
  mdiHouseAll$dv_rep = log(mdiHouseAll$rep_receipt + 0.001) }
if(depVar == "small_donations") {
  mdiHouseAll$dv_dem = log(mdiHouseAll$dem_Individual_Unitemized_Contribution + 0.001)
  mdiHouseAll$dv_rep = log(mdiHouseAll$rep_Individual_Unitemized_Contribution + 0.001) }
if(depVar == "followers") {
  mdiHouseAll$dv_dem = log(mdiHouseAll$dem_followers+ 0.01)
  mdiHouseAll$dv_rep = log(mdiHouseAll$rep_followers+ 0.01) }

mdiHouseContest = mdiAll %>% 
  filter(demPct2Party > 0 & demPct2Party < 100) %>% 
  filter(district !=99) %>%
  filter(is.na(rep_avgIdeal)==0 & is.na(dem_avgIdeal)==0) %>%
  filter(dem_partyLeader==0 & dem_partyLeader==0) 

# Competitive districts only
mdiHouseCompet <- mdiHouseAll %>% 
  filter(trump2020Pct > 42.5 & trump2020Pct < 57.5)
#dim(mdiHouseCompet)

formula_dem_no_covar  <- as.formula("dv_dem ~ dem_avgIdeal + rep_avgIdeal")
formula_rep_no_covar  <- as.formula("dv_rep ~ dem_avgIdeal + rep_avgIdeal")
formula_dem_covar     <- as.formula("dv_dem ~ dem_avgIdeal + rep_avgIdeal + trump2016Pct2Party + dem_inc + rep_inc +
                                blackPct + hispPct  + medianIncome + baPct")
                                #+ log(rep_receipt + 0.001) + dem_pctUncivil + rep_pctUncivil + log(dem_followers + 0.01) + log(rep_followers + 0.01)
formula_rep_covar     <- as.formula("dv_rep ~ dem_avgIdeal + rep_avgIdeal + trump2016Pct2Party + dem_inc + rep_inc +
                                blackPct + hispPct  + medianIncome + baPct") #+ log(rep_receipt + 0.001)

dem_no_covar  <- fixest::feols(formula_dem_no_covar,  data = mdiHouseAll)
summary(dem_no_covar)

dem_covar  <- fixest::feols(formula_dem_covar,  data = mdiHouseAll)
summary(dem_covar)

rep_no_covar  <- fixest::feols(formula_rep_no_covar,  data = mdiHouseAll)
summary(rep_no_covar)

rep_covar  <- fixest::feols(formula_rep_covar,  data = mdiHouseAll)
summary(rep_covar)

# Competitive districts
dem_no_covar_compet  <- fixest::feols(formula_dem_no_covar,  
                                      data = mdiHouseCompet %>% filter(dem_receipt>0.1))
summary(dem_no_covar_compet)

rep_no_covar_compet  <- fixest::feols(formula_rep_no_covar,  
                                      data = mdiHouseCompet %>% filter(rep_receipt>0.1))
summary(rep_no_covar_compet)

dem_covar_compet  <- fixest::feols(formula_dem_covar,  
                                   data = mdiHouseCompet %>% filter(dem_receipt>0.1))
summary(dem_covar_compet)

rep_covar_compet  <- fixest::feols(formula_rep_covar,  
                                   data = mdiHouseCompet %>% filter(rep_receipt>0.1))
summary(rep_covar_compet)

return = list("dem_no_covar" = dem_no_covar,
              "rep_no_covar" = rep_no_covar,
              "dem_covar"    = dem_covar,
              "rep_covar"    = rep_covar,
              "dem_no_covar_compet" = dem_no_covar_compet,
              "rep_no_covar_compet" = rep_no_covar_compet,
              "dem_covar_compet"    = dem_covar_compet,
              "rep_covar_compet"    = rep_covar_compet,
              "dta.tmp" = mdiHouseAll)
}


moneyFame_table_FUNCTION = function(year, depVar, party){
if(year == 2020 & depVar == "total_donations") res.tmp = total_2020
if(year == 2022 & depVar == "total_donations") res.tmp = total_2022
if(year == 2020 & depVar == "small_donations") res.tmp = small_2020
if(year == 2022 & depVar == "small_donations") res.tmp = small_2022
if(year == 2020 & depVar == "followers") res.tmp = follow_2020
if(depVar == "total_donations") title_term = "total donations"
if(depVar == "small_donations") title_term = "small donations"
if(depVar == "followers")       title_term = "followers"

if(party == "Democratic"){
  res.tmp_list = list(res.tmp$dem_no_covar,
                      res.tmp$dem_covar,
                      res.tmp$dem_no_covar_compet,
                      res.tmp$dem_covar_compet)}
if(party == "Republican"){
  res.tmp_list = list(res.tmp$rep_no_covar,
                      res.tmp$rep_covar,
                      res.tmp$rep_no_covar_compet,
                      res.tmp$rep_covar_compet)}

  fixest::etable(res.tmp_list, 
                 tex = TRUE, digits = 2,
                 fontsize = "footnotesize", 
                 title = paste0("House ", party, " ", title_term, ", ", year),
                 se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
                 order = "!Constant",
                 dict = c("cutpoint_diff" = "Cutpoint difference",
                          "dv_dem" = title_term,
                          "dv_rep" = title_term,
                          "dem_diff*100" = "Difference in Democratic percent",
                          "dem_avgIdeal" = "Dem. ideology",
                          "rep_avgIdeal" = "Rep. ideology",
                          "rep_inc" = "Rep. incumbent",
                          "dem_inc" = "Dem. incumbent",
                          "trump2016Pct2Party" = "Trump 2016",
                          "`2020`" = "2020",
                          "blackPct" = "Black pct.",
                          "hispPct" = "Hispanic pct.",
                          "medianIncome" = "Median income",
                          "baPct" = "College pct.",
                          "state_blackPct" = "Black pct.",
                          "state_hispPct" = "Hispanic pct.",
                          "state_medianIncome" = "Median income",
                          "state_baPct" = "College pct.",
                          "state" = "State"),
                 notes = "Note: Median income in \\$100,000",
                 headers = list(":_:" = list("All" = 2, "Competitive" = 2)))
}