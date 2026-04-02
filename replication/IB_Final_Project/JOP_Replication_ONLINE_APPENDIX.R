#### Replication file for ONLINE APPENDIX for "Ideological Moderation and Success in U.S. Elections, 2020-2022" 
#### Journal of Politics

#### Read me
# 1) Save "JOP_2025_Replication" folder
  #   a) Function files are in a subfolder called h_functions
  #   b) Data files are in a subfolder called Data

# 2) Code
  # a) JOP_Replication.R replicates all figures and tables in main body of paper
  # b) JOP_Replication_ONLINE_APPENDIX.R replicates all figures and tables in appendix


# Load packages
library(tidyverse)
library(estimatr)
library(tinytex)
library(readxl)
library(scales)
library(colorspace)
library(kableExtra)
library(haven)
library(fixest)
library(data.table)

## Load functions
sapply(paste0("h_functions/", list.files("h_functions")), source)


######################
## Figure A1: Extreme words
######################
# Load dfm for Twitter 2020 from Desktop in ICC (or old black laptop)
countMatrix = data.frame(data.table::fread(
  file = "Data/countMatrixPolTwitter2020Libcon_labelFish.LFS.csv"))

# Load ideal points
idealAll = read_csv("Data/IdealPointsAll2020_labelFish.csv") %>% 
  mutate("webIdeal" = idealWebLibcon, 
         "twIdeal"  = idealTwitterLibcon) %>%
  mutate_at(c("avgIdeal", "twWeight", "thetaExternal"), round, 2) %>%
  mutate(prob = round(pnorm(avgIdeal), 2), 
         candidate = paste0(candidate," (", party,")"),
         district = case_when(str_detect(district, "-Se") ~ paste0(district, "n"),
                              TRUE ~ district))   

qq = left_join(countMatrix, idealAll, by = "candStem") 
avgW = mean(qq$wordCountTwitterLibcon, na.rm=TRUE)

# Sum over uni and ngrams with term #names(countMatrix)[grep("green", names(countMatrix))]
countMatrix <- countMatrix %>%
  mutate(sum_bipartisan = rowSums(across(contains("bipartisan"))),
         sum_veterans = rowSums(across(contains("veterans"))),
         sum_workforce = rowSums(across(contains("workforce"))),
         sum_seniors = rowSums(across(contains("seniors"))),
         sum_blm = rowSums(across(contains("blm"))),
         sum_maga = rowSums(across(contains("maga"))),
         sum_antifa = rowSums(across(contains("antifa"))),
         sum_monopol = rowSums(across(contains("monopol"))),
         sum_working_families = rowSums(across(contains("working_famil"))),
         sum_troll = rowSums(across(contains("troll"))),
         sum_endless_war = rowSums(across(contains("endless_war"))),
         sum_stupid = rowSums(across(contains("stupid"))))

maxTemp = 100
maxY = maxTemp

vTerms = paste0("sum_", c("blm", "antifa", "maga", "stupid") )
maxY_list = c(20, 20, 40, 20)

idealFillA = seq(min(qq$twIdeal, na.rm=T), 
                 max(qq$twIdeal, na.rm=T), 
                 by = 0.1)
idealFillD = seq(min(qq$twIdeal, na.rm=T), 0, by = 0.1)
idealFillR = seq(0, max(qq$twIdeal, na.rm=T), by = 0.1)

savefile = 0 # Set to 1 to create png file
if(savefile ==1) png("vWords_Dec2025.png", width = 2000, height = 1500, res = 200)

par(mfrow=c(2, 2), 
    mar=c(1.5, 1.0, 1.5,  1.5), 
    oma=c(1.0, 2.0, 1.0,  0.3))	 	# mar(south, west, north, east)

for(vv in 1:length(vTerms)){
  #vv=1
  term = vTerms[vv]
  
  ww = which(colnames(countMatrix)==term)
  tt = table(as.vector(
    countMatrix[qq$incumbent.y==1 & 
                  is.na(qq$twIdeal)==0, ww]))
  maxY = maxY_list[vv]
  
  allcap = 1*(term=="sum_blm"|term=="sum_maga")
  if(allcap == 0) mainTitle = str_to_title(gsub("sum_", " ", term))
  if(allcap == 1) mainTitle = str_to_upper(gsub("sum_", " ", term))
  
  # Plot all observations Twitter
  plot(jitter(qq$twIdeal), jitter(countMatrix[, ww]), 
       xlab = "", ylab = "", main="", 
       type="n", cex.axis=0.9, 
       xaxt="n", yaxt="n", 
       ylim = c(0, maxY), 
       xlim = c(min(qq$twIdeal,na.rm=T), max(qq$twIdeal,na.rm=T)))
  
  points(jitter(qq$webIdeal), jitter(countMatrix[, ww]), pch=16, cex = 0.7, col="#3c78c3")
  if(vv > 2)    mtext("Ideology", las = 1, 
                      side = 1, 
                      at = 0.5*(min(qq$twIdeal)+max(qq$twIdeal)),
                      line = 1.0, cex = 0.6)	
  axis(1, tick = T, tck = -0.03, 
       at = seq(-3, 3, by=0.5), 
       labels = seq(-3, 3, by=0.5),  
       cex.axis = 0.5, mgp = c(1,0.001,0))
  if(!vv %% 2 == 0)  mtext("Word\nCount", side = 2, 
                           las = 1, 
                           at = 1.1*maxY, 
                           line = 0.5, cex = 0.6)	
  mtext(mainTitle, side = 3, las = 1, at = 0, 
        line = 0.2, cex = 0.7)	
  axis(2, las = 1, tick = T, cex.axis = 0.6, mgp = c(2,.7,0), 
       at     = seq(0, maxY, by=round(maxY/5)), 
       labels = seq(0, maxY, by=round(maxY/5)))
  
  ## Fitted values
  countAll = glm(countMatrix[, ww] ~ 
                   qq$twIdeal +
                   qq$wordCountTwitterLibcon,
                 family = poisson)
  fitCountAll = exp(coef(countAll)[1] + 
                      coef(countAll)[2]* idealFillA + 
                      coef(countAll)[3]* avgW)
  lines(idealFillA, fitCountAll, col = "grey", lwd = 2)
  
  countDem = glm(countMatrix[, ww][qq$party.y== "D"] ~ 
                   qq$twIdeal[qq$party.y== "D"] +
                   qq$wordCountTwitterLibcon[qq$party.y== "D"],
                 family = poisson)
  fitCountDem = exp(coef(countDem)[1] + 
                      coef(countDem)[2]* idealFillD + 
                      coef(countDem)[3]* avgW)
  lines(idealFillD, fitCountDem, col = "darkblue", lwd = 2)
  countRep = glm(countMatrix[, ww][qq$party.y== "R"] ~ 
                   qq$twIdeal  [qq$party.y== "R"] +
                   qq$wordCountTwitterLibcon[qq$party.y== "R"],
                 family = poisson)
  fitCountRep = exp(coef(countRep)[1] + 
                      coef(countRep)[2]* idealFillR + 
                      coef(countRep)[3]* avgW)
  lines(idealFillR, fitCountRep, col = "darkred", lwd = 2)
  maxY = max(fitCountDem,   fitCountRep,   fitCountAll)+1
  
}

if(savefile==1) dev.off()



######################
## Figure A2: Moderate words
######################
vTerms = paste0("sum_", c("bipartisan", "veterans", "seniors", "workforce")  )
maxY_list = c(140, 140, 40, 40)

savefile = 0 # Set to 1 to create png file
if(savefile ==1) png("MtWords_Dec2025.png", width = 2000, height = 1500, res = 200)

par(mfrow=c(2, 2), 
    mar=c(1.5, 1.0, 1.5,  1.5), 
    oma=c(1.0, 2.0, 1.0,  0.3))	 	# mar(south, west, north, east)

for(vv in 1:length(vTerms)){
  #vv=1
  term = vTerms[vv]
  
  ww = which(colnames(countMatrix)==term)
  tt = table(as.vector(
    countMatrix[qq$incumbent.y==1 & 
                  is.na(qq$twIdeal)==0, ww]))
  maxY = maxY_list[vv]
  mainTitle = str_to_title(gsub("sum_", " ", term))
  
  # Plot all observations Twitter
  plot(jitter(qq$twIdeal), jitter(countMatrix[, ww]), 
       xlab = "", ylab = "", main="", 
       type="n", cex.axis=0.9, 
       xaxt="n", yaxt="n", 
       ylim = c(0, maxY), 
       xlim = c(min(qq$twIdeal,na.rm=T), max(qq$twIdeal,na.rm=T)))
  
  points(jitter(qq$webIdeal), jitter(countMatrix[, ww]), pch=16, cex = 0.7, col="#3c78c3")
  if(vv > 2)    mtext("Ideology", las = 1, 
                      side = 1, 
                      at = 0.5*(min(qq$twIdeal)+max(qq$twIdeal)),
                      line = 1.0, cex = 0.6)	
  axis(1, tick = T, tck = -0.03, 
       at = seq(-3, 3, by=0.5), 
       labels = seq(-3, 3, by=0.5),  
       cex.axis = 0.5, mgp = c(1,0.001,0))
  if(!vv %% 2 == 0)  mtext("Word\nCount", side = 2, 
                           las = 1, 
                           at = 1.1*maxY, 
                           line = 0.5, cex = 0.6)	
  mtext(mainTitle, side = 3, las = 1, at = 0, 
        line = 0.2, cex = 0.7)	
  axis(2, las = 1, tick = T, cex.axis = 0.6, mgp = c(2,.7,0), 
       at     = seq(0, maxY, by=round(maxY/5)), 
       labels = seq(0, maxY, by=round(maxY/5)))
  
  ## Fitted values
  countAll = glm(countMatrix[, ww] ~ 
                   qq$twIdeal +
                   qq$wordCountTwitterLibcon,
                 family = poisson)
  fitCountAll = exp(coef(countAll)[1] + 
                      coef(countAll)[2]* idealFillA + 
                      coef(countAll)[3]* avgW)
  lines(idealFillA, fitCountAll, col = "grey", lwd = 2)
  
  countDem = glm(countMatrix[, ww][qq$party.y== "D"] ~ 
                   qq$twIdeal[qq$party.y== "D"] +
                   qq$wordCountTwitterLibcon[qq$party.y== "D"],
                 family = poisson)
  fitCountDem = exp(coef(countDem)[1] + 
                      coef(countDem)[2]* idealFillD + 
                      coef(countDem)[3]* avgW)
  lines(idealFillD, fitCountDem, col = "darkblue", lwd = 2)
  countRep = glm(countMatrix[, ww][qq$party.y== "R"] ~ 
                   qq$twIdeal  [qq$party.y== "R"] +
                   qq$wordCountTwitterLibcon[qq$party.y== "R"],
                 family = poisson)
  fitCountRep = exp(coef(countRep)[1] + 
                      coef(countRep)[2]* idealFillR + 
                      coef(countRep)[3]* avgW)
  lines(idealFillR, fitCountRep, col = "darkred", lwd = 2)
  maxY = max(fitCountDem,   fitCountRep,   fitCountAll)+1
  
}
if(savefile==1) dev.off()

######################
## Figure A3: Ideological densities
######################

mci <- read_excel(
  paste0("Data/main_candidate_information_2022Congress.xlsx"), na= "NA") %>%  mutate(partyVAR = party)

idealAll2022 = read_csv("Data/IdealPointsAll2022_labelFish.csv") %>% 
  mutate("webIdeal" = idealWebLibcon, 
         "twIdeal"  = idealTwitterLibcon) %>%
  mutate_at(c("avgIdeal", "twWeight", "thetaExternal"), round, 2) %>%
  mutate(prob = round(pnorm(avgIdeal), 2), 
         candidate = paste0(candidate," (", party,")"),
         district = case_when(str_detect(district, "-Se") ~ paste0(district, "n"),
                              TRUE ~ district),
         "influencer" = grepl("INFL", candStem)) 

allCand = mci %>% dplyr::select(candStem, chamber, party, generalElec, incumbent, primaryPct) %>%
  left_join(idealAll2022 %>% dplyr::select(candStem, avgIdeal, idealTwitter, idealWeb, twWeight))

# All with both ideal points
ipBoth = idealAll2022 %>% filter(is.na(idealWeb)==0 & is.na(idealTwitter)==0)

# Incumbents only
ipInc = idealAll2022 %>% filter(incumbent ==1)

# Non-Incumbents only
ipNonInc = idealAll2022 %>% filter(incumbent ==0)

# Influencers
ipInfl = idealAll2022 %>% filter(influencer==1)

# Governors
ipGov = idealAll2022 %>% filter(grepl("Gov", candStem))

# Senators
ipSen = idealAll2022 %>% filter(grepl("Sen", candStem))


# Incumbent Dems only
ipIncDem = idealAll2022 %>% filter(incumbent ==1 & party == "D"& is.na(idealWeb)==0 & is.na(idealTwitter)==0)

# Non-Incumbent Dems only
ipNonIncDem = idealAll2022 %>% filter(incumbent ==0 & party == "D"& is.na(idealWeb)==0 & is.na(idealTwitter)==0)

# Incumbent GOP only
ipIncRep = idealAll2022 %>% filter(incumbent ==1 & party == "R" & is.na(idealWeb)==0 & is.na(idealTwitter)==0)

# Non-Incumbent GOP only
ipNonIncRep = idealAll2022 %>% filter(incumbent ==0 & party == "R" &is.na(idealWeb)==0 & is.na(idealTwitter)==0)

# Densities
d_all_web    = density(idealAll2022$idealWeb, na.rm = T)
d_all_tw     = density(idealAll2022$idealTwitter, na.rm = T)
d_infl_tw    = density(ipInfl$idealTwitter, na.rm = T)
d_both_web   = density(ipBoth$idealWeb, na.rm = T)
d_both_tw    = density(ipBoth$idealTwitter, na.rm = T)
d_inc_web    = density(ipInc$idealWeb, na.rm = T)
d_inc_tw     = density(ipInc$idealTwitter, na.rm = T)
d_noninc_web = density(ipNonInc$idealWeb, na.rm = T)
d_noninc_tw  = density(ipNonInc$idealTwitter, na.rm = T)
d_inc_nom    = density(ipInc$thetaExternal, na.rm = T)
d_gov        = density(ipGov$idealTwitter, na.rm = T)
d_sen_tw    = density(ipSen$idealTwitter, na.rm = T)
d_sen_web   = density(ipSen$idealWeb, na.rm = T)
d_sen_inc_tw    = density(ipSen$idealTwitter[ipSen$incumbent==1], na.rm = T)
d_sen_inc_web   = density(ipSen$idealWeb[ipSen$incumbent==1], na.rm = T)


par(mfrow=c(2, 2), 
    mar=c(1.5, 2, 1.2,  0.8), 
    oma=c(1, 1.0, 0.5,  0.3))	 	# mar(south, west, north, east)
xplotMax = 2.5

# Incumbents
plot(d_inc_web, type = "n", xlab= "Ideology", main = "",
     xaxt="n", yaxt="n", xlim = c(-xplotMax, xplotMax))
mtext("House - incumbents", 3, cex = 0.9, line=0.1)
axis(1, tick = T, tck = -0.03, 
     at = seq(-4, 4, by=1), 
     labels = seq(-4, 4, by=1),  
     cex.axis = 0.65, mgp = c(1,0.001,0))
d_inc_web
polygon(d_inc_tw,  col = rgb(147/255, 233/255, 190/255, alpha = 0.6), border = "white")
polygon(d_inc_web, col = rgb(225/255, 218/255, 185/255, alpha = 0.6), border = "white")
text(xplotMax-0.95, max(d_inc_web$y)-0.03, "Web",     
     col = rgb(225/255, 218/255, 185/255))
text(xplotMax-0.45, max(d_inc_tw$y)-0.30, "Twitter", 
     col = rgb(147/255, 233/255, 190/255))

# House non-incumbents
plot(d_noninc_web, type = "n", xlab= "Ideology", 
     main = "",
     xaxt="n", yaxt="n", xlim = c(-xplotMax, xplotMax))
mtext("House - non-incumbents", 3, cex = 0.9, line=0.1)
axis(1, tick = T, tck = -0.03, 
     at = seq(-4, 4, by=1), 
     labels = seq(-4, 4, by=1),  
     cex.axis = 0.65, mgp = c(1,0.001,0))
polygon(d_noninc_tw,  col = rgb(147/255, 233/255, 190/255, alpha = 0.6), border = "white")
polygon(d_noninc_web, col = rgb(225/255, 218/255, 185/255, alpha = 0.6), border = "white")
text(xplotMax-0.45, max(d_noninc_tw$y)-0.11, "Twitter", 
     col = rgb(147/255, 233/255, 190/255))
text(xplotMax-0.90, max(d_noninc_web$y)-0.07, "Web",     
     col = rgb(225/255, 218/255, 185/255))

# Senators
plot(d_sen_web, type = "n", xlab= "Ideology", 
     main = "",
     xaxt="n", yaxt="n", xlim = c(-xplotMax, xplotMax))
mtext("Senate", 3, cex = 0.9, line=0.1)
axis(1, tick = T, tck = -0.03, 
     at = seq(-4, 4, by=1), 
     labels = seq(-4, 4, by=1),  
     cex.axis = 0.65, mgp = c(1,0.001,0))
polygon(d_sen_tw,  col = rgb(147/255, 233/255, 190/255, alpha = 0.6), border = "white")
polygon(d_sen_web, col = rgb(225/255, 218/255, 185/255, alpha = 0.6), border = "white")
text(xplotMax-0.45, max(d_sen_tw$y)-0.11, "Twitter", 
     col = rgb(147/255, 233/255, 190/255))
text(xplotMax-0.90, max(d_sen_web$y)-0.07, "Web",     
     col = rgb(225/255, 218/255, 185/255))



# Governors
plot(d_gov, type = "n", xlab= "Ideology", 
     main = "",
     xaxt="n", yaxt="n", xlim = c(-xplotMax, xplotMax))
mtext("Governors", 3, cex = 0.9, line=0.1)
axis(1, tick = T, tck = -0.03, 
     at = seq(-4, 4, by=1), 
     labels = seq(-4, 4, by=1),  
     cex.axis = 0.65, mgp = c(1,0.001,0))
polygon(d_gov,  col = rgb(147/255, 233/255, 190/255, alpha = 0.6), border = "white")
text(xplotMax-0.7, max(d_gov$y)-0.14, "Twitter", 
     col = rgb(147/255, 233/255, 190/255))


######################
## Table A1: Ideological inter-party terms, 2020
######################
cutlist = c('zzqq')

# Web interparty words
tableLen = 10
truncationLength = 24

YEAR = 2020
saveName = paste0("Web",       YEAR, "Libcon_labelFish")
termResultsInterParty = read.csv(
  paste0("Data/termsInterParty", saveName, ".csv"))

# Liberal interparty words
libInterPartyWordsWeb =  termResultsInterParty %>% 
  filter(totCount> 150 & !grepl(cutlist, term)) %>% 
  arrange(b1) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  unique() %>% 
  slice(1:tableLen) %>% 
  arrange(term)

# Conservative interparty words
conInterPartyWordsWeb =  termResultsInterParty %>%
  filter(totCount> 150 & !grepl(cutlist, term)) %>%
  arrange(desc(b1)) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  unique() %>% 
  slice(1:tableLen) %>% 
  arrange(term)

# Twitter interparty words
YEAR = 2020
saveName = paste0("Twitter",       YEAR, "Libcon_labelFish")

termResultsInterParty = read.csv(
  paste0("Data/termsInterParty", saveName, ".csv")) 

# Liberal interparty words
libInterPartyWords =  termResultsInterParty %>% 
  filter(totCount> 150 & !grepl(cutlist, term)) %>% 
  arrange(b1) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  unique() %>% 
  slice(1:tableLen) %>% 
  arrange(term)

# Conservative interparty words
conInterPartyWords =  termResultsInterParty %>%
  filter(totCount> 150 & !grepl(cutlist, term)) %>%
  arrange(desc(b1)) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  unique() %>% 
  slice(1:tableLen) %>% 
  arrange(term)

wordTable = data.frame(cbind(libInterPartyWordsWeb, conInterPartyWordsWeb, 
                             libInterPartyWords, conInterPartyWords))

knitr::kable(wordTable,
             caption = 'Ideological inter-party terms, 2020',
             booktabs = T,
             col.names = c("Liberal", "Conservative", "Liberal", "Conservative")) %>%   
  add_header_above(c("Web" = 2, "Twitter" = 2))  %>%
  kable_minimal(font_size = 11, full_width = FALSE) 


######################
## Table A2: Ideological website terms
######################
truncationLength = 18
tableLen = 11

# Web words by party
YEAR = 2020
saveName = paste0("Web",       YEAR, "Libcon_labelFish")

termResultsDem = read.csv(
  paste0("Data/termsDem", saveName, ".csv"))  
termResultsRep = read.csv(
  paste0("Data/termsRep", saveName, ".csv")) 
termResultsInterParty = read.csv(
  paste0("Data/termsInterParty", saveName, ".csv"))

# Liberal Democratic words
libDemWords =  termResultsDem %>% 
  filter(demCount> 150 & !grepl(cutlist, term)) %>% 
  arrange(b1) %>% 
  slice(1:tableLen) %>%
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  arrange(term)
#libDemWords

# Moderate Democratic words
modDemWords =  termResultsDem %>%
  filter(demCount> 150 & !grepl(cutlist, term)) %>%
  arrange(desc(b1)) %>% 
  slice(1:tableLen) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  arrange(term)
#modDemWords

# Conservative Republican words
conRepWords =  termResultsRep %>%
  filter(repCount> 150 & !grepl(cutlist, term)) %>%
  arrange(desc(b1)) %>% 
  slice(1:tableLen) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  arrange(term)
#conRepWords

# Moderate Republican words
modRepWords =  termResultsRep %>%
  filter(repCount> 190 & !grepl(cutlist, term)) %>% 
  arrange(b1) %>%
  slice(1:tableLen) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  arrange(term)
#modRepWords

wordTable = data.frame(cbind(libDemWords, modDemWords, modRepWords, conRepWords))
knitr::kable(wordTable,
             caption = 'Ideological website terms (truncated), by group', booktabs = T, 
             col.names = c("Liberal Democrat   ", "Moderate Democrat  ", "Moderate Republican  ", "Conservative")) %>% 
  kable_minimal(font_size = 10, full_width = FALSE)

######################
## Table A3: Ideological Twitter terms
######################

## Twitter words across parties
truncationLength = truncationLength+1 # Show one more letter for this table

saveName = paste0("Twitter", YEAR, "Libcon_labelFish")
termResultsDem = read.csv(
  paste0("Data/termsDem", saveName, ".csv")) 
termResultsRep = read.csv(
  paste0("Data/termsRep", saveName, ".csv")) 

# Liberal Democratic words
libDemWords =  termResultsDem %>% 
  filter(demCount> 150 & !grepl(cutlist, term)) %>% 
  arrange(b1) %>% 
  slice(1:tableLen) %>%
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  arrange(term)                #libDemWords

# Moderate Democratic words
modDemWords =  termResultsDem %>%
  filter(demCount> 150 & !grepl(cutlist, term)) %>%
  arrange(desc(b1)) %>% slice(1:tableLen) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  mutate(term = substr(term, 1, 20)) %>%
  arrange(term)

# Conservative Republican words
conRepWords =  termResultsRep %>%
  filter(repCount> 150 & !grepl(cutlist, term)) %>%
  filter(term != "keep_bear" & term != "right_keep" & term != "right_keep_bear") %>%
  arrange(desc(b1)) %>% 
  slice(1:tableLen) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  arrange(term)

# Moderate Republican words
modRepWords =  termResultsRep %>%
  filter(repCount> 190 & !grepl(cutlist, term)) %>% 
  arrange(b1) %>%
  slice(1:tableLen) %>% 
  mutate(term = substr(term, 1, truncationLength)) %>%
  dplyr::select(term) %>%
  arrange(term)

wordTable = data.frame(cbind(libDemWords, modDemWords, modRepWords, conRepWords))
knitr::kable(wordTable,
             caption = 'Ideological Twitter terms (truncated), by group', booktabs = T,
             col.names = c("Liberal Democrat   ", "Moderate Democrat  ", 
                           "Moderate Republican  ", "Conservative Republican")) %>% 
  kable_minimal(font_size = 10, full_width = FALSE)


######################
## Table A4: Selected candidates
######################

# Load 2020 data with all ideal points
idealAll = read_csv("Data/IdealPointsAll2020_labelFish.csv") %>% 
  mutate("webIdeal" = idealWebLibcon, 
         "twIdeal"  = idealTwitterLibcon) %>%
  mutate_at(c("avgIdeal", "twWeight", "thetaExternal"), round, 2) %>%
  mutate(prob = round(pnorm(avgIdeal), 2), 
         candidate = paste0(candidate," (", party,")"),
         district = case_when(str_detect(district, "-Se") ~ paste0(district, "n"),
                              TRUE ~ district))   

incumbentAvg = idealAll %>%
  filter(incumbent ==1 & (party =="D"|party =="R")) %>%
  group_by(party) %>%
  summarize(incumbentAvg = mean(avgIdeal, na.rm=TRUE))

# Single table with all member ideal points
selIncumbents = idealAll %>%
  filter(str_detect(candidate, 
                    pattern = "r Lamb|Ocasio|Ilhan Omar|Spanb|Gaetz|Gosar|Goh|Kinzinger|Anthony Gonz|Jim Jordan|Hoyer|Cuellar|Katko|n Fitzpatrick|Sinema|Brindisi|Luria|Xochitl|Boebert|r Greene|g Kim|Cawthorne|ta Hart|l Bowman|i Bush|Warnock")) %>%
  arrange(desc(avgIdeal)) %>% 
  dplyr::select(c("Candidate" = candidate, "District" = district, "Ideal Point" = avgIdeal)) #,"Trump 2016" = trump2016Pct  

knitr::kable(selIncumbents,  
             caption = 'Selected Candidates', 
             booktabs = T,
             align=rep('c', length(selIncumbents[,1]))) %>%
  kable_minimal(font_size = 11) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))



######################
## Table A5: Effect of word count on ideology estimates
######################

resultsTable = data.frame("Year" = c(2020, 2020, 2022, 2022), 
                          "Media" = c("Web", "Twitter", "Web", "Twitter"), 
                          "DemCoef"= NA, "DemWords_75th"= NA, "DemChange" = NA, 
                          "RepCoef"= NA, "RepWords_75th"= NA, "RepChange" = NA)

for(yy in c(2020, 2022)){
  YEAR = yy
  results = read.csv(
    paste0("Data/IdealPointsAll", YEAR, "_labelFish.csv"))
  
  wordCount.ols.dem = lm(avgIdeal~ wordCountWebLibcon + wordCountTwitterLibcon, 
                         data = results %>% filter(party=="D"))
  summary(wordCount.ols.dem)
  if(YEAR ==2020) baseRow = 1
  if(YEAR ==2022) baseRow = 3
  
  resultsTable[baseRow, "DemCoef"] = 
    paste0(round(coefficients(wordCount.ols.dem)["wordCountWebLibcon"], 7), " (t = ", 
           round(summary(wordCount.ols.dem)$coefficients["wordCountWebLibcon","t value"], 
                 2), ")")
  resultsTable[baseRow, "DemWords_75th"] = 
    floor(summary(results$wordCountWebLibcon[results$party=="D"])["3rd Qu."])
  resultsTable[baseRow, "DemChange"] = 
    round(summary(results$wordCountWebLibcon[results$party=="D"])["3rd Qu."]* 
            coefficients(wordCount.ols.dem)["wordCountWebLibcon"], 2)
  
  resultsTable[baseRow+1, "DemCoef"] = 
    paste0(round(coefficients(wordCount.ols.dem)["wordCountTwitterLibcon"], 6), " (t = ", 
           round(summary(wordCount.ols.dem)$coefficients["wordCountTwitterLibcon",
                                                         "t value"], 2), ")")
  resultsTable[baseRow+1, "DemWords_75th"] = 
    floor(summary(results$wordCountTwitterLibcon[results$party=="D"])["3rd Qu."])
  
  resultsTable[baseRow+1, "DemChange"] = 
    round(summary(results$wordCountTwitterLibcon[results$party=="D"])["3rd Qu."]* 
            coefficients(wordCount.ols.dem)["wordCountTwitterLibcon"], 2)
  
  wordCount.ols.rep = lm(avgIdeal~ wordCountWebLibcon + wordCountTwitterLibcon, 
                         data = results %>% filter(party=="R"))
  summary(wordCount.ols.rep)
  
  
  resultsTable[baseRow, "RepCoef"] = 
    paste0(round(coefficients(wordCount.ols.rep)["wordCountWebLibcon"], 6), " (t = ", 
           round(summary(wordCount.ols.rep)$coefficients["wordCountWebLibcon","t value"], 
                 2), ")")
  resultsTable[baseRow, "RepWords_75th"] = 
    floor(summary(results$wordCountWebLibcon[results$party=="R"])["3rd Qu."])
  resultsTable[baseRow, "RepChange"] = 
    round(summary(results$wordCountWebLibcon[results$party=="R"])["3rd Qu."]* 
            coefficients(wordCount.ols.rep)["wordCountWebLibcon"], 2)
  
  resultsTable[baseRow+1, "RepCoef"] = 
    paste0(round(coefficients(wordCount.ols.rep)["wordCountTwitterLibcon"], 6), " (t = ", 
           round(summary(wordCount.ols.rep)$coefficients["wordCountTwitterLibcon",
                                                         "t value"], 2), ")")
  resultsTable[baseRow+1, "RepWords_75th"] = 
    floor(summary(results$wordCountTwitterLibcon[results$party=="R"])["3rd Qu."])
  
  resultsTable[baseRow+1, "RepChange"] = 
    round(summary(results$wordCountTwitterLibcon[results$party=="R"])["3rd Qu."]* 
            coefficients(wordCount.ols.rep)["wordCountTwitterLibcon"], 2)
} # END for yy loop

resultsTable

######################
## Table B1: House overperformers 2020
######################

# Generate estimates used below: House v pres and house v senate models
r_hp_2020 = kappa_eachYear_House_FUNCTION(2020, compare = "Pres", measure = "avgText", clusterCode =1)
r_hp_2022 = kappa_eachYear_House_FUNCTION(2022, compare = "Pres", clusterCode =1)
r_hs_2020 = kappa_eachYear_House_FUNCTION(2020, compare = "Sen",  clusterCode =1)

## House v President 2020 overperformers

repMedian = quantile(r_hp_2020$mdiHouseAll$rep_avgIdeal)["50%"]
demMedian = quantile(r_hp_2020$mdiHouseAll$dem_avgIdeal)["50%"]

# Republican overperformers
rep2020_overperformers=r_hp_2020$mdiHouseAll %>% 
  filter(demDiff_HousePres <  -3 & (biden2020Pct > 42 & biden2020Pct<58)) %>%  #
  dplyr::select(stateDist, demDiff_HousePres, dem_candStem, dem_avgIdeal, rep_candStem, rep_avgIdeal,
                house_dem_inc, house_rep_inc,
                biden2020Pct, demPct, demPct2Party, trump2020Pct) %>%
  mutate(demModerate = 1*(dem_avgIdeal>demMedian),
         repModerate = 1*(rep_avgIdeal<repMedian)) %>%
  arrange(demDiff_HousePres) 

repOver = table(rep2020_overperformers$house_rep_inc, rep2020_overperformers$repModerate)
row.names(repOver) = c("Not incumbent", "Incumbent") 
colnames(repOver) = c("Not moderate", "Moderate") 

# Democrat overperformers 2020
dem2020_overperformers=r_hp_2020$mdiHouseAll %>% 
  filter(demDiff_HousePres >  3 & (biden2020Pct > 42 & biden2020Pct<58)) %>%  #
  dplyr::select(stateDist, demDiff_HousePres, dem_candStem, dem_avgIdeal, rep_candStem, rep_avgIdeal,
                house_dem_inc, house_rep_inc,
                biden2020Pct, demPct, demPct2Party, trump2020Pct) %>%
  mutate(demModerate = 1*(dem_avgIdeal>demMedian),
         repModerate = 1*(rep_avgIdeal<repMedian)) %>%
  arrange(demDiff_HousePres) 
dem2020_overperformers %>% dplyr::select(-c(stateDist, rep_candStem, biden2020Pct, demPct))
table(dem2020_overperformers$house_dem_inc, dem2020_overperformers$demModerate)

demOver = table(dem2020_overperformers$house_dem_inc, dem2020_overperformers$demModerate)
row.names(demOver) = c("Not incumbent", "Incumbent") 
colnames(demOver) = c("Not moderate", "Moderate") 

## Output for table
repOver 
demOver

######################
## Table B2: House overperformers 2022
######################
# Additional background in appendix code
repMedian2022 = quantile(r_hp_2022$mdiHouseAll$rep_avgIdeal)["50%"]
demMedian2022 = quantile(r_hp_2022$mdiHouseAll$dem_avgIdeal)["50%"]

# Republican overperformers in 2022
rep2022_overperformers=r_hp_2022$mdiHouseAll %>% 
  filter(demDiff_HousePres <  -4 & (biden2020Pct > 42 & biden2020Pct<58)) %>%  #
  dplyr::select(stateDist, demDiff_HousePres, dem_candStem, dem_avgIdeal, rep_candStem, rep_avgIdeal,
                house_dem_inc, house_rep_inc,
                biden2020Pct, demPct, demPct2Party, trump2020Pct) %>%
  mutate(demModerate = 1*(dem_avgIdeal>demMedian2022),
         repModerate = 1*(rep_avgIdeal<repMedian2022)) %>%
  arrange(demDiff_HousePres) 

repOver = table(rep2022_overperformers$house_rep_inc, rep2022_overperformers$repModerate)
row.names(repOver) = c("Not incumbent", "Incumbent") 
colnames(repOver) = c("Not moderate", "Moderate") 

# Democrat overperformers 2022
dem2022_overperformers=r_hp_2022$mdiHouseAll %>% 
  filter(demDiff_HousePres > 2  & (biden2020Pct > 42 & biden2020Pct<58)) %>%  #
  dplyr::select(stateDist, demDiff_HousePres, dem_candStem, dem_avgIdeal, rep_candStem, rep_avgIdeal,
                house_dem_inc, house_rep_inc,
                biden2020Pct, demPct, demPct2Party, trump2020Pct) %>%
  mutate(demModerate = 1*(dem_avgIdeal>demMedian2022),
         repModerate = 1*(rep_avgIdeal<repMedian2022)) %>%
  arrange(demDiff_HousePres) 
dem2022_overperformers %>% dplyr::select(-c(stateDist, rep_candStem, biden2020Pct, demPct))
table(dem2022_overperformers$house_dem_inc, dem2022_overperformers$demModerate)

demOver = table(dem2022_overperformers$house_dem_inc, dem2022_overperformers$demModerate)
row.names(demOver) = c("Not incumbent", "Incumbent") 
colnames(demOver) = c("Moderate")

## Output for Table
repOver
demOver

######################
## Table C1: House v Pres, 2020
######################
yearList = c(2020, 2022, 2020, 2022)

# Generate estimates used below: Senate-Pres, Senate-Gov, Pres-Gov
county_res_sp = kappa_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "sp", weights=1)
state_res_sp  = kappa_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "sp", weights=1)

# Senate-Governor comparisons
county_res_sg = kappa_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "sg", weights=1)
state_res_sg  = kappa_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "sg", weights=1)

# President - Governor comparisons
county_res_pg = kappa_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "pg", weights=1)
state_res_pg  = kappa_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "pg", weights=1)

# Run statistical models in results_figure_FUNCTION above
#r_hp_2020 = kappa_eachYear_House_FUNCTION(2020, measure = "avgText", compare = "Pres")

# Latex output using fixest::etable
# Set style parameters for etable
sty <- style.tex(tablefoot.value = "Signif. codes: *** p<0.01, ** p<0.05, * p<0.10.",
                 stats.title = "", var.title = "")

fixest::etable(list(r_hp_2020$all_no_covar,    r_hp_2020$all_inc_covar,    r_hp_2020$all_covar,
                    r_hp_2020$compet_no_covar, r_hp_2020$compet_inc_covar, r_hp_2020$compet_covar, r_hp_2020$hispend_covar),
               title = "House Dem. margin relative to Biden margin, 2020",
               label = "tab:hp2020", 
               tex=TRUE, 
               digits = 2,
               depvar = FALSE, 
               fontsize = "scriptsize",
               style.tex = sty,
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               order = "!Constant",
               dict = c("kappaDiff_HousePres" = "Cutpoint diff.",
                        "trump2020Pct2Party" = "Trump 2020 pct.",
                        "dem_inc" = "Dem. inc.",
                        "rep_inc" = "Rep. inc.",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "blackPct" = "Black pct.",
                        "hispPct" = "Hispanic pct.",
                        "medianIncome" = "Median income",
                        "baPct" = "College pct.",
                        "nonCollWhitePct" = "Non-college White pct.",
                        "dem_receipt" = "Dem. House receipts",
                        "rep_receipt" = "Rep. House receipts",
                        "demDiff_HousePres" = "Difference in Democratic percent",
                        "state" = "State"),
               notes = "\\hspace*{0.09 in} Median income in \\$100,000",
               headers = list(":_:" = list("Contested" = 3, "Competitive" = 3, "Fully funded" = 1)))

######################
## Table C2: House v Pres, 2022
######################
# Run statistical models in results_figure_FUNCTION above
#r_hp_2022 = kappa_eachYear_House_FUNCTION(2022, measure = "avgText", compare = "Pres")

# Latex output
sty <- style.tex(tablefoot.value = "Signif. codes: *** p<0.01, ** p<0.05, * p<0.10.",
                 stats.title = "", var.title = "")

fixest::etable(list(r_hp_2022$all_no_covar,    r_hp_2022$all_inc_covar,    r_hp_2022$all_covar,
                    r_hp_2022$compet_no_covar, r_hp_2022$compet_inc_covar, r_hp_2022$compet_covar, r_hp_2022$hispend_covar), 
               tex=TRUE, digits = 2,  depvar = FALSE, 
               fontsize = "scriptsize", 
               title = "House Dem. margin relative to Biden 2020 margin, 2022",
               label = "tab:hp2022",  
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               style.tex = sty,
               order = "!Constant",
               dict = c("kappaDiff_HousePres" = "Cutpoint diff.",
                        "trump2020Pct2Party" = "Trump 2020 pct.",
                        "dem_inc" = "Dem. inc.",
                        "rep_inc" = "Rep. inc.",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "blackPct" = "Black pct.",
                        "hispPct" = "Hispanic pct.",
                        "medianIncome" = "Median income",
                        "baPct" = "College pct.",
                        "nonCollWhitePct" = "Non-college White pct.",
                        "dem_receipt" = "Dem. House receipts",
                        "rep_receipt" = "Rep. House receipts",
                        "demDiff_HousePres" = "Difference in Democratic percent",
                        "state" = "State"),
               notes = "\\hspace*{0.09 in} Median income in \\$100,000",
               headers = list(":_:" = list("Contested" = 3, "Competitive" = 3, "Fully funded" = 1)))

######################
## Table C3: House v Senate, 2020
######################
# Run statistical models in results_figure_FUNCTION above
#r_hs_2020 = kappa_eachYear_House_FUNCTION(2020, measure = "avgText", compare = "Sen")

# Latex output
fixest::etable(list(r_hs_2020$all_no_covar,    r_hs_2020$all_inc_covar,    r_hs_2020$all_covar,
                    r_hs_2020$compet_no_covar, r_hs_2020$compet_inc_covar, r_hs_2020$compet_covar, r_hs_2020$hispend_covar), 
               tex=TRUE, digits = 2,  depvar = FALSE, 
               fontsize = "scriptsize", 
               title = "House Dem. margin relative to Senate Dem. margin, 2020", 
               label = "tab:hs2020", 
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               style.tex = sty,
               order = "!Constant",
               dict = c("kappaDiff_HouseSen" = "Cutpoint diff.",
                        "demDiff_HouseSen" = "Difference in Democratic percent",
                        "trump2016Pct2Party" = "Trump 2016 pct.",
                        "trump2020Pct2Party" = "Trump 2020 pct.",
                        "dem_house_inc" = "Dem. House inc.",
                        "rep_house_inc" = "Rep. House inc.",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "blackPct" = "Black pct.",
                        "hispPct" = "Hispanic pct.",
                        "medianIncome" = "Median income",
                        "baPct" = "College pct.",
                        "nonCollWhitePct" = "Non-college White pct.",
                        "dem_receipt" = "Dem. House receipts",
                        "rep_receipt" = "Rep. House receipts",
                        "state" = "State"),
               notes = "\\hspace*{0.09 in} Median income in \\$100,000",
               headers = list(":_:" = list("Contested" = 3, "Competitive" = 3, "Fully funded" = 1)))

######################
## Figure C1: Counterfactual Congressional Election Results
######################

par(mfrow=c(1, 1),
    mar=c(1.0, 1.2, 3, 0.5),
    oma=c(1.0, 4,   2, 0.4))  # mar(south, west, north, east)

## Create figure for counterfactual
cf2020 = simChange_FUNCTION(2020) %>% unlist()
cf2022 = simChange_FUNCTION(2022) %>% unlist()

# Use fully funded (high spend) model results
cf = rbind(c(cf2020[1], cf2020[6:7], cf2020[1], cf2020[8:9]),
           c(cf2022[1], cf2022[6:7], cf2022[1], cf2022[8:9]))

plot(1:6, seq(175, 260, length = 6),
     type="n", lty=2, xlab="", ylab="", xaxt='n', cex.axis=0.9, yaxt='n')
cf.labels = c("Actual\n", "Dem. moderate\nRep. actual", "Dem. moderate\nRep. extreme",
              "Actual\n", "Dem. actual\nRep. moderate", "Dem. extreme\nRep. moderate")
axis(1, at=seq(1:6),	labels=cf.labels, cex.axis=0.6, padj = -0.5,  tck = -0.02)
axis(2, at=seq(180, 250, by = 10),	labels=seq(180, 250, by = 10), 
     cex.axis=0.65, las = T,   mgp = c(3, 0.5, 0), tck = -0.025)
abline(v= 3.5, col="grey")
abline(h= 218, col="grey")
text(2.8, 214, "218 seats required for\nHouse majority", col= "grey", cex=0.6)
mtext("Number",    side = 2, las= T, cex = 0.8, line = 2.2, at = 240)
mtext("of",        side = 2, las= T, cex = 0.8, line = 3.0, at = 234)
mtext("Democrats", side = 2, las= T, cex = 0.8, line = 1.8, at = 228)


# Set colors
blues <- colorRampPalette(c("lightblue", "blue", "darkblue"))
#barplot(1:10, col = blues(10), main = "Blue Color Scale")
color_dem = blues(10)[c(4, 10)]
reds <- colorRampPalette(c("red", "tomato2", "darkred"))
#barplot(1:10, col = reds(10), main = "Red Color Scale")
color_rep = reds(10) [c(4, 10)]

for(rr in 1:2){
  lines(1:3, cf[rr,1:3], col= color_dem[rr])
  points(1:3, cf[rr,1:3], pch = 19, col= color_dem[rr])
  
  lines(4:6, cf[rr,4:6], col= color_rep[rr])
  points(4:6, cf[rr,4:6], pch = 19, col= color_rep[rr])
}

text(2.1, 255, "Changes Helping Democrats", col= "darkblue", cex = 0.8)
text(4.9, 255, "Changes Helping Republicans", col= "darkred", cex = 0.8)

text(1.2 , 225, "2020", col= color_dem[1], cex = 0.7)
text(1.23, 212, "2022", col= color_dem[2], cex = 0.7)

text(4.2 , 224, "2020", col= color_rep[1], cex = 0.7)
text(4.13, 210, "2022", col= color_rep[2], cex = 0.7)

######################
## Table D1: Senate v Pres
######################
fixest::etable(list(county_res_sp$res_no_covar,
                    county_res_sp$res_inc_covar,
                    county_res_sp$res_all_covar,
                    state_res_sp$res_no_covar,
                    state_res_sp$res_inc_covar,
                    state_res_sp$res_all_covar), 
               tex = TRUE, digits = 2,  depvar = FALSE, 
               fontsize = "footnotesize", 
               title = "Senate Dem. margin relative to Biden 2020 margin, 2020 & 2022", 
               label = "tab:sp202022",    
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               order = "!Constant",
               style.tex = sty,
               dict = c("cutpoint_diff" = "Cutpoint difference",
                        "dem_diff*100" = "Difference in Democratic percent",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "`2020`" = "2020",
                        "`Ethnicities.Black Alone`" = "Black pct.",
                        "`Ethnicities.Hispanic or Latino`" = "Hispanic pct.",
                        "`Income.Median Houseold Income`" = "Median income",
                        "`Education.Bachelor's Degree or Higher`" = "College pct.",
                        "state_blackPct" = "Black pct.",
                        "state_hispPct" = "Hispanic pct.",
                        "state_medianIncome" = "Median income",
                        "state_baPct" = "College pct.",
                        "state" = "State"),
               notes = "\\hspace*{0.09 in} Median income in \\$100,000. Weighted by number of votes. Standard errors for county results clustered by state.",
               headers = list(":_:" = list("County level" = 3, "State level" = 3)))

               

######################
## Table D2: Senate v Gov 
######################
fixest::etable(list(county_res_sg$res_no_covar,
                    county_res_sg$res_inc_covar,
                    county_res_sg$res_all_covar,
                    state_res_sg$res_no_covar,
                    state_res_sg$res_inc_covar,
                    state_res_sg$res_all_covar), 
               tex = TRUE, digits = 2,  depvar = FALSE, 
               fontsize = "footnotesize",  
               title = "Senate Dem. margin relative to gubernatorial Dem. margin, 2020 & 2022", 
               label = "tab:sg202022", se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               style.tex = sty,
               order = "!Constant",
               dict = c("cutpoint_diff" = "Cutpoint difference",
                        "dem_diff*100" = "Difference in Democratic percent",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "rep_pres_percent" = "Trump 2020 pct.",
                        "`Ethnicities.Black Alone`" = "Black pct.",
                        "`2020`" = "2020",
                        "`Ethnicities.Hispanic or Latino`" = "Hispanic pct.",
                        "`Income.Median Houseold Income`" = "Median income",
                        "`Education.Bachelor's Degree or Higher`" = "College pct.",
                        "state_blackPct" = "Black pct.",
                        "state_hispPct" = "Hispanic pct.",
                        "state_medianIncome" = "Median income",
                        "state_baPct" = "College pct.",
                        "state" = "State"),
               notes = "\\hspace*{0.09 in} Median income in \\$100,000. Weighted by number of votes. Standard errors for county results clustered by state.",
               headers = list(":_:" = list("County level" = 3, "State level" = 3)))


######################
## Table D3: Pres v Gov
######################
fixest::etable(list(county_res_pg$res_no_covar,
                    county_res_pg$res_inc_covar, 
                    county_res_pg$res_all_covar,
                    state_res_pg $res_no_covar,
                    state_res_pg $res_inc_covar,
                    state_res_pg $res_all_covar), 
               tex = TRUE, digits = 2,  depvar = FALSE, 
               title = "Biden 2020 margin relative to gubernatorial Dem. margin, 2020 & 2022", 
               label = "tab:pg202022", 
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               order = "!Constant",
               fontsize = "footnotesize",  
               style.tex = sty,
               dict = c("cutpoint_diff" = "Cutpoint difference",
                        "dem_diff*100" = "Difference in Democratic percent",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "`Ethnicities.Black Alone`" = "Black pct.",
                        "`2020`" = "2020",
                        "`Ethnicities.Hispanic or Latino`" = "Hispanic pct.",
                        "`Income.Median Houseold Income`" = "Median income",
                        "`Education.Bachelor's Degree or Higher`" = "College pct.",
                        "county_diff*100" = "Diff. Dem. vote share",
                        "state_blackPct" = "Black pct.",
                        "state_hispPct" = "Hispanic pct.",
                        "state_medianIncome" = "Median income",
                        "state_baPct" = "College pct.",
                        "state" = "State"),
               notes = "\\hspace*{0.09 in} Median income in \\$100,000. Weighted by number of votes. Standard errors for county results clustered by state.",
               headers = list(":_:" = list("County level" = 3, "State level" = 3)))

######################
## Table E1: House v Pres 2020, cf scores
######################
r.cf     = kappa_eachYear_House_FUNCTION(2020, measure = "cf",     compare = "Pres")
r.dwdime = kappa_eachYear_House_FUNCTION(2020, measure = "dwdime", compare = "Pres")

# Latex output
fixest::etable(list(r.cf$all_covar,   r.dwdime$all_covar,
                    r.cf$compet_covar, r.dwdime$compet_covar),
               tex=TRUE, digits = 2,  depvar = FALSE, 
               fontsize = "footnotesize",  
               title = "House Dem. margin relative to Biden margin using donor-based scores, 2020", label = "tab:hp2020donor",
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               order = "!Constant",
               style.tex = sty,
               dict = c("kappaDiff_HousePres" = "Cutpoint difference",
                        "trump2020Pct2Party" = "Trump 2020 pct.",
                        "dem_inc" = "Dem. House inc.",
                        "rep_inc" = "Rep. House inc.",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "blackPct" = "Black pct.",
                        "hispPct" = "Hispanic pct.",
                        "medianIncome" = "Median income",
                        "baPct" = "College pct.",
                        "nonCollWhitePct" = "Non-college White pct.",
                        "dem_receipt" = "Dem. House receipts",
                        "rep_receipt" = "Rep. House receipts",
                        "demDiff_HousePres" = "Difference in Democratic percent",
                        "state" = "State"),
               notes = "\\hspace*{0.7 in} Median income in \\$100,000. Columns (1) and (3) are CF scores; (2) and (4) are DW-Dime scores.",
               headers = list(":_:" = list("Contested" = 2, "Competitive" = 2)))

######################
## Table E2: House v Pres 2022, cf scores
######################
r.cf     = kappa_eachYear_House_FUNCTION(2022, measure = "cf",     compare = "Pres")
r.dwdime = kappa_eachYear_House_FUNCTION(2022, measure = "dwdime", compare = "Pres")

# Latex output
fixest::etable(list(r.cf$all_covar,   r.dwdime$all_covar,
                    r.cf$compet_covar, r.dwdime$compet_covar),
               tex=TRUE, digits = 2,  depvar = FALSE, 
               fontsize = "footnotesize",  
               title = "House Dem. margin relative to Biden margin using donor-based scores, 2022", label = "tab:hp2022donor",  
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               order = "!Constant",
               style.tex = sty,
               dict = c("kappaDiff_HousePres" = "Cutpoint difference",
                        "trump2020Pct2Party" = "Trump 2020 pct.",
                        "dem_inc" = "Dem. House inc.",
                        "rep_inc" = "Rep. House inc.",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "blackPct" = "Black pct.",
                        "hispPct" = "Hispanic pct.",
                        "medianIncome" = "Median income",
                        "baPct" = "College pct.",
                        "nonCollWhitePct" = "Non-college White pct.",
                        "dem_receipt" = "Dem. House receipts",
                        "rep_receipt" = "Rep. House receipts",
                        "demDiff_HousePres" = "Difference in Democratic percent",
                        "state" = "State"),
               notes = "\\hspace*{0.7 in} Median income in \\$100,000. Columns (1) and (3) are CF scores; (2) and (4) are DW-Dime scores.",
               headers = list(":_:" = list("Contested" = 2, "Competitive" = 2)))


######################
## Table E3: House v Senate 2020, cf scores
######################
# Run statistical models in results_figure_FUNCTION above
r.cf =     kappa_eachYear_House_FUNCTION(2020, compare = "Sen", measure = "cf")
r.dwdime = kappa_eachYear_House_FUNCTION(2020, compare = "Sen", measure = "dwdime")

# Latex output
fixest::etable(list(r.cf$all_covar,   r.dwdime$all_covar,
                    r.cf$compet_covar, r.dwdime$compet_covar),
               tex=TRUE, digits = 2,  depvar = FALSE, 
               fontsize = "footnotesize",  
               title = "House Dem. margin relative to Senate Dem. margin using donor-based scores, 2020", 
               label = "tab:hs2020donor",  
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               order = "!Constant",
               style.tex = sty,
               dict = c("kappaDiff_HouseSen" = "Cutpoint difference",
                        "demDiff_HouseSen" = "Difference in Democratic percent",
                        "trump2016Pct2Party" = "Trump 2016 pct.",
                        "trump2020Pct2Party" = "Trump 2020 pct.",
                        "dem_house_inc" = "Dem. House inc.",
                        "rep_house_inc" = "Rep. House inc.",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "blackPct" = "Black pct.",
                        "hispPct" = "Hispanic pct.",
                        "medianIncome" = "Median income",
                        "baPct" = "College pct.",
                        "nonCollWhitePct" = "Non-college White pct.",
                        "dem_receipt" = "Dem. House receipts",
                        "rep_receipt" = "Rep. House receipts",
                        "state" = "State"),
               notes = "\\hspace*{0.95 in} Median income in \\$100,000. Columns (1) and (3) are CF scores; (2) and (4) are DW-Dime scores.",
               headers = list(":_:" = list("Contested" = 2, "Competitive" = 2)))


######################
## Table E4: Sen v Pres 2020, cf scores
######################

# Senate-President comparisons
county_sp.cf     = kappa_bothYears_SenPresGov_FUNCTION(geography = "County",  measure = "cf", compare = "sp")
county_sp.dwdime = kappa_bothYears_SenPresGov_FUNCTION(geography = "County", measure = "dwdime", compare = "sp")
state_sp.cf     = kappa_bothYears_SenPresGov_FUNCTION(geography = "State",  measure = "cf", compare = "sp")
state_sp.dwdime = kappa_bothYears_SenPresGov_FUNCTION(geography = "State", measure = "dwdime", compare = "sp")

fixest::etable(list(county_sp.cf$res_all_covar,
                    county_sp.dwdime$res_all_covar,
                    state_sp.cf$res_all_covar,
                    state_sp.dwdime$res_all_covar), 
               tex = TRUE, digits = 2,  depvar = FALSE, 
               fontsize = "footnotesize",  
               title = "Senate Dem. margin relative to Biden 2020 margin using donor-based scores",
               label = "tab:sp202022donor",
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               order = "!Constant",
               style.tex = sty,
               dict = c("cutpoint_diff" = "Cutpoint difference",
                        "dem_diff*100" = "Difference in Democratic percent",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "`2020`" = "2020",
                        "`Ethnicities.Black Alone`" = "Black pct.",
                        "`Ethnicities.Hispanic or Latino`" = "Hispanic pct.",
                        "`Income.Median Houseold Income`" = "Median income",
                        "`Education.Bachelor's Degree or Higher`" = "College pct.",
                        "state_blackPct" = "Black pct.",
                        "state_hispPct" = "Hispanic pct.",
                        "state_medianIncome" = "Median income",
                        "state_baPct" = "College pct.",
                        "state" = "State"),
               notes = "\\hspace*{0.8 in} Median income in \\$100,000. Weighted by number of votes. Standard errors for county results clustered by state. Columns (1) and (3) are CF scores; (2) and (4) are DW-Dime scores.",
               headers = list(":_:" = list("County level" = 2, "State level" = 2)))


######################
## Table F1:  No weights
######################

# Senate-President comparisons
state_sp_noWt  = kappa_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "sp", weights=0)

# Senate-Governor comparisons
state_sg_noWt  = kappa_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "sg", weights=0)

# President - Governor comparisons
state_pg_noWt  = kappa_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "pg", weights=0)

fixest::etable(list(state_sp_noWt$res_inc_covar,
                    state_sg_noWt$res_inc_covar,
                    state_pg_noWt$res_inc_covar), 
               tex = TRUE, digits = 2,  depvar = FALSE,  
               fontsize = "footnotesize",  
               title = "State level models with no weights",
               label = "tab:stateNoWeight",  
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               order = c("!Constant", "!2020"), #"!Constant"),
               style.tex = sty,
               dict = c("cutpoint_diff" = "Cutpoint difference",
                        "dem_diff*100" = "Difference in Democratic percent",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "rep_pres_percent" = "Trump pct.",
                        "`2020`" = "2020",
                        "`Ethnicities.Black Alone`" = "Black pct.",
                        "`Ethnicities.Hispanic or Latino`" = "Hispanic pct.",
                        "`Income.Median Houseold Income`" = "Median income",
                        "`Education.Bachelor's Degree or Higher`" = "College pct.",
                        "state_blackPct" = "Black pct.",
                        "state_hispPct" = "Hispanic pct.",
                        "state_medianIncome" = "Median income",
                        "state_baPct" = "College pct.",
                        "state" = "State"),
               headers = c("Sen.-Pres","Sen.-Gov.", "Pres.-Gov."), 
               notes = "\\hspace*{1.4 in} Median income in \\$100,000.")

######################
## Table G1: With campaign receipts
######################

fixest::etable(list(r_hp_2020$all_covar_money, r_hp_2020$compet_covar_money, 
                    r_hp_2022$all_covar_money, r_hp_2022$compet_covar_money,
                    r_hs_2020$all_covar_money, r_hs_2020$compet_covar_money),
               tex=TRUE, digits = 2,  depvar = FALSE, 
               fontsize = "scriptsize",  
               title = "Differences in Dem. margin, with campaign receipts", 
               label = "tab:campFinance",  
               se.below = F, fitstat=c('n', 'r2'), digits.stats = 'r2',
               order = "!Constant",
               style.tex = sty,
               dict = c("kappaDiff_HousePres" = "Cutpoint difference",
                        "kappaDiff_HouseSen"  = "Cutpoint difference",
                        "demDiff_HouseSen" = "Difference in Democratic percent",
                        "trump2016Pct2Party" = "Trump 2016 pct.",
                        "trump2020Pct2Party" = "Trump 2020 pct.",
                        "dem_inc" = "Dem. House inc.",
                        "rep_inc" = "Rep. House inc.",
                        "dem_house_inc" = "Dem. House inc.",
                        "rep_house_inc" = "Rep. House inc.",
                        "dem_sen_inc" = "Dem. Sen. inc.",
                        "rep_sen_inc" = "Rep. Sen. inc.",
                        "dem_gov_inc" = "Dem. Gov. inc.",
                        "rep_gov_inc" = "Rep. Gov. inc.",
                        "blackPct" = "Black pct.",
                        "hispPct" = "Hispanic pct.",
                        "medianIncome" = "Median income",
                        "baPct" = "College pct.",
                        "nonCollWhitePct" = "Non-college White pct.",
                        "dem_receipt" = "Dem. House receipts",
                        "rep_receipt" = "Rep. House receipts",
                        "log_house_dem_receipt" = "Log Dem. House receipts",
                        "log_house_rep_receipt" = "Log Rep. House receipts",
                        "log_sen_dem_receipt" = "Log Dem. Sen. receipts",
                        "log_sen_rep_receipt" = "Log Rep. Sen. receipts",
                        "house_demMoneyAdv" = "House Dem. receipt pct.",
                        "demDiff_HousePres" = "Difference in Democratic percent",
                        "state" = "State"),
               notes = "\\hspace*{0.09 in} Median income in \\$100,000",
               headers = list(":_:" = list("House-Pres. 2020" = 2, "House-Pres. 2022" = 2,"House-Sen. 2020" = 2)))

######################
## Table H1: 
######################
# In House-Senate comparison, find number of races with both Dem incumbents, both R incumbents etc
all = r_hs_2020$mdiHouseAll %>% filter(!is.na(kappaDiff_HouseSen))

# Create data frame to store counts for all races
incumbentN = data.frame("senDemInc" = rep(NA, 3), "senRepInc" = rep(NA, 3), "senOpen" = rep(NA, 3))
row.names(incumbentN) = c("houseDemInc", "houseRepInc", "houseOpen")

incumbentN["houseDemInc", "senDemInc"] = length(all$stateDist[all$dem_house_inc==1 & all$dem_sen_inc==1])
incumbentN["houseDemInc", "senRepInc"] = length(all$stateDist[all$dem_house_inc==1 & all$rep_sen_inc==1])
incumbentN["houseDemInc", "senOpen"]   = length(all$stateDist[all$dem_house_inc==1  
                                                              & all$dem_sen_inc==0 & all$rep_sen_inc==0])
incumbentN["houseRepInc", "senDemInc"] = length(all$stateDist[all$rep_house_inc==1 & all$dem_sen_inc==1])
incumbentN["houseRepInc", "senRepInc"] = length(all$stateDist[all$rep_house_inc==1 & all$rep_sen_inc==1])
incumbentN["houseRepInc", "senOpen"]   = length(all$stateDist[all$rep_house_inc==1  
                                                              & all$dem_sen_inc==0 & all$rep_sen_inc==0])
incumbentN["houseOpen", "senDemInc"] = length(all$stateDist[all$dem_house_inc==0 & all$rep_house_inc==0 & all$dem_sen_inc==1])
incumbentN["houseOpen", "senRepInc"] = length(all$stateDist[all$dem_house_inc==0 & all$rep_house_inc==0 & all$rep_sen_inc==1])
incumbentN["houseOpen", "senOpen"] = length(all$stateDist[all$dem_house_inc==0 & all$rep_house_inc==0 
                                                          & all$dem_sen_inc==0 & all$rep_sen_inc==0])

## Output for Table
incumbentN


######################
## Table H2: 
######################
# HouseSen specifications on subsets based on whether both incumbents are Dems etc
all = r_hs_2020$mdiHouseAll %>% filter(!is.na(kappaDiff_HouseSen)) 
formula_no_inc   <- as.formula("demDiff_HouseSen ~ kappaDiff_HouseSen")
formula_inc   <- as.formula("demDiff_HouseSen ~ kappaDiff_HouseSen + dem_house_inc + dem_sen_inc + rep_house_inc  + rep_sen_inc")

all_dd    <- fixest::feols(formula_no_inc,    
                           data = all %>% filter(dem_house_inc==1 & dem_sen_inc==1))
summary(all_dd)


all_rr    <- fixest::feols(formula_no_inc,    
                           data = all %>%  filter(rep_house_inc==1 & rep_sen_inc==1))
summary(all_rr)

all_dr    <- fixest::feols(formula_no_inc,    
                           data = all %>%  filter(dem_house_inc==1 & rep_sen_inc==1))
summary(all_dr)

all_r_notr    <- fixest::feols(formula_inc,    
                               data = all %>%  filter(rep_house_inc==1 & rep_sen_inc!=1))
summary(all_r_notr)


all_notd_d    <- fixest::feols(formula_inc,    
                               data = all %>% filter(dem_house_inc!=1 & dem_sen_inc==1))
summary(all_notd_d)

all_notr_r    <- fixest::feols(formula_inc,    
                               data = all %>%  filter(rep_house_inc!=1 & rep_sen_inc==1))
summary(all_notr_r)

## Storage table
subsets = data.frame("Subset" = c("House D, Sen. D",     "House R, Sen. R",     "House D, Sen. R", 
                                  "House R, Sen. not R", "House not D, Sen. D", "House not R, Sen. R"), 
                     "N" = NA, "Coef" = NA, "t-stat"=NA)
subsets[1, 2:4]=c(summary(all_dd)$nobs, round(summary(all_dd)$coeftable[2, c(1, 3)], 2))
subsets[2, 2:4]=c(summary(all_rr)$nobs, round(summary(all_rr)$coeftable[2, c(1, 3)], 2))
subsets[3, 2:4]=c(summary(all_dr)$nobs, round(summary(all_dr)$coeftable[2, c(1, 3)], 2))
subsets[4, 2:4]=c(summary(all_r_notr)$nobs, round(summary(all_r_notr)$coeftable[2, c(1, 3)], 2))
subsets[5, 2:4]=c(summary(all_notd_d)$nobs, round(summary(all_notd_d)$coeftable[2, c(1, 3)], 2))
subsets[6, 2:4]=c(summary(all_notr_r)$nobs, round(summary(all_notr_r)$coeftable[2, c(1, 3)], 2))

## Output for Table
subsets

