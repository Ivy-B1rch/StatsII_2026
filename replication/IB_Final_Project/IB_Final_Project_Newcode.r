
library(tidyverse)
library(estimatr)
library(tinytex)
library(gridExtra)
library(readxl)
library(scales)
library(colorspace)
library(kableExtra)
library(haven)
library(fixest)

## Load functions
sapply(paste0("h_functions/", list.files("h_functions")), source)


r_hp_2020 = kappa_extremist_eachYear_House_FUNCTION(2020, compare = "Pres", cluster=1)
r_hp_2022 = kappa_extremist_eachYear_House_FUNCTION(2022, compare = "Pres", cluster=1)
r_hs_2020 = kappa_extremist_eachYear_House_FUNCTION(2020, compare = "Sen",  cluster=1)

yearList = c(2020, 2022, 2020, 2022)


par(mfrow=c(2, 2), 
    mar=c(1.0, 2.0, 0.75,  0.2), 
    oma=c(1.1, 1.8, 0.5,  0.2))	 	# mar(south, west, north, east)
for(plotCount in 1:4){
  if(plotCount==4){
    plot(c(-0.6, 0.3), c(-10, 8), 
         xlab = "", ylab = "", 
         type="n", cex.axis=0.9, 
         xaxt="n", yaxt="n", ylim = c(-11.4, 8.4))
    axis(1, tick = T, tck = -0.03, 
         at = seq(-1, 1, by=0.25), 
         labels = seq(-1, 1, by=0.25),  
         cex.axis = 0.7, mgp = c(2, 0.0, 0))
    axis(2, las = 1, tick = T, tck = -0.03, 
         cex.axis = 0.7, 
         mgp = c(2, 0.25, 0), 
         at = seq(-10, 10, by=2), 
         labels = paste0(seq(-10, 10, by=2), "%")) 
    text(-0.14, -1, "Data unavailable", col = "darkgrey")}
  
  if(plotCount!=4){
    if(plotCount == 1) r.tmp = r_hp_2020
    if(plotCount == 2) r.tmp = r_hp_2022
    if(plotCount == 3) r.tmp = r_hs_2020
    plot(r.tmp$mdiHouseAll$demDiff_HousePres ~r.tmp$mdiHouseAll$kappaDiff_HousePres, 
         xlab = "", ylab = "",
         type="n", cex.axis=0.9, 
         xaxt="n", yaxt="n", ylim = c(-11.4, 8.4))  #xlim = c(-0.5, 0.75)
    
    abline(r.tmp$all_no_covar,    col="darkgrey", lwd = 2)
    abline(r.tmp$compet_no_covar, col="#3c78c3",  lwd = 3)
    points(r.tmp$mdiHouseAll$demDiff_HousePres ~ r.tmp$mdiHouseAll$kappaDiff_HousePres, 
           pch=16, cex = 0.8, col="darkgrey")
    points(r.tmp$mdiHouseCompet$demDiff_HousePres ~ r.tmp$mdiHouseCompet$kappaDiff_HousePres,
           pch=16, cex = 1.0, col="#3c78c3")
    
    if(plotCount< 3) {
      title(yearList[plotCount], line = 0.19, cex.main =0.9)}
    
    if(plotCount ==3){
      mtext("House\nDem.\nmargin\nover\nSenate\nDem.", side = 2, 
            las = 1, at = 0, line = 1.2, cex = 0.75)
      mtext("Cutpoint difference in Extreme Races", side = 1, 
            las = 1, 
            at = 0.72, 
            line = 1.1, cex = 1)}
    # KEY
    if(plotCount ==1){
      points(0.35, -10.0, pch=16, cex = 1.0, col="#3c78c3")
      text  (0.48, -10.0, "Competitive", col="#3c78c3", cex = 0.7)
      points(0.35, -11.2, pch=16, cex = 0.8, col="darkgrey")
      text  (0.51, -11.2, "Not competitive", col="darkgrey", cex = 0.7) }
    if(plotCount ==2){
      points(-0.60, 8.3, pch=16, cex = 1.0, col="#3c78c3")
      text  (-0.49, 8.3, "Competitive", col="#3c78c3", cex = 0.7)
      points(-0.60, 7, pch=16, cex = 0.8, col="darkgrey")
      text  (-0.46, 7, "Not competitive", col="darkgrey", cex = 0.7) }
    if(plotCount ==3){
      points(0.35, -10.0, pch=16, cex = 1.0, col="#3c78c3")
      text  (0.47, -10.0, "Competitive", col="#3c78c3", cex = 0.7)
      points(0.35, -11.2, pch=16, cex = 0.8, col="darkgrey")
      text  (0.50, -11.2, "Not competitive", col="darkgrey", cex = 0.7) }
    
    axis(1, tick = T, tck = -0.03, 
         at = seq(-1, 1, by=0.25), 
         labels = seq(-1, 1, by=0.25),  
         cex.axis = 0.5, mgp = c(2, 0.0, 0))
    axis(2, las = 1, tick = T, tck = -0.03, 
         cex.axis = 0.5, 
         mgp = c(2, 0.25, 0), 
         at = seq(-10, 10, by=2), 
         labels = paste0(seq(-10, 10, by=2), "%")) 
    if(plotCount == 1){
      mtext("House\nDem.\nmargin\nover\nBiden\n2020", side = 2, 
            las = 1, at = 0, line = 1.2, cex = 0.75)}
  } }



#moderate races


r_hp_2020 = kappa_moderate_eachYear_House_FUNCTION(2020, compare = "Pres", cluster=1)
r_hp_2022 = kappa_moderate_eachYear_House_FUNCTION(2022, compare = "Pres", cluster=1)
r_hs_2020 = kappa_moderate_eachYear_House_FUNCTION(2020, compare = "Sen",  cluster=1)

yearList = c(2020, 2022, 2020, 2022)


par(mfrow=c(2, 2), 
    mar=c(1.0, 2.0, 0.75,  0.2), 
    oma=c(1.1, 1.8, 0.5,  0.2))	 	# mar(south, west, north, east)
for(plotCount in 1:4){
  if(plotCount==4){
    plot(c(-0.6, 0.3), c(-10, 8), 
         xlab = "", ylab = "", 
         type="n", cex.axis=0.9, 
         xaxt="n", yaxt="n", ylim = c(-11.4, 8.4))
    axis(1, tick = T, tck = -0.03, 
         at = seq(-1, 1, by=0.25), 
         labels = seq(-1, 1, by=0.25),  
         cex.axis = 0.7, mgp = c(2, 0.0, 0))
    axis(2, las = 1, tick = T, tck = -0.03, 
         cex.axis = 0.7, 
         mgp = c(2, 0.25, 0), 
         at = seq(-10, 10, by=2), 
         labels = paste0(seq(-10, 10, by=2), "%")) 
    text(-0.14, -1, "Data unavailable", col = "darkgrey")}
  
  if(plotCount!=4){
    if(plotCount == 1) r.tmp = r_hp_2020
    if(plotCount == 2) r.tmp = r_hp_2022
    if(plotCount == 3) r.tmp = r_hs_2020
    plot(r.tmp$mdiHouseAll$demDiff_HousePres ~r.tmp$mdiHouseAll$kappaDiff_HousePres, 
         xlab = "", ylab = "",
         type="n", cex.axis=0.9, 
         xaxt="n", yaxt="n", ylim = c(-11.4, 8.4))  #xlim = c(-0.5, 0.75)
    
    abline(r.tmp$all_no_covar,    col="darkgrey", lwd = 2)
    abline(r.tmp$compet_no_covar, col="#3c78c3",  lwd = 3)
    points(r.tmp$mdiHouseAll$demDiff_HousePres ~ r.tmp$mdiHouseAll$kappaDiff_HousePres, 
           pch=16, cex = 0.8, col="darkgrey")
    points(r.tmp$mdiHouseCompet$demDiff_HousePres ~ r.tmp$mdiHouseCompet$kappaDiff_HousePres,
           pch=16, cex = 1.0, col="#3c78c3")
    
    if(plotCount< 3) {
      title(yearList[plotCount], line = 0.19, cex.main =0.9)}
    
    if(plotCount ==3){
      mtext("House\nDem.\nmargin\nover\nSenate\nDem.", side = 2, 
            las = 1, at = 0, line = 1.2, cex = 0.75)
      mtext("Cutpoint difference in Moderate Races", side = 1, 
            las = 1, 
            at = 0.72, 
            line = 1.1, cex = 1)}
    # KEY
    if(plotCount ==1){
      points(0.35, -10.0, pch=16, cex = 1.0, col="#3c78c3")
      text  (0.48, -10.0, "Competitive", col="#3c78c3", cex = 0.7)
      points(0.35, -11.2, pch=16, cex = 0.8, col="darkgrey")
      text  (0.51, -11.2, "Not competitive", col="darkgrey", cex = 0.7) }
    if(plotCount ==2){
      points(-0.60, 8.3, pch=16, cex = 1.0, col="#3c78c3")
      text  (-0.49, 8.3, "Competitive", col="#3c78c3", cex = 0.7)
      points(-0.60, 7, pch=16, cex = 0.8, col="darkgrey")
      text  (-0.46, 7, "Not competitive", col="darkgrey", cex = 0.7) }
    if(plotCount ==3){
      points(0.35, -10.0, pch=16, cex = 1.0, col="#3c78c3")
      text  (0.47, -10.0, "Competitive", col="#3c78c3", cex = 0.7)
      points(0.35, -11.2, pch=16, cex = 0.8, col="darkgrey")
      text  (0.50, -11.2, "Not competitive", col="darkgrey", cex = 0.7) }
    
    axis(1, tick = T, tck = -0.03, 
         at = seq(-1, 1, by=0.25), 
         labels = seq(-1, 1, by=0.25),  
         cex.axis = 0.5, mgp = c(2, 0.0, 0))
    axis(2, las = 1, tick = T, tck = -0.03, 
         cex.axis = 0.5, 
         mgp = c(2, 0.25, 0), 
         at = seq(-10, 10, by=2), 
         labels = paste0(seq(-10, 10, by=2), "%")) 
    if(plotCount == 1){
      mtext("House\nDem.\nmargin\nover\nBiden\n2020", side = 2, 
            las = 1, at = 0, line = 1.2, cex = 0.75)}
  } }



### next figure
# Senate-President comparisons
county_res_sp = kappa_extremist_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "sp")
state_res_sp  = kappa_extremist_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "sp")

# Senate-Governor comparisons
county_res_sg = kappa_extremist_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "sg")
state_res_sg  = kappa_extremist_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "sg")

# President - Governor comparisons
county_res_pg = kappa_extremist_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "pg")
state_res_pg  = kappa_extremist_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "pg")

percentile <- function(x) round((rank(x) - 1)/(length(x) - 1), 2)
par(mfrow=c(3, 2),
    mar=c(1, 1.2, 1.,0.5),
    oma=c(2.0, 4.0,  1, 0.3))  # mar(south, west, north, east)
for(plotCount in 1:6){
  if(plotCount ==1) {
    tmp = county_res_sp
    yMin = -12
    yMax =  12}
  if(plotCount ==2) {
    tmp = state_res_sp
    yMin = -12
    yMax =  12}
  if(plotCount ==3) {
    tmp = county_res_sg
    yMin = -25
    yMax =  25}
  if(plotCount ==4) {
    tmp = state_res_sg
    yMin = -25
    yMax =  25}
  if(plotCount ==5) {
    tmp = county_res_pg
    yMin = -20
    yMax =  50}
  if(plotCount ==6) {
    tmp = state_res_pg
    yMin = -20
    yMax =  50}
  
  plot(100*tmp$dta$dem_diff ~ tmp$dta$cutpoint_diff, 
       xlab = "", ylab = "",
       type="n", cex.axis=0.9, 
       xaxt="n", yaxt="n", ylim = c(yMin, yMax))  #, ylim = c(-56, 12), xlim = c(-0.5, 0.75)
  #c(min(tmp$dta$dem_diff, na.rm= T), max(tmp$dta$dem_diff, na.rm= T))
  if(plotCount == 1) mtext("Senate\nDem.\nmargin\nover\nBiden", side = 2,
                           las = 1, at = 0, line = 1.6, cex = 0.75)
  if(plotCount == 3) mtext("Senate\nDem.\nmargin\nover\nDem.\nGov.", side = 2, 
                           las = 1, at = 0, line = 1.6, cex = 0.75)
  if(plotCount == 5) mtext("Biden\n2020\nmargin\nover\nDem.\nGov.", side = 2, 
                           las = 1, at = 15, line = 1.6, cex = 0.75)
  if(plotCount==1) title("County", line = 0.3, cex.main =1)
  if(plotCount==2) title("State",  line = 0.3, cex.main =1)
  
  abline(tmp$res_no_covar, col="#3c78c3", lwd = 2)
  if(plotCount %in% c(1,3,5)) points(100*tmp$dta$dem_diff ~ tmp$dta$cutpoint_diff, 
                                     pch=16, cex = percentile(tmp$dta$county_votes)*1.4, col="#3c78c3")
  if(plotCount %in% c(2,4,6)) points(100*tmp$dta$dem_diff ~ tmp$dta$cutpoint_diff, 
                                     pch=16, cex = 1.2, col="#3c78c3")
  axis(1, tick = T, tck = -0.03, 
       at = seq(-1, 1, by=0.25), 
       labels = seq(-1, 1, by=0.25),  
       cex.axis = 0.5, mgp = c(2, 0.0, 0))
  axis(2, las = 1, tick = T, tck = -0.03, 
       cex.axis = 0.5, 
       mgp = c(2, 0.25, 0), 
       at = seq(-70, 70, by=10), 
       labels = paste0(seq(-70, 70, by=10), "%")) 
  if(plotCount> 4) mtext("Cutpoint difference in Extreme Races", side = 1, 
                         las = 1, at = 0.2, line = 1.4, cex = 0.8)
}


##moderate version
county_res_sp = kappa_moderate_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "sp")
state_res_sp  = kappa_moderate_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "sp")

# Senate-Governor comparisons
county_res_sg = kappa_moderate_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "sg")
state_res_sg  = kappa_moderate_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "sg")

# President - Governor comparisons
county_res_pg = kappa_moderate_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "pg")
state_res_pg  = kappa_moderate_bothYears_SenPresGov_FUNCTION(geography = "State",  compare = "pg")

percentile <- function(x) round((rank(x) - 1)/(length(x) - 1), 2)
par(mfrow=c(3, 2),
    mar=c(1, 1.2, 1.,0.5),
    oma=c(2.0, 4.0,  1, 0.3))  # mar(south, west, north, east)
for(plotCount in 1:6){
  if(plotCount ==1) {
    tmp = county_res_sp
    yMin = -12
    yMax =  12}
  if(plotCount ==2) {
    tmp = state_res_sp
    yMin = -12
    yMax =  12}
  if(plotCount ==3) {
    tmp = county_res_sg
    yMin = -25
    yMax =  25}
  if(plotCount ==4) {
    tmp = state_res_sg
    yMin = -25
    yMax =  25}
  if(plotCount ==5) {
    tmp = county_res_pg
    yMin = -20
    yMax =  50}
  if(plotCount ==6) {
    tmp = state_res_pg
    yMin = -20
    yMax =  50}
  
  plot(100*tmp$dta$dem_diff ~ tmp$dta$cutpoint_diff, 
       xlab = "", ylab = "",
       type="n", cex.axis=0.9, 
       xaxt="n", yaxt="n", ylim = c(yMin, yMax))  #, ylim = c(-56, 12), xlim = c(-0.5, 0.75)
  #c(min(tmp$dta$dem_diff, na.rm= T), max(tmp$dta$dem_diff, na.rm= T))
  if(plotCount == 1) mtext("Senate\nDem.\nmargin\nover\nBiden", side = 2,
                           las = 1, at = 0, line = 1.6, cex = 0.75)
  if(plotCount == 3) mtext("Senate\nDem.\nmargin\nover\nDem.\nGov.", side = 2, 
                           las = 1, at = 0, line = 1.6, cex = 0.75)
  if(plotCount == 5) mtext("Biden\n2020\nmargin\nover\nDem.\nGov.", side = 2, 
                           las = 1, at = 15, line = 1.6, cex = 0.75)
  if(plotCount==1) title("County", line = 0.3, cex.main =1)
  if(plotCount==2) title("State",  line = 0.3, cex.main =1)
  
  abline(tmp$res_no_covar, col="#3c78c3", lwd = 2)
  if(plotCount %in% c(1,3,5)) points(100*tmp$dta$dem_diff ~ tmp$dta$cutpoint_diff, 
                                     pch=16, cex = percentile(tmp$dta$county_votes)*1.4, col="#3c78c3")
  if(plotCount %in% c(2,4,6)) points(100*tmp$dta$dem_diff ~ tmp$dta$cutpoint_diff, 
                                     pch=16, cex = 1.2, col="#3c78c3")
  axis(1, tick = T, tck = -0.03, 
       at = seq(-1, 1, by=0.25), 
       labels = seq(-1, 1, by=0.25),  
       cex.axis = 0.5, mgp = c(2, 0.0, 0))
  axis(2, las = 1, tick = T, tck = -0.03, 
       cex.axis = 0.5, 
       mgp = c(2, 0.25, 0), 
       at = seq(-70, 70, by=10), 
       labels = paste0(seq(-70, 70, by=10), "%")) 
  if(plotCount> 4) mtext("Cutpoint difference in Moderate Races", side = 1, 
                         las = 1, at = 0.2, line = 1.4, cex = 0.8)
}





