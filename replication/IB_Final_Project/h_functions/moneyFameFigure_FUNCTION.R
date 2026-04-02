## Results figure for money and fame

# Run statistical models for each year
year = 2020
depVar = "small_donations" # "total_donations" "small_donations" "followers"

## Prototype figure
moneyFameFigure_FUNCTION = function(year, depVar){

plot(c(-6, 7), c(0.7, 2.2), type="n", lty=2, xlab="", ylab="", xaxt='n',
     cex.axis=0.9, yaxt='n')
title(year, line = 0.1, cex.main =0.9)
axis(1, at=seq(-10, 30, by = 2),	labels=seq(-10, 30, by = 2), 
     cex.axis=0.7, padj = -1.6)
lines(c(0, 0), c(0, 2.3), col = "grey")  
if(year ==2020) {
  axis(2, at = seq(1, 2, by = 1), 
     label = c("Rep.", "Dem."), 
     tick = F, las = 1, cex.axis = 0.9, mgp = c(3, 0.2, 0)) }

if(depVar == "total_donations" | depVar == "small_donations"){
  for(rr in 1:2){
    text(-5.8, rr + 0.1, "All", cex = 0.7, col = "grey")
    text(-5.1, rr - 0.1, "Compet.", cex = 0.7, col = "grey") } }
if(depVar == "followers"){
  for(rr in 1:2){
    text(-5.8, rr + 0.1, "All", cex = 0.7, col = "grey")
    text(-5.5, rr - 0.1, "Compet.", cex = 0.7, col = "grey") } }

if(year == 2020 & depVar == "total_donations") res.tmp = total_2020
if(year == 2022 & depVar == "total_donations") res.tmp = total_2022
if(year == 2020 & depVar == "small_donations") res.tmp = small_2020
if(year == 2022 & depVar == "small_donations") res.tmp = small_2022
if(year == 2020 & depVar == "followers") res.tmp = follow_2020
if(depVar == "total_donations") title_term = "total donations"
if(depVar == "small_donations") title_term = "small donations"
if(depVar == "followers")       title_term = "followers"

# if(year ==2020) r.tmp = total_2020
# if(year ==2022) r.tmp = total_2022

# Democrats on top
# All districts -standard error
lines(c(res.tmp$dem_covar$coeftable["dem_avgIdeal", 1] - 1.96*res.tmp$dem_covar$coeftable["dem_avgIdeal", 2],
        res.tmp$dem_covar$coeftable["dem_avgIdeal", 1] + 1.96*res.tmp$dem_covar$coeftable["dem_avgIdeal", 2]), 
      c(2.1, 2.1), lwd = 2, 
      col = "darkgrey")
# Coefficient
lines(rep(res.tmp$dem_covar$coeftable["dem_avgIdeal", 1], 2), 
      c(2.05, 2.15), lwd = 3, 
      col = "black")


# Competitive districts
# Standard error
lines(c(res.tmp$dem_covar_compet$coeftable["dem_avgIdeal", 1] - 1.96*res.tmp$dem_covar_compet$coeftable["dem_avgIdeal", 2],
        res.tmp$dem_covar_compet$coeftable["dem_avgIdeal", 1] + 1.96*res.tmp$dem_covar_compet$coeftable["dem_avgIdeal", 2]), 
      c(1.9, 1.9), lwd = 2, 
      col = "darkgrey")
# Coefficient
lines(rep(res.tmp$dem_covar_compet$coeftable["dem_avgIdeal", 1], 2), 
      c(1.85, 1.95), lwd = 3, 
      col = "black")

# Republicans on bottom
  # All districts -standard error
lines(c(res.tmp$rep_covar$coeftable["rep_avgIdeal", 1] - 1.96*res.tmp$rep_covar$coeftable["rep_avgIdeal", 2],
        res.tmp$rep_covar$coeftable["rep_avgIdeal", 1] + 1.96*res.tmp$rep_covar$coeftable["rep_avgIdeal", 2]),
          c(1.1, 1.1), lwd = 2, 
          col = "darkgrey")
    # Coefficient
    lines(rep(res.tmp$rep_covar$coeftable["rep_avgIdeal", 1], 2), 
          c(1.05, 1.15), lwd = 3, 
          col = "black")
    # Competitive districts
    # Standard error
    lines(c(res.tmp$rep_covar_compet$coeftable["rep_avgIdeal", 1] - 1.96*res.tmp$rep_covar_compet$coeftable["rep_avgIdeal", 2],
            res.tmp$rep_covar_compet$coeftable["rep_avgIdeal", 1] + 1.96*res.tmp$rep_covar_compet$coeftable["rep_avgIdeal", 2]),
          c(0.9, 0.9), lwd = 2, 
          col = "darkgrey")
    # Coefficient
    lines(rep(res.tmp$rep_covar_compet$coeftable["rep_avgIdeal", 1], 2), 
          c(0.85, 0.95), lwd = 3, 
          col = "black")

    }