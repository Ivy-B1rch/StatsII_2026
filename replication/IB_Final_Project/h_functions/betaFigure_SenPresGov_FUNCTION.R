## Results figure for senate, president, gov comparisons at county and state level
# geography = "County"
# compare = "sp"
# res_sp = kappa_bothYears_SenPresGov_FUNCTION(geography = "County", compare = "sp")
# res_sp = kappa_bothYears_SenPresGov_FUNCTION(geography = "State", compare = "sp")

## Figure function
betaFigure_SenPresGov_FUNCTION = function(geography = "County"){

plot(c(-5, 27), c(0.7, 3.3), type="n", lty=2, xlab="", ylab="", xaxt='n',
       cex.axis=0.9, yaxt='n') #, main = geography
#  mtext("Effect of ideology", side = 1, at = 25.0, line = 1.9, cex = 1)
  axis(1, at=seq(-10, 70, by = 5),	labels=seq(-10, 70, by = 5), 
       cex.axis=0.7, padj = -1.6)
  lines(c(0, 0), c(0, 3.2), col = "grey")  
  axis(2, at = seq(1, 3, by = 1), 
                       label = c("President -\nGovernor", "Senate -\nGovernor", "Senate -\nPresident"), 
                       tick = F, las = 1, cex.axis = 0.9, mgp = c(3, 0.2, 0))
  for(rr in 1:3){
    text(-4.4, rr + 0.1, "County", cex = 0.7, col = "grey")
    text(-4.2, rr - 0.1, "State", cex = 0.7, col = "grey")
  }
  
  # Sen-Pres is on top
  # Standard error
  lines(c(county_res_sp$beta_all_covar - 1.96*county_res_sp$se_all_covar,
          county_res_sp$beta_all_covar + 1.96*county_res_sp$se_all_covar), 
        rep(3.1, 2), lwd = 2, 
        col = "darkgrey")
  lines(c(state_res_sp$beta_all_covar - 1.96*state_res_sp$se_all_covar,
          state_res_sp$beta_all_covar + 1.96*state_res_sp$se_all_covar), 
        rep(2.9, 2), lwd = 2, 
        col = "darkgrey")
  
  # Coefficient
  lines(rep(county_res_sp$beta_all_covar, 2), 
        c(3.05, 3.15), lwd = 3, 
        col = "black")
  lines(rep(state_res_sp$beta_all_covar, 2), 
        c(2.85, 2.95), lwd = 3, 
        col = "black")
  
  # Sen-Gov is in middle
  # Standard error
  lines(c(county_res_sg$beta_all_covar - 1.96*county_res_sg$se_all_covar,
          county_res_sg$beta_all_covar + 1.96*county_res_sg$se_all_covar), 
        rep(2.1, 2), lwd = 2, 
        col = "darkgrey")
  lines(c(state_res_sg$beta_all_covar - 1.96*state_res_sg$se_all_covar,
          state_res_sg$beta_all_covar + 1.96*state_res_sg$se_all_covar), 
        rep(1.9, 2), lwd = 2, 
        col = "darkgrey")
  
  # Coefficient
  lines(rep(county_res_sg$beta_all_covar, 2), 
        c(2.05, 2.15), lwd = 3, 
        col = "black")
  lines(rep(state_res_sg$beta_all_covar, 2), 
        c(1.85, 1.95), lwd = 3, 
        col = "black")
  

  # Pres-Gov is on bottom
  # Standard error
  lines(c(county_res_pg$beta_all_covar - 1.96*county_res_pg$se_all_covar,
          county_res_pg$beta_all_covar + 1.96*county_res_pg$se_all_covar), 
        rep(1.1, 2), lwd = 2, 
        col = "darkgrey")
  lines(c(state_res_pg$beta_all_covar - 1.96*state_res_pg$se_all_covar,
          state_res_pg$beta_all_covar + 1.96*state_res_pg$se_all_covar), 
        rep(0.9, 2), lwd = 2, 
        col = "darkgrey")
  
  # Coefficient
  lines(rep(county_res_pg$beta_all_covar, 2), 
        c(1.05, 1.15), lwd = 3, 
        col = "black")
  lines(rep(state_res_pg$beta_all_covar, 2), 
        c(0.85, 0.95), lwd = 3, 
        col = "black")
  }
