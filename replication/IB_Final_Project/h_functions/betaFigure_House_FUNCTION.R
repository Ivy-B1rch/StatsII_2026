## Results figure
#year = 2022

# Run statistical models for each year
  # r_hp_2020 = kappa_eachYear_House_FUNCTION(2020, compare = "Pres")
  # r_hp_2022 = kappa_eachYear_House_FUNCTION(2022, compare = "Pres")
  # r_hs_2020 = kappa_eachYear_House_FUNCTION(2020, compare = "Senate")


## Prototype figure
betaFigure_House_FUNCTION = function(year){
  plot(c(-5.5, 15), c(0.7, 2.2), type="n", lty=2, xlab="", ylab="", xaxt='n',
       cex.axis=0.9, yaxt='n')
  title(year, line = 0.1, cex.main =0.9)
  axis(1, at=seq(-10, 30, by = 5),	labels=seq(-10, 30, by = 5), 
       cex.axis=0.7, padj = -1.6)
  lines(c(0, 0), c(0, 2.22), col = "grey")  
  if(year ==2020) axis(2, at = c(1, 1.8), 
                       label = c("House - \nSenate", "House - \nPresident"), 
                       tick = F, las = 1, cex.axis = 0.9, mgp = c(3, 0.2, 0))
  text(-4.6, 2.0, "All", cex = 0.7, col = "grey")
  text(-3.3, 1.8, "Compet.", cex = 0.7, col = "grey")
  text(-2.8, 1.63, "Fully", cex = 0.7, col = "grey")
  text(-2.8, 1.57, "funded", cex = 0.7, col = "grey")
  if(year ==2020) {
    text(-4.6, 1.2, "All", cex = 0.7, col = "grey")
    text(-3.3, 1.0, "Compet.", cex = 0.7, col = "grey")
    text(-2.8, 0.83, "Fully", cex = 0.7, col = "grey") 
    text(-2.8, 0.77, "funded", cex = 0.7, col = "grey") }

if(year ==2020) r.tmp = r_hp_2020
if(year ==2022) r.tmp = r_hp_2022
  # House-Pres is on top
  # All districts
  # Standard error
  lines(c(r.tmp$beta_all - 1.96*r.tmp$se_all,
          r.tmp$beta_all + 1.96*r.tmp$se_all), 
        c(2.0, 2.0), lwd = 2, 
        col = "darkgrey")
  # Coefficient
  lines(rep(r.tmp$beta_all, 2), 
        c(1.95, 2.05), lwd = 3, 
        col = "black")
  # Competitive districts
  # Standard error
  lines(c(r.tmp$beta_compet - 1.96*r.tmp$se_compet,
          r.tmp$beta_compet + 1.96*r.tmp$se_compet), 
        c(1.8, 1.8), lwd = 2, 
        col = "darkgrey")
  # Coefficient
  lines(rep(r.tmp$beta_compet, 2), 
        c(1.75, 1.85), lwd = 3, 
        col = "black")

  # Hi spend districts
  # Standard error
  lines(c(r.tmp$beta_hispend - 1.96*r.tmp$se_hispend,
          r.tmp$beta_hispend + 1.96*r.tmp$se_hispend), 
        c(1.6, 1.6), lwd = 2, 
        col = "darkgrey")
  # Coefficient
  lines(rep(r.tmp$beta_hispend, 2), 
        c(1.55, 1.65), lwd = 3, 
        col = "black")
  
  if(year == 2020)  {
    # House-Sen is on bottom for2020 only
    # All districts
    # Standard error
    lines(c(r_hs_2020$beta_all - 1.96*r_hs_2020$se_all,
            r_hs_2020$beta_all + 1.96*r_hs_2020$se_all), 
          c(1.2, 1.2), lwd = 2, 
          col = "darkgrey")
    # Coefficient
    lines(rep(r_hs_2020$beta_all, 2), 
          c(1.15, 1.25), lwd = 3, 
          col = "black")
    # Competitive districts
    # Standard error
    lines(c(r_hs_2020$beta_compet - 1.96*r_hs_2020$se_compet,
            r_hs_2020$beta_compet + 1.96*r_hs_2020$se_compet), 
          c(1, 1), lwd = 2, 
          col = "darkgrey")
    # Coefficient
    lines(rep(r_hs_2020$beta_compet, 2), 
          c(0.95, 1.05), lwd = 3, 
          col = "black")

    # Hi spend districts
    # Standard error
    lines(c(r_hs_2020$beta_hispend - 1.96*r_hs_2020$se_hispend,
            r_hs_2020$beta_hispend + 1.96*r_hs_2020$se_hispend), 
          c(0.8, 0.8), lwd = 2, 
          col = "darkgrey")
    # Coefficient
    lines(rep(r_hs_2020$beta_hispend, 2), 
          c(0.75, 0.85), lwd = 3, 
          col = "black")
      }
  if(year == 2022)  {
    text(7.5, 1, "Data unavailable", col = "grey")
  } 
  }
