# Create plot for model for a_generalElec_July2024.Rmd
# dist_mean = -1
# district_plot_FUNCTION(dist_mean = 1)


theory_district_plot_FUNCTION= function(dist_mean){
  ## Cutpoint model figure
  X  		= seq(-4, 4, length=200)
  #dist_mean = -1
  Dist = dnorm(X, mean = dist_mean, sd = 3)
  adjDown = 0.1
  
  # plot(X, Dist, type="n", lty=2, xlab="", ylab="", xaxt='n',
  #      cex.axis=0.9, yaxt='n',
  #      ylim = c(0, 1.20*max(Dist)), xlim= c(-3.7, 3.7))
  # axis(1, at=0,	labels=0, cex.axis=0.7, padj = -1.6)
  # 
  # # Senate race
  cutpoint_senate = -1
  # polygon(c(-4,      seq(-4,cutpoint_senate,0.01), cutpoint_senate),
  #         c( -0.01, dnorm(seq(-4,cutpoint_senate,0.01), mean= dist_mean, sd=3), -0.01),
  #         col = rgb( 19/255, 52/255, 143/255, alpha = 0.6), border = "white")
  # lines(c(-2.5, 0.5), c(0.01, 0.01), lwd = 2, col = "black")
  # points(-2.5, 0.01, pch= 19, cex = 2, col = "#0f2a72") #text(-2.7, 0.003, expression(theta[D]^S), col = "black" )
  # axis(1, at=-2.5,	labels= -2.5, cex.axis=0.8, padj = -2, tick=TRUE, tck = -0.02)
  # points( 0.5, 0.01, pch= 19, cex = 2, col = rgb( 200/255, 0/255, 4/255))
  # axis(1, at=0.5,	labels= 0.5, cex.axis=0.8, padj = -2, tick=TRUE, tck = -0.02)
  # lines(c(cutpoint_senate,cutpoint_senate), 
  #       c(-1, dnorm(cutpoint_senate, mean = dist_mean, sd = 3)), 
  #       lwd = 2, col = "black")
  # text(-1.6, 0.057, "Senate\ncutpoint", cex = 0.8, col = "black")
  # mtext(expression(kappa^S), side = 1, at = cutpoint_senate, line = 1.2, 
  #       cex = 1, col = "black")
  # text(1.1*cutpoint_senate, 1.15*dnorm(cutpoint_senate, mean = dist_mean, sd = 3)- (dist_mean<0)*0.01, 
  #      paste0(100*round(pnorm(cutpoint_senate, mean = dist_mean, sd = 3), 2), "%"),
  #      col = "black", cex = 0.9)
  # # Voter distribution
  # lines(X, Dist, type="l", lty=1, lwd=2, col= "black")
  # text(2.9, (1.5-(dist_mean>0)*0.23)*dnorm(3, mean = dist_mean, sd = 3), 
  #      "Distribution of\nvoters", cex = 0.7)
  
  # Bottom plot with both races
  plot(X, Dist, type="n", lty=2, xlab="", ylab="", xaxt='n',
       cex.axis=0.9, yaxt='n',
       ylim = c(0, 1.20*max(Dist)), xlim= c(-3.6, 3.6))
  mtext("Ideology", side = 1, at = 0.0, line = 2.3, cex = 1.0)
  axis(1, at=0,	labels=0, cex.axis=0.7, padj = -1.6)
  #axis(1, at=((-3:5)*2),	labels=((-3:5)*2), cex.axis=0.7, padj = -1.6)
  
  # Print House vote share right away to put in "back" of figure
  cutpoint_house = 0.5*(-0.5+2.5)
  polygon(c(-4,      seq(-4,cutpoint_house,0.01), cutpoint_house),
          c( -0.01,dnorm(seq(-4,cutpoint_house,0.01), mean= dist_mean, sd=3), -0.01),
          col = rgb( 114/255, 147/255, 236/255, alpha = 0.6), border = "white")
  
  
  # Senate race
  polygon(c(-4,      seq(-4,cutpoint_senate,0.01), cutpoint_senate),
          c( -0.01, dnorm(seq(-4,cutpoint_senate,0.01), mean= dist_mean, sd=3), -0.01),
          col = rgb( 19/255, 52/255, 143/255, alpha = 0.6), border = "white")
  lines(c(-2.5, 0.5), c(0.005, 0.005), lwd = 2, col = "black")
  points(-2.5, 0.005, pch= 19, cex = 2, col = "#0f2a72") #text(-2.7, 0.003, expression(theta[D]^S), col = "black" )
  axis(1, at=-2.5,	labels= -2.5, cex.axis=0.8, padj = -2, tick=TRUE, tck = -0.02)
  points( 0.5, 0.005, pch= 19, cex = 2, col = rgb( 200/255, 0/255, 4/255))
  axis(1, at=0.5,	labels= 0.5, cex.axis=0.8, padj = -2, tick=TRUE, tck = -0.02)
  lines(c(cutpoint_senate,cutpoint_senate), 
        c(-1, dnorm(cutpoint_senate, mean = dist_mean, sd = 3)), 
        lwd = 2, col = rgb( 19/255, 52/255, 143/255))
  text(-0.1, 0.018, "Senate\ncutpoint", cex = 0.8, col = "black")
  mtext(expression(kappa^S), side = 1, at = cutpoint_senate, line = 1.2, 
        cex = 1, col = "black")
  text(1.1*cutpoint_senate, 1.15*dnorm(cutpoint_senate, mean = dist_mean, sd = 3)- (dist_mean<0)*0.01, 
       paste0(100*round(pnorm(cutpoint_senate, mean = dist_mean, sd = 3), 2), "%"),
       col = rgb( 19/255, 52/255, 143/255), cex = 0.9)
  # Voter distribution
  lines(X, Dist, type="l", lty=1, lwd=2, col= "black")
  
  # House race
  lines(c(-0.5, 2.5), c(0.04, 0.04), lwd = 2, col = "black")
  points(-0.5, 0.04, pch= 15, cex = 2, col = "#0f2a72")
  axis(1, at=-0.5,	labels= -0.5, cex.axis=0.8, padj = -2, tick=TRUE, tck = -0.02)
  points(2.5, 0.04, pch= 15, cex = 2, col = rgb( 200/255, 0/255, 4/255))
  axis(1, at=2.5,	labels= 2.5, cex.axis=0.8, padj = -2, tick=TRUE, tck = -0.02)
  lines(c(cutpoint_house, cutpoint_house), 
        c(-1, dnorm(cutpoint_house, mean= dist_mean, sd = 3)), 
        lwd = 2.5, col = rgb( 114/255, 147/255, 236/255))
  text(1.88, 0.055, "House\ncutpoint", cex = 0.8, col = "black")
  mtext(expression(kappa^H), side = 1, at = cutpoint_house, line = 1.2, cex = 1, 
        col = "black")
  text(cutpoint_house, 1.15*dnorm(cutpoint_house, mean = dist_mean, sd = 3) - (dist_mean>0)*0.01, 
       paste0(100*round(pnorm(cutpoint_house, 
                              mean = dist_mean, sd = 3), 2), "%"),
       col = rgb( 114/255, 147/255, 236/255), cex =0.9)
  
  # Voter distribution
  text(2.5, (1.8-(dist_mean>0)*0.23)*dnorm(3, mean = dist_mean, sd = 3), 
       "Distribution of\nvoters", cex = 0.7)
}

