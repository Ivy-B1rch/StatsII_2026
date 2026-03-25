#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS","tidyverse"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)
gdp_data$GDPWdiffcat <- cut(gdp_data$GDPWdiff,
                        breaks = c(-Inf, 0, Inf),
                        labels = c("Decrease", "Increase"))

gdp_data$GDPWdiffcat <- as.character(gdp_data$GDPWdiffcat)
gdp_data$GDPWdiffcat[gdp_data$GDPWdiff == 0] <- "No change"
gdp_data$GDPWdiffcat <- factor(gdp_data$GDPWdiffcat)
gdp_data$GDPWdiffcat <- relevel(gdp_data$GDPWdiffcat, ref = "No change")

model <- multinom(GDPWdiffcat ~ REG + OIL, data = gdp_data)
summary(model)

model.ord <-polr(GDPWdiffcat ~ REG + OIL, data = gdp_data, Hess =TRUE)
model.ord




#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")

#my system has an evil bug involving R studio that wont let me save the model as an object so the code shall be a bit jank
summary(glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
             family = poisson, data = mexico_elections))


exp(-3.81023 + -0.08135 + -2.08014)
