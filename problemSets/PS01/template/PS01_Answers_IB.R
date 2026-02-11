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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

set.seed(123)

test_data<-(rcauchy(1000, location = 0, scale = 1))

# create empirical distribution of observed data

KS_test <- function(data) {
  
  ECDF <- ecdf(data) #create the function
  empiricalCDF <- ECDF(data) #create empirical distribution using the function
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  
  bigsum <- c() #setup empty int
  
  #calculate that big sum part of the equation
  for (i in 1:length(data)) {
    result = exp((-(2*i-1)^2 * pi^2) / (8 * D^2)) 
    bigsum[i] <- result
  }

  # multiply by the part outside the sum
  final = (sqrt(2*pi)/D)*sum(bigsum)

  print(final)
}

KS_test(test_data)

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

linear.lik <- function(theta, y, X){
  n <- nrow(X)
  k <- ncol(X)
  beta <-theta[1:k]
  sigma2 <- theta[k+1]^2
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi)-.5*n*log(sigma2)-((t(e) %*% e)/(2*sigma2))
  
  return(-logl)
}

linear.MLE <-optim(fn=linear.lik, par=c(1,1,1), hessian=TRUE,y=data$y, X=cbind(1,data$x),method= "BFGS")

summary(lm(y~x,data))
linear.MLE$par
