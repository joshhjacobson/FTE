
# Motivating figure with a verification field and three forecast fields, 
# one with correct spatial structure, one with close structure, and one with mediocre structure

library(RandomFields)
library(fields)
library(tidyverse)
library(gridExtra)
library(RColorBrewer)


## Functions to numerically determine rho value that produces desired xi
rho_root <- function(rho, xi, smooth, rng, var) {
  model <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
  return (RFcov(model, x=0)[1,1,2] - xi)
}
rhored_search <- function(xi, smooth, rng, var) {
  # xi (float): desired weight ratio between ensemble mean and perturbation
  # NOTE: other model parameters passed to RMbiwm() are assumed to be set and constant
  if(rng[1]==rng[3]){
    return(xi)
  } else{
    rhored <- uniroot(rho_root, c(xi, 1), xi=xi, smooth=smooth, rng=rng, var=var)$root
    return (rhored)
  }
}

## grid
x <- y <- seq(-20, 20, 0.2)

## model params
a1 <- 2
a2 <- c(a1, 1.5*a1, 0.9*a1)
xi <- 0.8; smooth <- c(1.5, 1.5, 1.5); var <- c(1, 1)

fields <- data.frame(array(dim=c(length(x)*length(y), 4)))

## simulate the verification and various ensemble means
seeds <- c(15, 20, 10) 
for(i in 1:length(a2)){
  rng <- c(a1, sqrt(a1*a2[i]), a2[i])
  rho <- rhored_search(xi, smooth, rng, var)
  
  ## model
  set.seed(22)
  model_biwm <- RMbiwm(nu=smooth, s=rng, cdiag=var, rhored=rho)
  sim <- RFsimulate(model_biwm, x, y)
  
  ## ensemble perturbation
  set.seed(seeds[i])
  model_whittle <- RMwhittle(nu=smooth[3], notinvnu=TRUE,
                             scale=rng[3], var=var[2])
  omega <- RFsimulate(model_whittle, x, y)
  
  fcast <- xi*sim$variable2 + sqrt(1-xi^2)*omega$variable1
  
  ## set verification field using simulation with "correct" structure
  if(i==1){
    fields[,1] <- sim$variable1
  }
  fields[,(i+1)] <- fcast
  rm(rng, rho, sim, omega, fcast, model_biwm, model_whittle)
} 


## plot the fields
dat <- expand.grid(x = x, y = y)
dat["z"] <- fields[,1]


png("motivating_example.png", units="in", width=8, height=2.2, res=220, pointsize=9)

par(mfrow=c(1,4))
par(mar=c(3,3,4,2)+0.1)
for(i in 1:ncol(fields)){
  dat$z <- fields[,i]
  if (i == 1) {
    quilt.plot(dat, col=brewer.pal(9, "BuPu"), nx=200, ny=200, 
               zlim = range(fields[,1]) + c(-0.1, 0.1), add.legend=FALSE,
               main="Verification")
  } else {
    quilt.plot(dat, col=brewer.pal(9, "BuPu"), nx=200, ny=200, 
               zlim = range(fields[,1]) + c(-0.1, 0.1), add.legend=FALSE,
               main=paste("Forecast", i-1, sep=" "))
  }
}

dev.off()

