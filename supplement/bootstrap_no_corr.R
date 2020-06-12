
library(tidyverse)
library(reshape2)
library(fitdistrplus)

# NOTE: ranks have already been mapped to (0,1) and disaggregated
load("../data/supplement/listed_ratio_ranks_s2_no_corr.RData")


beta_score <- function(a, b) {
  return( 1 - sqrt(1 / (a * b)) )
}

beta_bias <- function(a, b) {
  return( b - a )
}


set.seed(10)
niter <- 1001
tau_list <- seq(0, 4, 0.5)
boot_fit <- array(dim = c(niter, 2, 9, 5))

for(r in 1:length(listed_ratio_ranks)) {
  df <- listed_ratio_ranks[[r]] 
  
  # For each value of tau, perform nonparametric bootstrap (sample with replacement) using niter samples
  for(t in 1:length(tau_list)) {
    print(paste0("Ratio: ", r, ", Tau: ", t))
    # Get the empirical distribution
    data <- df %>% filter(tau == tau_list[t]) %>% pull(rank)
    # Constuct initial fit object
    fit <- fitdist(data, "beta")
    # Bootstrap the parameter fits
    estim <- bootdist(fit, bootmethod="nonparam", niter=niter, parallel="multicore", ncpus=4)$estim
    boot_fit[, , t, r] <- data.matrix(estim)
  }
  
}


# Coerce array into tidy dataframe, compute metrics
boot_fit_tab <- melt(boot_fit, varnames=c("N", "param", "tau", "ratio")) %>%
  mutate(
    param=c("a", "b")[param],
    tau=tau_list[tau],
    ratio=c(0.5, 0.9, 1.0, 1.1, 1.5)[ratio]
  ) %>%
  pivot_wider(names_from=param, values_from=value) %>%
  mutate(
    beta.score=beta_score(a, b),
    beta.bias=beta_bias(a, b)
  )

write.table(boot_fit_tab, file="data/supplement/boot_fit_tab_no_corr.RData")
