
library(tidyverse)
library(fitdistrplus)

# NOTE: ranks have already been mapped to (0,1) and disaggregated
load("data/listed_ratio_ranks.RData")


# Draw new sample from data with replacement
sample_fit <- function(data) {
  samp <- base::sample(data, size=length(data), replace=TRUE)
  return( fitdist(samp, 'beta')$estimate )
}

set.seed(10)
N_reps <- 1000
tau_list <- seq(0, 4, 0.5)
boot_fit <- array(dim = c(2, N_reps, 9, 5))

for(r in 1:length(listed_ratio_ranks)) {
  df <- listed_ratio_ranks[[r]] 
  
  # For each value of tau, draw new sample (with replacement) of same size, then fit parameters (repeat N_rep times)
  for(t in 1:length(tau_list)) {
    # Get the empirical distribution
    data <- df %>% filter(tau == tau_list[t]) %>% pull(rank)
    # Bootstrap the parameter fits
    boot_fit[, , t, r] <- replicate(N_reps, sample_fit(data))
  }
  
}

# Coerce array into tidy dataframe
boot_fit_tab <- melt(boot_fit, varnames=c("param", "N", "tau", "ratio")) %>%
  mutate(., 
         param=c("a", "b")[param],
         tau=tau_list[tau],
         ratio=c(0.5, 0.9, 1.0, 1.1, 1.5)[ratio]
  )

write.table(boot_fit_tab, file="data/rank_tab.RData")
