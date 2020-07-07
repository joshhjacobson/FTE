
## Process and combine exceedence data to use for analysis

library(tidyverse)
library(reshape2)
library(fitdistrplus)

## NOTE: In this codebase, s1 and s2 refer to the verification and ensemble correlation lengths, respectively 


# Collect data ------------------------------------------------------------

## Compute rank of an observation's mean in the distribution
## of the ensemble means
rank_obs <- function(means) {
  # means: observation and ensemble means with obs listed first
  if(length(unique(means)) == 1) { 
    # disregard as random
    return(NA)
  }
  r <- rank(means, ties.method = "random")[[1]] 
  return(r)
}


# NOTE: s1 = 2
nam <- "../data/appx/exceed_dat_no_corr_s2.RData"
load(nam)

## store verification fte rank data for each realization
set.seed(0)
rank_arr <- apply(arr_dat, c(1, 4, 3), rank_obs)


# Reformat data -----------------------------------------------------------

s2_list <- c(1, 1.8, 2, 2.2, 3)
tau_list <- seq(0,4,0.5)

## flatten data
rank_tab <- melt(rank_arr, value.name="rank", varnames=c("N", "s2", "tau")) %>%
  mutate(
    s2=s2_list[s2],
    tau=tau_list[tau]
  )

## fit beta parameters to rank hists after disaggregation
set.seed(10)
spread_rank <- function(r) {
  return(runif(1, r-1/24, r+1/24))
}

cont_fit_tab <- rank_tab %>%
  drop_na() %>%
  mutate(rank = (rank-0.5)/12, ratio=s2/2) %>%
  mutate(rank = sapply(rank, spread_rank)) %>%
  group_by(ratio, tau) %>%
  summarise(params=paste(fitdist(rank,'beta')$estimate, collapse=" ")) %>%
  separate(params, c('a', 'b'), sep=" ") %>%
  mutate(a=as.numeric(a), b=as.numeric(b))



## separate each ratio group into seperate table with cols (ratio, tau, rank)
# this will be used for bootstrap resampling in fig 5
set.seed(10)
listed_ratio_ranks <- rank_tab %>%
  drop_na() %>%
  mutate(rank = (rank-0.5)/12, ratio=s2/2) %>%
  dplyr::select(c(ratio, tau, rank)) %>%
  mutate(rank = sapply(rank, spread_rank)) %>%
  group_split(ratio)


# Save to data folder -----------------------------------------------------

write.table(rank_tab, file='../data/appx/rank_tab_s2_no_corr.RData')
write.table(cont_fit_tab, file='../data/appx/cont_fit_tab_s2_no_corr.RData')
save(listed_ratio_ranks, file='../data/appx/listed_ratio_ranks_s2_no_corr.RData')

