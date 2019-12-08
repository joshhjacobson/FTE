
## Process and combine exceedence data to use for analysis

library(dplyr)
library(tidyr)
library(reshape2)
library(scales)
library(fitdistrplus)
library(ggplot2)

## NOTE: In this codebase, s1 and s2 refer to the verification and ensemble correlation lengths (respectively) 


# Collect data ------------------------------------------------------------

nam <- "data/exceed_dat_s"
s_1 <- seq(1,4,0.5)
tau <- seq(0,4,0.5)
N <- 5000

## store verification fte rank data from all simulations
rank_arr <- array(dim = c(N, 11, 9, 7))


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


for (ii in 1:length(s_1)) {
  print(paste("s1 = ", s_1[ii]))
  s_2 <- seq(0.5*s_1[ii],1.5*s_1[ii],0.1*s_1[ii])
  ## init rank_cube array
  rank_cube <- array(dim = c(N, 11, 9))
  ## read in data
  load(paste(nam, s_1[ii], ".RData", sep=""))
  ## loop over s_2
  for (jj in 1:length(s_2)) {
    ## loop over tau
    for (t in 1:length(tau)) {
      ## rank each realization
      rank_cube[,jj,t] <- apply(arr_dat[,,t,jj], 1, rank_obs)
    }
  }
  
  ## fill in ranks array
  rank_arr[,,,ii] <- rank_cube
  rm(arr_dat, rank_cube)
}


# Reformat data -----------------------------------------------------------

## flatten data
rank_tab <- melt(rank_arr, value.name='rank',
                 varnames=c('N', 's2_idx', 'tau_idx', 's1_idx')) %>%
            mutate(., 
                   s1=rescale(s1_idx, to=c(1,4)),
                   s2=rescale(s2_idx, to=c(0.5,1.5))*s1,
                   tau=rescale(tau_idx, to=c(0,4))
            ) %>%
            dplyr::select(., s1, s2, tau, N, rank)


## fit beta parameters to rank hists after spreading
set.seed(10)
spread_rank <- function(r) {
  return(runif(1, r-1/24, r+1/24))
}

cont_fit_tab <- rank_tab %>%
  drop_na() %>%
  mutate(rank = (rank-0.5)/12,
         ratio=s2/s1) %>%
  mutate(rank = sapply(rank, spread_rank)) %>%
  group_by(s1,ratio,tau) %>%
  summarise(params=paste(fitdist(rank,'beta')$estimate, collapse=" ")) %>%
  separate(params, c('a', 'b'), sep=" ") %>%
  mutate(a=as.numeric(a), b=as.numeric(b))


# Save to data folder -----------------------------------------------------

write.table(rank_tab, file='data/rank_tab.RData')
write.table(cont_fit_tab, file='data/cont_fit_tab.RData')

