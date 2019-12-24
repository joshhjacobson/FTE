
## Produce FTE histograms for downscaled GEFS data

library(lubridate)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(fitdistrplus)
library(latex2exp)


# Threshold exceedence ranking --------------------------------------------

## import fields_dat dim(lat x lon x time x mem)
load("../data/gefs_downscaled_fields.RData")

## compute the mean threshold exceedence of fields_df at array of thresholds
## and return analysis ranks
exceed_ranks <- function(dat_arr, tau){
  # dat_arr: 3d array (lon x lat x member)
  # tau: vector of thresholds
  ranks <- array(dim = length(tau))
  for (i in 1:length(tau)) {
    m <- apply(dat_arr, 3, function(field) mean(as.vector(field) > tau[i]))
    if(length(unique(m)) != 1) {
      ranks[i] <- rank(m, ties.method = "random")[1]
    } else {
      # exclude exact ties
      ranks[i] <- NA
    }
  }
  return(ranks)
}

disagg_rank <- function(r) {
  return(runif(1, r-1/24, r+1/24))
}

## iterate over time, compute ranks at different thresholds
set.seed(10)
tau <- c(5,10,20)
ranks_df <- data.frame(t(apply(field_dat, 3, exceed_ranks, tau=tau))) # (day x tau)


# FTE Histograms ----------------------------------------------------------

## stratify days by month and build histograms for each month-threshold pair
names(ranks_df) <- paste('tau', tau, sep='')

m <- c(1, 4, 7, 10) # Jan, Apr, Jul, Oct
dates <- seq.Date(as.Date('2002-01-02'), as.Date('2015-12-30'), by='day')
date_idx <- (month(dates) %in% m)

## fit beta parameters to density fte hists
set.seed(20)
down_fit_tab <- ranks_df %>%
  mutate(month = month(dates[date_idx])) %>%
  melt(id.vars='month', variable.name='tau', value.name='rank') %>%
  mutate(rank = sapply((rank-0.5)/12, disagg_rank)) %>%
  group_by(tau, month) %>%
  drop_na() %>%
  summarise(params=paste(fitdist(rank,'beta')$estimate, collapse=" ")) %>%
  separate(params, c('a', 'b'), sep=" ") %>%
  mutate(a=round(as.numeric(a), 3), b=round(as.numeric(b),3)) %>%
  unite(params, a:b, sep = ", ")

## build dataframe with months and tau as factors to facet over
month_labs <- rep("", 12)
month_labs[c(1,4,7,10)] <- c("January", "April", "July", "October")
tau_labs <- c(tau5=TeX(paste("$\\tau = $", "5 mm")),
              tau10=TeX(paste("$\\tau = $", "10 mm")),
              tau20=TeX(paste("$\\tau = $", "20 mm")))

ranks_df <- ranks_df %>%
  mutate(month = month(dates[date_idx])) %>%
  melt(id.vars='month', variable.name='tau', value.name='rank') %>%
  mutate(month = as.factor(month_labs[month])) %>%
  mutate(rank = (rank-0.5)/12)

ranks_df$month <- factor(ranks_df$month,
                     levels = c("January", "April", "July", "October"),
                     labels = c("bold(January)", "bold(April)", "bold(July)", "bold(October)"))
levels(ranks_df$tau) <- tau_labs


png("fig09.png", units="in", height=6.2, width=8, res=200, pointsize=10)

ranks_df %>%
  ggplot(aes(x=rank)) +
  geom_hline(yintercept=1, linetype=3, size=0.3, color="grey") +
  geom_histogram(aes(y=..density..), bins=12, fill="black", color="white") +
  scale_y_continuous(breaks=seq(0,2)) +
  facet_grid(rows=vars(tau), cols=vars(month), labeller=label_parsed) +
  annotate("text", x=0.48, y=2.5, size=4, label=down_fit_tab$params) +
  labs(y="", x="") +
  theme_bw() +
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        text = element_text(color="black"),
        strip.text = element_text(size=12),
        axis.text = element_text(size=9, color="black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_blank(),
        aspect.ratio = 1/1,
        plot.margin = unit(c(0,0,0,0), "cm"))

dev.off()
