
## Demonstrate dependence on domain size by analyzing range of ratios for a1 = 1,3 and tau fixed at 1

library(tidyverse)
library(reshape2)

beta_score <- function(a, b) {
  return( 1 - sqrt(1 / (as.numeric(a)*as.numeric(b))) )
}

beta_bias <- function(a, b) {
  return( as.numeric(b) - as.numeric(a) )
}


## Beta param plot for a1 = 2
cont_fit_tab <- read.table('../data/cont_fit_tab.RData')

df <- cont_fit_tab %>%
  filter((tau==1 & s1==1) |  (tau==1 & s1==3)) %>%
  mutate(score=beta_score(a, b), bias=beta_bias(a, b)) %>%
  select(-c(tau, a, b)) %>%
  melt(id.vars=c("s1", "ratio"))

range_labs <- c(
  `1` = "a0 = 1",
  `3` = "a0 = 3"
)

png("fig06.png", units="in", height=3.3, width=6.4, res=200, pointsize=9)

ggplot(data=df, aes(x=ratio, y=value, color=variable, linetype=variable)) +
  facet_grid(~s1, labeller = as_labeller(range_labs)) +
  geom_hline(yintercept=0, linetype=3, size=0.3) +
  geom_line(size=0.3) +
  geom_point(size=0.8) +
  scale_colour_manual(values=c(score="skyblue3", bias="darkred")) +
  scale_linetype_manual(breaks=c("score","bias"), values=c(1, 2)) +
  scale_y_continuous(breaks=seq(-0.8, 0.2, 0.2)) +
  labs(x="Ratio", y="Metric") +
  theme_bw() +
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        text = element_text(color="black"),
        strip.text= element_text(size=12),
        axis.text = element_text(size=9, color="black"),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        aspect.ratio = 1/1,
        plot.margin = unit(c(0,0,0,0.1), "cm"))

dev.off()
