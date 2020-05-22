
## Demonstrate dependence on tau by analyzing range of taus at low, even, and high ratios (a1 = 2)

library(tidyverse)
library(reshape2)
library(grid)
library(gridExtra)

beta_score <- function(a, b) {
  return( 1 - sqrt(1 / (as.numeric(a)*as.numeric(b))) )
}

beta_bias <- function(a, b) {
  return( as.numeric(b) - as.numeric(a) )
}



## bootstrapped parameter fits for fig 5
# for each ratio in (0.5, 0.9, 1.0, 1.1. 1.5) we need multiple fits at each threshold
# generate new table for each ratio (samples x threshold)

load('../data/listed_ratio_ranks.RData')


cont_fit_tab <- read.table('../data/cont_fit_tab.RData')

df_params <- cont_fit_tab %>%
  filter(s1==2, ratio==0.5 | ratio==0.9 | ratio==1.0 | ratio == 1.1 | ratio==1.5) %>%
  dplyr::select(-s1) %>%
  melt(id.vars=c("tau", "ratio"))

df_scores <- cont_fit_tab %>%
  filter(s1==2, ratio==0.5 | ratio==0.9 | ratio==1.0 | ratio == 1.1 | ratio==1.5) %>%
  mutate(score=beta_score(a, b), bias=beta_bias(a, b)) %>%
  dplyr::select(-c(s1, a, b)) %>%
  melt(id.vars=c("tau", "ratio"))

ratio_labs <- c(
  `0.5` = "ratio = 0.5",
  `0.9` = "ratio = 0.9",
  `1` = "ratio = 1.0",
  `1.1` = "ratio = 1.1",
  `1.5` = "ratio = 1.5"
)


p1 <- ggplot(data=df_params, aes(x=tau, y=value, color=variable, linetype=variable)) +
  facet_grid(~ratio, labeller = as_labeller(ratio_labs)) +
  geom_hline(yintercept=1, linetype=3, size=0.2) +
  geom_line(size=0.3) +
  geom_point(size=0.6) +
  scale_colour_manual(values=c(a="sandybrown", b="navy")) +
  scale_linetype_manual(breaks=c("a","b"), values=c(1, 2)) +
  scale_y_continuous(breaks = c(0.5, 1.0, 1.5)) +
  labs(x="", y="Parameter") +
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

p2 <-ggplot(data=df_scores, aes(x=tau, y=value, color=variable, linetype=variable)) +
  facet_grid(~ratio) +
  geom_hline(yintercept=0, linetype=3, size=0.2) +
  geom_line(size=0.3) +
  geom_point(size=0.6) +
  scale_colour_manual(values=c(score="skyblue3", bias="darkred")) +
  scale_linetype_manual(breaks=c("score","bias"), values=c(1, 2)) +
  scale_y_continuous(breaks=seq(-0.8,0.4,0.4)) +
  labs(x="Threshold", y="Metric") +
  theme_bw() +
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(color="black"),
        axis.text = element_text(size=9, color="black"),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        aspect.ratio = 1/1,
        plot.margin = unit(c(0,0,0,0.1), "cm"))


# png("fig05.png", units="in", height=4.4, width=8.4, res=200, pointsize=10)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size="last"))
# dev.off()
