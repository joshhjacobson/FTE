
## Demonstrate dependence on tau by analyzing range of taus at low, even, and high ratios (a1 = 2)

library(tidyverse)
library(reshape2)

cont_fit_tab <- read.table('../data/cont_fit_tab.RData')

df <- cont_fit_tab %>%
  filter(s1==2, ratio==0.5 | ratio==0.9 | ratio==1.0 | ratio == 1.1 | ratio==1.5) %>%
  select(-s1) %>%
  melt(id.vars=c("tau", "ratio"))

ratio_labs <- c(
  `0.5` = "ratio = 0.5",
  `0.9` = "ratio = 0.9",
  `1` = "ratio = 1.0",
  `1.1` = "ratio = 1.1",
  `1.5` = "ratio = 1.5"
)

png("fig05.png", units="in", height=2.2, width=8.4, res=200, pointsize=10)

ggplot(data=df, aes(x=tau, y=value, color=variable, linetype=variable)) +
  facet_grid(~ratio, labeller = as_labeller(ratio_labs)) +
  geom_hline(yintercept=1, linetype=3, size=0.2) +
  geom_line(size=0.25) +
  geom_point(size=0.6) +
  scale_colour_manual(values=c(a="darkred", b="skyblue3")) +
  scale_linetype_manual(breaks=c("a","b"), values=c(2, 1)) +
  scale_y_continuous(breaks = c(0.5, 1.0, 1.5)) +
  labs(x="Threshold", y="Parameter") +
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
