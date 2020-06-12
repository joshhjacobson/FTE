
## Demonstrate dependence on tau by analyzing range of taus at low, even, and high ratios (a1 = 2)

library(tidyverse)
library(reshape2)
library(grid)
library(gridExtra)

boot_fit_tab <- read.table('../data/boot_fit_tab.RData')

df_params <- boot_fit_tab %>%
  select(c(ratio, tau, a, b)) %>%
  pivot_longer(cols=c(a, b), names_to="param") %>%
  group_by(ratio, tau, param) %>%
  summarise(
    med = median(value),
    lwr = quantile(value, 0.025),
    upr = quantile(value, 0.975)
  )

df_metric <- boot_fit_tab %>%
  rename(score=beta.score, bias=beta.bias) %>%
  select(c(ratio, tau, score, bias)) %>%
  pivot_longer(cols=c(score, bias), names_to="metric") %>%
  group_by(ratio, tau, metric) %>%
  summarise(
    med = median(value),
    lwr = quantile(value, 0.025),
    upr = quantile(value, 0.975)
  )


ratio_labs <- c(
  `0.5` = "ratio = 0.5",
  `0.9` = "ratio = 0.9",
  `1` = "ratio = 1.0",
  `1.1` = "ratio = 1.1",
  `1.5` = "ratio = 1.5"
)

# The errorbars overlap, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right


p1 <- ggplot(data=df_params, aes(x=tau, y=med, color=param)) +
  facet_grid(~ratio, labeller = as_labeller(ratio_labs)) +
  geom_hline(yintercept=1, linetype=3, size=0.2) +
  geom_line(size=0.2, position=pd) +
  geom_point(size=0.4, position=pd) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), size=0.2, width=0.2, position=pd) +
  scale_colour_manual(values=c(a="sandybrown", b="navy")) +
  # scale_linetype_manual(breaks=c("a","b"), values=c(1, 2)) +
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

p2 <- ggplot(data=df_metric, aes(x=tau, y=med, color=metric)) +
  facet_grid(~ratio) +
  geom_hline(yintercept=0, linetype=3, size=0.2) +
  geom_line(size=0.2, position=pd) +
  geom_point(size=0.4, position=pd) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), size=0.2, width=0.2, position=pd) +
  scale_colour_manual(values=c(score="skyblue3", bias="darkred")) +
  # scale_linetype_manual(breaks=c("score","bias"), values=c(1, 2)) +
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


png("fig05.png", units="in", height=4, width=8.4, res=250, pointsize=9)
# grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size="last"))
dev.off()
