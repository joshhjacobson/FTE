
## Create standard vefification rank histograms from downscaled field data

library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)

## import fields_dat dim(lat x lon x time x mem)
load('../data/gsdm_downscaled_fields.RData')

disagg_rank <- function(r) {
  return(runif(1, r-1/24, r+1/24))
}

beta_score <- function(a, b) {
  bs <- 1 - sqrt(1 / (as.numeric(a)*as.numeric(b)))
  return( round(bs, 3) )
}

beta_bias <- function(a, b) {
  bb <- as.numeric(b) - as.numeric(a)
  return( round(bb, 3) )
}


m <- c(1, 4, 7, 10) # Jan, Apr, Jul, Oct
dates <- seq.Date(as.Date('2002-01-02'), as.Date('2015-12-30'), by='day')
dates <- dates[(month(dates) %in% m)]


## iterate over time, compute pointwise ranks across members
ranks_arr <- apply(field_dat, c(3,1,2),
                   function(point){
                     if(length(unique(point)) == 1) {
                       ## ignore exact ties in rank
                       return(NA)
                     }
                     return(rank(point, ties.method = "random")[1])
                   })


## tidy data
ranks_df <- data.frame(t(matrix(ranks_arr, dim(ranks_arr)[1], prod(dim(ranks_arr)[2:3]))))
colnames(ranks_df) <- dates

## map ranks into (0,1)
ranks_df <- melt(ranks_df, variable.name='date', value.name='rank')
ranks_df <- ranks_df %>% drop_na() %>% mutate(rank = (rank-0.5)/12)

## fit beta parameters to density rank hists
set.seed(20)
down_fit_tab <- ranks_df %>%
  drop_na() %>%
  mutate(rank = sapply(rank, disagg_rank), month = month(date)) %>%
  group_by(month) %>%
  summarise(params=paste(fitdist(rank,'beta')$estimate, collapse=" ")) %>%
  separate(params, c('a', 'b'), sep=" ") %>%
  mutate(beta.score=beta_score(a, b), beta.bias=beta_bias(a, b)) %>%
  unite(scores, beta.score:beta.bias, sep = ", ") 


## create labels for facets
month_labs <- c(
  '1' = "January",
  '4' = "April",
  '7' = "July",
  '10' = "October"
)

## stratify days by month and build histograms
png("fig08.png", units="in", height=2.3, width=8, res=200, pointsize=10)

ranks_df %>%
  mutate(month = month(date)) %>%
  ggplot(aes(x=rank)) +
  geom_hline(yintercept=1, linetype=3, size=0.3, color="grey") +
  geom_histogram(aes(y=..density..), bins=12, fill="black", color="white") +
  facet_wrap(~month, ncol=4, labeller=as_labeller(month_labs)) +
  annotate("text", x=0.45, y=1.5, size=4, label=down_fit_tab$scores) +
  labs(y="", x="") +
  theme_bw() +
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        text = element_text(color="black"),
        strip.text= element_text(size=12, face="bold"),
        axis.text = element_text(size=9, color="black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_blank(),
        aspect.ratio = 1/1,
        plot.margin = unit(c(0,0.1,0,0), "cm"))

dev.off()
