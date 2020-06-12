
## Binary verification and three ensemble members with FTE histogram

library(RandomFields)
library(fields)
library(fitdistrplus)
library(tidyverse)
library(gridExtra)
library(RColorBrewer)


# Setup -------------------------------------------------------------------

## Functions to numerically determine rho value that produces desired xi
rho_root <- function(rho, xi, smooth, rng, var) {
  model <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
  return (RFcov(model, x=0)[1,1,2] - xi)
}
rhored_search <- function(xi, smooth, rng, var) {
  # xi (float): desired weight ratio between ensemble mean and perturbation
  # NOTE: other model parameters passed to RMbiwm() are assumed to be set and constant
  if(rng[1]==rng[3]){
    return(xi)
  } else{
    rhored <- uniroot(rho_root, c(xi, 1), xi=xi, smooth=smooth, rng=rng, var=var)$root
    return (rhored)
  }
}

demo_ens_sim <- function(a1, a2) {
  ## grid
  x <- y <- seq(-20, 20, 0.2)
  ## model params
  xi <- 0; smooth <- c(1.5, 1.5, 1.5); var <- c(1, 1)
  
  rng <- c(a1, sqrt(a1*a2), a2)
  rho <- rhored_search(xi, smooth, rng, var)
  
  # model
  set.seed(0)
  model_biwm <- RMbiwm(nu=smooth, s=rng, cdiag=var, rhored=rho)
  sim <- RFsimulate(model_biwm, x, y)
  
  ## ensemble perturbation
  model_whittle <- RMwhittle(nu=smooth[3], notinvnu=TRUE,
                             scale=rng[3], var=var[2])
  omega <- RFsimulate(model_whittle, x, y, n=3)
  omega <- as.matrix(data.frame(omega))
  
  ensemble_mean <- replicate(3, sim$variable2)
  ensemble <- xi*ensemble_mean + sqrt(1-xi^2)*omega
  
  fields <- data.frame(sim$variable1, ensemble)
  return(fields)
}

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



## data for histogram
rank_tab <- read.table('../data/supplement/rank_tab_s2_no_corr.RData')
rank_tab <- rank_tab %>% mutate(rank = (rank-0.5)/12)

## range parameters and grid
a1 <- 2
a2 <- c(0.9*a1, a1, 1.1*a1)
tau <- 0
x <- y <- seq(-20, 20, 0.2)


# Make figure -------------------------------------------------------------

pl <- list()
for (i in seq(1,11,5)){
  fields <- demo_ens_sim(a1, a2[round(i/5)+1])
  dat <- expand.grid(x = x, y = y)
  dat["z"] <- fields[,1]
  
  ## binary fields
  for (j in 1:ncol(fields))
    local({
      j <- j
      # update data being plotted
      dat$z <- fields[,j]
      p <- ggplot(dat, aes(x, y)) +
        geom_raster(aes(fill = z > tau)) +
        scale_fill_manual(values = c("TRUE" = "#08306B", "FALSE" = "#F7FBFF")) +
        theme_void() +
        theme(plot.title = element_blank(),
              aspect.ratio = 1/1,
              legend.position="none") +
        labs(x=NULL, y=NULL)
      pl[[i+j-1]] <<- p
    })
  
  ## fte ranks for given range pair
  set.seed(0)
  j <- round(i/5)+1
  df <- rank_tab %>% filter(s2==a2[j], tau==0)
  
  fit.beta <- df %>%
    mutate(rank = sapply(rank, disagg_rank)) %>%
    summarise(params=paste(fitdist(rank,'beta')$estimate, collapse=" ")) %>%
    separate(params, c('a', 'b'), sep=" ") %>%
    mutate(beta.score=beta_score(a, b), beta.bias=beta_bias(a, b)) %>%
    unite(scores, beta.score:beta.bias, sep = ", ") %>%
    dplyr::select(scores)
  
  p <- ggplot(df, aes(rank)) +
    geom_hline(yintercept=1, linetype=3, size=0.5, color="grey") +
    geom_histogram(aes(y=..density..), bins=12, fill="black", color="white") +
    theme_void() +
    theme(plot.title = element_blank(),
          aspect.ratio = 1/1) +
    labs(x=NULL, y=NULL)
  
  if (j == 1) {
    p <- p + annotate("text", x=0.48, y=1.15, size=3.5, label=fit.beta$scores)
  } else if (j == 2) {
    p <- p + ylim(0, 1.25) +
      annotate("text", x=0.48, y=1.2, size=3.5, label=fit.beta$scores)
  } else {
    p <- p + ylim(0, 1.3) +
      annotate("text", x=0.48, y=1.25, size=3.5, label=fit.beta$scores)
  }
  
  pl[[i+4]] <- p
}


## create row labels
row_labs <- c("A", "B", "C")
col_labs <- c("", "Verification", "Member 1", "Member 2", "Member 3", "FTE Histogram")

## figure matrix
tt <- ttheme_minimal(core = list(fg_params=list(fontface="bold")))
grd <- rbind(tableGrob(t(col_labs), theme = tt),
             cbind(tableGrob(row_labs, theme = tt),
                   arrangeGrob(grobs = pl, ncol=5),  size = "last"), size = "last")


png('fig03.png', units='in', width=8, height=5, res=400, pointsize=9)
grid.draw(grd)
dev.off()
