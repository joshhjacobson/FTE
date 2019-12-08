

## Sandalone file containing all functions and code to simulate realization data

library(RandomFields)


# Functions ---------------------------------------------------------------

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


## Function to obtain the threshhold exeedence for each field in a realization
exceedence <- function(data, tau) {
  # data: observation and ensemble in cols of data frame
  # tau: threshold
  m <- array(colMeans(data > tau))
  return(m)
}


## Function to construct single realization
build_ensemble <- function(arr_dat, index, rng, rhored,
                           xi=0.8, smooth=c(1.5, 1.5, 1.5), var=c(1, 1), n=11) {

  # arr_dat (array): 4D array storing exceedence data
  # index: realization number and s2 id
  # rng (list of 3): scale parameters for observation and ensemble; c(s_1, gmean, s_2), s=1/a
  # rhored: percent cross correlation
  # xi (float): weight ratio between ensemble mean and perturbation
  # smooth: smoothnes or differentiability (nu)
  # var: variances
  # x, y (arrays): field grid points
  # n (num): number of ensemble members

  ## grid vectors
  x <- y <- seq(-20, 20, 0.2)

  ## model
  model_biwm <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rhored)
  fields <- RFsimulate(model_biwm, x, y)

  ## ensemble perturbation
  model_whittle <- RMwhittle(nu = smooth[3], notinvnu = TRUE,
                             scale = rng[3], var = var[2])

  omega <- RFsimulate(model_whittle, x, y, n=n)
  omega <- as.matrix(data.frame(omega))

  ensemble_mean <- fields$variable2
  ensemble_mean <- replicate(n, ensemble_mean)

  ## NOTE: xi is set as 0.8, rhored is now adjusted above s.t. (true_rho = xi) holds
  ## weight ratio between ensemble mean and variance (force xi = true_rho)

  ensemble <- xi*ensemble_mean + sqrt(1-xi^2)*omega

  ## realization
  realization <- data.frame(fields$variable1, ensemble)
  # names(realization) <- c("obs", paste("f", 1:n, sep = ""))

  ## compute field exceedences
  tau <- seq(0, 4, 0.5)
  for(t in 1:length(tau)) {
    arr_dat[index[1],,t,index[2]] <- exceedence(realization, tau[t])
  }

  ## save array slice dim(ens-mem x tau) into 4D array
  nam <- paste("exceed_dat_s", rng[1], sep="")
  save(arr_dat, file = paste(nam, ".RData", sep = ""))

  return(arr_dat)
}


## Function to simulate N realizations for each (s_1,s_2) pair
## where s_1 is given and s_2 in seq(0.5*s_1, 1.5*s_1, 0.1*s_1)
range_sim <- function(s_1, N) {

  # s_1 (num): observation range parameter (s=1/a)
  # N (num): number of samples to generate for each (s_1, s_2) pair

  s_2 <- seq(0.5*s_1, 1.5*s_1, 0.1*s_1)

  ## load or build 4D array
  init_array <- function(N, s_2) {
    ## create array for each s_1 dim(N x realization member x tau x s2)
    arr_dat <- array(dim = c(N, 12, 9, length(s_2)))
    return(arr_dat)
  }
  ## check whether data already exists
  check <- try(load(paste("exceed_dat_s", s_1, ".RData", sep="")))
  if("try-error" %in% class(check)) {
    arr_dat <- init_array(N, s_2)
  }

  ## simulations
  for (ii in 1:length(s_2)) {
    set.seed(20*s_1+ii-1)
    print(paste("range:", s_1, s_2[ii], sep=" "))

    ## check if current s_2 is already completely filled out
    if ( is.na(arr_dat[N, 12, 9, ii]) ) {
      xi <- 0.8; smooth <- c(1.5, 1.5, 1.5); var <- c(1, 1); n=11
      rng <- c(s_1, sqrt(s_1*s_2[ii]), s_2[ii])  #geometric mean

      ## determine rho for desired xi at given range
      rhored <- rhored_search(xi, smooth, rng, var)

      ## loop over N (sample size)
      for (jj in 1:length(arr_dat[,1,1,1])) {
        print(jj)
        arr_dat <- build_ensemble(arr_dat, index=c(jj,ii), rng=rng, rhored=rhored, xi=xi, n=n)
      }
    } else {
      print(paste("Data found: skipping s_2 = ", s_2[ii], sep=""))
    }
  }

  return("Complete")
}



# Simulation script -------------------------------------------------------

## Simulate N realizations for a variety of range values
s_1 <- 4
N <- 5000

range_sim(s_1, N)
