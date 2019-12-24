
## Collect downscaled forecast and analysis data in a single array

library(ncdf4)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)


# Analyses ----------------------------------------------------------------

## NOTE: dates range from 2002010200 to 2015123000

## grab analyses and time data
ncin <- nc_open('./data/GEFS_GSDM_CCPA/refcstv2_precip_ccpav3_subset_066_to_072.nc')
apcp_anal <- ncvar_get(ncin, 'apcp_anal')

## subset data to same region
lons_anal <- ncvar_get(ncin, 'lons_anal')
lats_anal <- ncvar_get(ncin, 'lats_anal')
lon_idx <- which(lons_anal[,1] >= -91 &  lons_anal[,1] <= -81)
lat_idx <- which(lats_anal[1,] >= 30 &  lats_anal[1,] <= 40)
apcp_anal <- apcp_anal[lon_idx, lat_idx, ]
rm(lons_anal, lats_anal, lon_idx, lat_idx)

## create an array to store all each analysis-ensemble pair dim(lon, lat, time, obs/ens_n)
field_dat <- array(dim = c(dim(apcp_anal), 12))
field_dat[,,,1] <- apcp_anal

nc_close(ncin)
rm(ncin, apcp_anal)


# Ensembles (downscaled) --------------------------------------------------

## NOTE: Each array has space for 31 days, but 
#   - January 2002 starts from 01/02 (start timeframe one day earlier, and trim)
#   - April always has 30 days

m <- c(1, 4, 7, 10) # Jan, Apr, Jul, Oct
dates <- seq.Date(as.Date('2002-01-02'), as.Date('2015-12-30'), by='day')

files <- list.files(path='./data/GEFS_GSDM_CCPA', pattern='*.nc', full.names=TRUE)
lapply(seq_along(files), function(i) {
  ncin <- nc_open(files[i])
  fcsts <- ncvar_get(ncin, 'downscaled')
  
  lons_fcst <- ncvar_get(ncin, 'Longitude')
  lats_fcst <- ncvar_get(ncin, 'Latitude')
  lon_idx <- which(lons_fcst[,1] >= -91 &  lons_fcst[,1] <= -81)
  lat_idx <- which(lats_fcst[1,] >= 30 &  lats_fcst[1,] <= 40)
  
  # april
  if(m[i]==4){
    fcsts <- fcsts[,,,1:30,]
  }
  
  d <- dim(fcsts)
  dim(fcsts) <- c(d[1], d[2], d[3], d[4]*d[5])
  
  # january
  if(m[i]==1){
    fcsts <- fcsts[,,, -31]
  }
  
  date_idx <- which(month(dates)==m[i])
  for(j in 1:11) {
    jj <- j+1
    field_dat[,, date_idx, jj] <<- fcsts[lon_idx, lat_idx, j,]
  }
  
  nc_close(ncin)
  rm(ncin)
})
rm(files)

date_idx <- (month(dates) %in% m)
field_dat <- field_dat[,, date_idx,] 

save(field_dat, file="./data/gsdm_downscaled_fields.RData")


