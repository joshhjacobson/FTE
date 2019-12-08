
## Produce FTE histograms for upscaled GEFS reanalysis data

library(ncdf4)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)


# Fcst + Analysis ---------------------------------------------------------

## NOTE: dates range from 2002010200 to 2015123000

## grab field and time data
ncin <- nc_open('./data/refcstv2_precip_ccpav3_subset_066_to_072.nc')
fcst_ens <- ncvar_get(ncin, 'apcp_fcst_ens')
anal_upsc <- ncvar_get(ncin, 'apcp_anal_upsc')
init_anal <- ncvar_get(ncin, 'yyyymmddhh_init')

## get indices of data in upscaled region
lons_fcst <- ncvar_get(ncin, 'lons_fcst')
lats_fcst <- ncvar_get(ncin, 'lats_fcst')
lon_idx <- which(lons_fcst[,1] >= -91 &  lons_fcst[,1] <= -81)
lat_idx <- which(lats_fcst[1,] >= 30 &  lats_fcst[1,] <= 40)

## create an array to store each analysis-ensemble pair dim(lon, lat, time, obs/ens_n)
field_dat <- array(dim = c(length(lon_idx), length(lat_idx), length(init_anal), 12))

## populate and trim to same regions
field_dat[,,,1] <- anal_upsc[lon_idx, lat_idx, ]
field_dat[,,,2:12] <- fcst_ens[lon_idx, lat_idx, , ]

nc_close(ncin)
rm(ncin, anal_upsc, fcst_ens, init_anal, lats_fcst, lons_fcst, lat_idx, lon_idx)

