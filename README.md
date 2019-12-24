# Fraction of Threshold Exceedance

[![DOI](https://zenodo.org/badge/226600439.svg)](https://zenodo.org/badge/latestdoi/226600439)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3592495.svg)](https://doi.org/10.5281/zenodo.3592495)


This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Overview

This repository contains the R code used for simulation, data processing, and figure generation in Jacobson et al. (2020).

*Important note:* Throughout the simulation code, correlation length is represented by the variable `s`, whereas the paper denotes correlation length as `a`.

#### Data
The corresponding data repository containing simulation results and forecast data can be found [here](https://doi.org/10.5281/zenodo.3592495
). The repository contains the following
- Simulation results: Each .RData file contains eleven sets of 5000 samples of fraction of threshold exceedance (FTE) values for a simulated verification field and corresponding 11-member ensemble. The verification field's correlation length `s_1` is fixed according to the file name (e.g. `_s2.5` indicates correlation length 2.5), and the correlation length of the ensemble mean `s_2` ranges from `0.5*s_1` to `1.5*s_1` by `0.1*s_1`, yielding the eleven simulation sets per file.
- `refcstv2_precip_ccpav3_subset_066_to_072.nc`: GEFS coarse resolution reforecast data and corresponding CCPA analyses.
- `GSDM_downscaled_*_066_to_072.nc`: GEFS forecast data from January, April, July, and October, downscaled to CCPA resolution according to a simplified version of the Gibbs sampling disaggregation model (GSDM).

Code used to process simulation results and forecast-analyses data can be found in `process_results.R` and `process_gefs.R`, respectively.


#### `sim`
Directory containing simulation code and shell scripts used to carry out the simulation experiment.


#### `fig`
Code used to produce all figures.


## Suggested citation
