#!/bin/bash
#SBATCH --nodes=1                               # Number of requested nodes
#SBATCH --time=168:00:00                        # Max wall time 7 days
#SBATCH --qos=long                              # Specify longer wall time
#SBATCH --partition=smem                        # Specify high memory job
#SBATCH --ntasks=1                              # Number of tasks per job
#SBATCH --job-name=run_s2_no_corr               # Job submission name
#SBATCH --output=run_s2_no_corr.%j.out          # Output file name with Job ID


# Written by:	 Josh Jacobson
# Date:		     9 June 2020
# Purpose: 	     This script submits a simulation script for 5000 realizations of the s1=2 parameter (with omega / xi = 0) to the Slurm job scheduler

# purge all existing modules
module purge

# load any modules needed to run your program
module load R

# The directory where you want the job to run
# cd /projects/$joja7332  ## this has alot more space...
# cd /home/$joja7332

# Run your program
R CMD BATCH --no-save --no-restore simulate_s2_no_corr.R
