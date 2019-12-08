#!/bin/bash
#SBATCH --nodes=1                               # Number of requested nodes
#SBATCH --time=0:05:00                          # Max wall time
#SBATCH --qos=testing                           # Specify testing QOS
#SBATCH --partition=shas-testing                # Specify Summit haswell nodes
#SBATCH --ntasks=1                              # Number of tasks per job
#SBATCH --job-name=test-job                     # Job submission name
#SBATCH --output=test-job.%j.out                # Output file name with Job ID


# Written by:	 Josh Jacobson
# Date:		     4 November 2018
# Purpose: 	   This script submits a simulation script for 5 realizations to the Slurm job scheduler to confirm that the output is as expected

# purge all existing modules
module purge

# load any modules needed to run your program
module load R

# The directory where you want the job to run
# cd /projects/$joja7332  ## this has alot more space...
# cd /home/$joja7332

# Run your program
R CMD BATCH --no-save --no-restore simulate_s1.R
