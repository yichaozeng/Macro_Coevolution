#!/bin/bash
#SBATCH --job-name=hybr_lik_noex
#SBATCH --ntasks=16
#SBATCH --nodes=1
#SBATCH --time=60:00:00   
#SBATCH --account=wiensj
#SBATCH --partition=standard


module load gnu8
module load R/4.0
Rscript Desktop/model_rcpp_hpc_OneDrive/control/hybr_lik_noex.R