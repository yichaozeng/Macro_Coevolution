#!/bin/bash
#SBATCH --job-name=prop_col_ex
#SBATCH --ntasks=16
#SBATCH --nodes=1
#SBATCH --time=20:00:00   
#SBATCH --account=wiensj
#SBATCH --partition=standard


module load gnu8
module load R/4.0
Rscript Desktop/model_rcpp_hpc_OneDrive/control/prop_col_ex.R