#!/bin/bash
#SBATCH --job-name=prec4tsa
#SBATCH --output=logs/job_%A_%a.out
#SBATCH --error=logs/job_%A_%a.err
#SBATCH --array=1-20453%186          
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --nodes=1                # required on Ubelix
#SBATCH --mem=16G                # required on Ubelix
#SBATCH --time=00:90:00
#SBATCH --account=paygo
#SBATCH --wckey=GIUB_prec4tsa

# Load R (adjust if needed)
module load GDAL/3.11.1-foss-2025a
module load R/4.5.1-gfbf-2025a

# Map array index to day
start_date="1960-01-01"
run_date=$(date -d "$start_date + $((SLURM_ARRAY_TASK_ID-1)) days" +%Y-%m-%d)

# Run your script for ONE day
Rscript scripts/06_prec4sa/02_get-grids_obs_bc.R $run_date
