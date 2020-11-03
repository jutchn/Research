#!/bin/bash -l

#SBATCH --partition=nonpre               # Partition                                                                                                               #SBATCH --requeue                        # Return job to the queue if preempted
#SBATCH --job-name=file               # Job name
#SBATCH --nodes=1                       # How many nodes
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16000                      # Memory in MB
#SBATCH --time=10:00:00                 # Run time limit, 72 hours at most
#SBATCH --output=slurm.%N.%j.out   # Output file
#SBATCH --error=slurm.%N.%j.err                                                                                                                                    #SBATCH --export=ALL

################################################################################################################
# !! Need to revise the following to merge time series data 09/05/2020
# we need to generate different versions of fMRI data
# 1) 4 runs of fMRI data mapped to different parcellations, i.e., output four txt files for each subject
# 2) different merged time-series data
################################################################################################################

export outdir=/scratch/jc2549/Research

for mg in $(find $outdir/HCP -name '*clean.dtseries.nii')
do
subid=$(echo "$mg" |awk -F '/' '{print $6}'| uniq)
temp=`echo "$mg"|rev|cut -d'/' -f2-|rev`
id=$(echo "$mg" |awk -F '/' '{print $9}'| uniq)

#DEMEAN ALL FOUR RUNS
srun wb_command -cifti-reduce $mg MEAN $temp/mean.dscalar.nii

#STDEV ALL FOUR RUNS
srun wb_command -cifti-reduce $mg STDEV $temp/stdev.dscalar.nii

#NORMALIZE ALL FOUR RUNS
srun wb_command -cifti-math "(x - mean) / stdev" "$temp/$id.dtseries.nii" -fixnan 0 -var x "$mg" -var mean $temp/mean.dscalar.nii -select 1 1 -repeat -var stdev $temp/stdev.dscalar.nii -select 1 1 -repeat

rm $mg

done

#find $outdir/HCP -name "*dscalar.nii*" -type f -delete
#find $outdir/HCP -name "*_clean.dtseries.nii" #-type f -delete
