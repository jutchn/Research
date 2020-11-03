#!/bin/bash -l

#SBATCH --partition=nonpre               # Partition                                                                                                               #SBATCH --requeue                        # Return job to the queue if preempted
#SBATCH --job-name=file               # Job name
#SBATCH --nodes=1                       # How many nodes
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32000                      # Memory in MB
#SBATCH --time=08:00:00                 # Run time limit, 72 hours at most
#SBATCH --output=slurm.%N.%j.out   # Output file
#SBATCH --error=slurm.%N.%j.err
#SBATCH --export=ALL

#module load connectomewb/1.3.2
module load MATLAB/R2020a
export outdir=/scratch/jc2549/Research

#CONVERT TO GIFTI FILES

for gnm in $(find $outdir -name '*REST*.ptseries.nii')
do
subid=$(echo "$gnm" |awk -F '/' '{print $5}'| uniq)
temp=`echo $gnm|rev|cut -d'.' -f2-|rev`
srun wb_command -cifti-convert -to-gifti-ext $gnm `echo "$temp.gii"`
done

#CONVERT TO TXT
cd /scratch/jc2549/Research/scripts

srun matlab -nodisplay -singleCompThread -r "getfn,exit"
srun matlab -nodisplay -singleCompThread -r "HCP,exit"

#find $outdir -name "*dtseries.nii*" -type f -delete
find $outdir/HCP -name "*gii*" -type f -delete
#find $outdir/HCP -name "*ptseries.nii*" -type f -delete


################################################################################################################
# !! Need to revise the following to merge time series data 09/05/2020
# we need to generate different versions of fMRI data
# 1) 4 runs of fMRI data mapped to different parcellations, i.e., output four txt files for each subject
# 2) different merged time-series data
################################################################################################################

#DEMEAN ALL FOUR RUNS
# wb_command -cifti-reduce $id MEAN $outdir/$subid/$fmn.mean.dscalar.nii

#wb_command -cifti-reduce ${ARRAY[${id}]} MEAN $outdir/$subid/$subid.$fmn.mean.dscalar.nii

#STDEV ALL FOUR RUNS
# wb_command -cifti-reduce $id STDEV $outdir/$subid/$fmn.stdev.dscalar.nii

#wb_command -cifti-reduce ${ARRAY[${id}]} STDEV $outdir/$subid/$subid.$fmn.stdev.dscalar.nii

#NORMALIZE ALL FOUR RUNS
# wb_command -cifti-math "(x - mean) / stdev" $outdir/$subid/$fmn.normalized.dtseries.nii -fixnan 0 -var x $id -var mean $outdir/$subid/$fmn.mean.dscalar.nii -select 1 1 -repeat -var stdev $outdir/$subid/$fmn.stdev.dscalar.nii -select 1 1 -repeat

#wb_command -cifti-math "(x - mean) / stdev" $outdir/$subid/$subid.$fmn.normalized.dtseries.nii -fixnan 0 -var x ${ARRAY[${id}]} -var mean $outdir/$subid/$subid.$fmn.mean.dscalar.nii -select 1 1 -repeat -var stdev $outdir/$subid/$subid.$fmn.stdev.dscalar.nii -select 1 1 -repeat


# the followings generate 5 versions of merged fMRI data
#for mg in $(find $outdir -name '*.ptseries.nii')
#do
#subid=$(echo "$mg" |awk -F '/' '{print $6}'| uniq)

#MERGE REST1 LR AND REST2 LR
#srun wb_command -cifti-merge $outdir/$subid/$subid.REST_12_LR.ptseries.nii -cifti $outdir/$subid/$subid.rfMRI_REST1_LR_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/$subid/$subid.rfMRI_REST2_LR_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 20 -up-to 1200

#MERGE REST1 RL AND REST2 RL
#srun wb_command -cifti-merge $outdir/$subid/$subid.REST_12_RL.ptseries.nii -cifti $outdir/$subid/$subid.rfMRI_REST1_RL_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/$subid/$subid.rfMRI_REST2_RL_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 20 -up-to 1200

#MERGE REST1 RL AND LR
#srun wb_command -cifti-merge $outdir/$subid/$subid.REST_1_RLLR.ptseries.nii -cifti $outdir/$subid/$subid.rfMRI_REST1_RL_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/$subid/$subid.rfMRI_REST1_LR_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 20 -up-to 1200

#MERGE REST2 LR AND RL
#srun wb_command -cifti-merge $outdir/$subid/$subid.REST_2_LRRL.ptseries.nii -cifti $outdir/$subid/$subid.rfMRI_REST2_LR_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 177 -up-to 1200 -cifti $outdir/$subid/$subid.rfMRI_REST2_RL_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 177 -up-to 1200

#MERGE ALL FOUR RUNS
#srun wb_command -cifti-merge $outdir/$subid/$subid.REST_MALL.ptseries.nii -cifti $outdir/$subid/$subid.rfMRI_REST1_RL_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 177 -up-to 1200 -cifti $outdir/$subid/$subid.rfMRI_REST1_LR_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 177 -up-to 1200 -cifti $outdir/$subid/$subid.rfMRI_REST2_LR_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 177 -up-to 1200 -cifti $outdir/$subid/$subid.rfMRI_REST2_RL_Atlas_hp2000_clean.dtseries.nii.ptseries.nii -column 177 -up-to 1200
#done
