#!/bin/bash -l

#SBATCH --partition=nonpre               # Partition                                                                                                               #SBATCH --requeue                        # Return job to the queue if preempted
#SBATCH --job-name=file               # Job name
#SBATCH --nodes=1                       # How many nodes
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16000                      # Memory in MB
#SBATCH --time=08:00:00                 # Run time limit, 72 hours at most
#SBATCH --output=slurm.%N.%j.out   # Output file
#SBATCH --error=slurm.%N.%j.err                                                                                                                                    #SBATCH --export=ALL

export outdir=/scratch/jc2549/Research

sub_id=/scratch/jc2549/Research/subjID_trim3.list

declare -a subj_array

while IFS= read -r line; do
    subj_array+=($line)
done < ${sub_id}

# the followings generate 6 versions of merged fMRI data
for((id=0 ; id<${#subj_array[@]} ; id++))
do
subid=${subj_array[$id]}
echo "$subid | $((id+1)) of ${#subj_array[@]}"

#MERGE REST1 LR AND REST2 LR
srun wb_command -cifti-merge $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST_Merge/rfMRI_REST12_LR.ptseries.nii -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST1_LR/rfMRI_REST1_LR.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST2_LR/rfMRI_REST2_LR.ptseries.nii -column 20 -up-to 1200

#MERGE REST1 RL AND REST2 RL
srun wb_command -cifti-merge $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST_Merge/rfMRI_REST12_RL.ptseries.nii -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST1_RL/rfMRI_REST1_RL.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST2_RL/rfMRI_REST2_RL.ptseries.nii -column 20 -up-to 1200

#MERGE REST1 RL AND LR
srun wb_command -cifti-merge $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST_Merge/rfMRI_REST1_RLLR.ptseries.nii -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST1_RL/rfMRI_REST1_RL.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST1_LR/rfMRI_REST1_LR.ptseries.nii -column 20 -up-to 1200

#MERGE REST2 LR AND RL
srun wb_command -cifti-merge $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST_Merge/rfMRI_REST2_LRRL.ptseries.nii -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST2_LR/rfMRI_REST2_LR.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST2_RL/rfMRI_REST2_RL.ptseries.nii -column 20 -up-to 1200

#MERGE REST1 RL + REST1 LR + REST2 LR + REST2 RL
srun wb_command -cifti-merge $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST_Merge/rfMRI_REST12_RLLR.ptseries.nii -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST1_RL/rfMRI_REST1_RL.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST1_LR/rfMRI_REST1_LR.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST2_LR/rfMRI_REST2_LR.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST2_RL/rfMRI_REST2_RL.ptseries.nii -column 20 -up-to 1200

#MERGE REST1 LR + REST1 RL + REST2 LR + REST2 RL
srun wb_command -cifti-merge $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST_Merge/rfMRI_REST12_LRRL.ptseries.nii -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST1_LR/rfMRI_REST1_LR.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST1_RL/rfMRI_REST1_RL.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST2_LR/rfMRI_REST2_LR.ptseries.nii -column 20 -up-to 1200 -cifti $outdir/HCP/$subid/MNINonLinear/Results/rfMRI_REST2_RL/rfMRI_REST2_RL.ptseries.nii -column 20 -up-to 1200

done
