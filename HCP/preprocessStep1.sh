#!/bin/bash -l

#SBATCH --partition=nonpre               # Partition
#SBATCH --requeue                        # Return job to the queue if preempted
#SBATCH --job-name=file               # Job name
#SBATCH --nodes=1                       # How many nodes
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8000                      # Memory in MB
#SBATCH --time=08:00:00                	# Run time limit, 72 hours at most
#SBATCH --output=slurm.%N.%j.out   # Output file
#SBATCH --error=slurm.%N.%j.err
#SBATCH --export=ALL

#module load connectomewb/1.3.2
module load MATLAB/R2020a
export outdir=/scratch/jc2549/Research

sub_id=/scratch/jc2549/Research/subjID_trim3.list

declare -a subj_array

while IFS= read -r line; do
    subj_array+=($line)
done < ${sub_id}

echo "the array contains ${#subj_array[@]} elements"

#ARRAYString=$(cmd < file ${sub_id} |tr "\n" " ")
#ARRAY=($ARRAYString)
#ELEMENTS=${#ARRAY[@]}

#START=$1
#END=$2

#${#subj_array[@]}

for((id=0 ; id<${#subj_array[@]} ; id++))
# for id in $(cat /project/zhaohcp/test/subid5.list)
do
#echo $id
#IFS='/' read -a my_array <<< "${ARRAY[${id}]}"
#ARRAY_LENGTH=$((${#my_array[@]-1}))
#subid=${my_array[$ARRAY_LENGTH -5]};
#fmn=${my_array[$ARRAY_LENGTH -1]};

#subid=$(echo "$id" |awk -F '/' '{print $5}'| uniq)
#fmn=$(echo "$id" |awk -F '/' '{print $8}')
#if [ ! -d "$outdir/$subid" ]; then
#mkdir $outdir/"$subid"
#fi

subid=${subj_array[$id]}
echo "$subid | $((id+1)) of ${#subj_array[@]}"
#PARCELLATION ALL FOUR RUNS
srun wb_command -cifti-parcellate $outdir/HCP/"$subid"/MNINonLinear/Results/rfMRI_REST2_LR/rfMRI_REST2_LR.dtseries.nii $outdir/ROIs/Q1-Q6_RelatedValidation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii COLUMN $outdir/HCP/"$subid"/MNINonLinear/Results/rfMRI_REST2_LR/rfMRI_REST2_LR.ptseries.nii

srun wb_command -cifti-parcellate $outdir/HCP/"$subid"/MNINonLinear/Results/rfMRI_REST2_RL/rfMRI_REST2_RL.dtseries.nii $outdir/ROIs/Q1-Q6_RelatedValidation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii COLUMN $outdir/HCP/"$subid"/MNINonLinear/Results/rfMRI_REST2_RL/rfMRI_REST2_RL.ptseries.nii

srun wb_command -cifti-parcellate $outdir/HCP/"$subid"/MNINonLinear/Results/rfMRI_REST1_LR/rfMRI_REST1_LR.dtseries.nii $outdir/ROIs/Q1-Q6_RelatedValidation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii COLUMN $outdir/HCP/"$subid"/MNINonLinear/Results/rfMRI_REST1_LR/rfMRI_REST1_LR.ptseries.nii 
     
srun wb_command -cifti-parcellate $outdir/HCP/"$subid"/MNINonLinear/Results/rfMRI_REST1_RL/rfMRI_REST1_RL.dtseries.nii $outdir/ROIs/Q1-Q6_RelatedValidation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii COLUMN $outdir/HCP/"$subid"/MNINonLinear/Results/rfMRI_REST1_RL/rfMRI_REST1_RL.ptseries.nii 
#rm -rf $outdir/$subid/$subid.$fmn.mean.dscalar.nii
#rm -rf $outdir/$subid/$subid.$fmn.stdev.dscalar.nii
#rm -rf $outdir/$subid/$subid.$fmn.normalized.dtseries.nii
done
