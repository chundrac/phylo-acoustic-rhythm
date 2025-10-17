for i in {1..50}
do
    for modtype in 'BM_UV_dist' 'BM_UV_nondist' 'OU_UV_dist' 'OU_UV_nondist'
    do
	sbatch run_job.sh Rscript run_model.R $modtype $i
    done
done
