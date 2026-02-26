for i in {1..10}
do
    for modtype in pred.dist pred.nodist nopred.dist nopred.nodist
    do
	sbatch run_job.sh Rscript run_model_frequency.R $modtype $i
	sbatch run_job.sh Rscript run_model_dominant_frequency.R $modtype $i
    done
done

