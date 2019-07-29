inference:
	sbt "runMain probprog.Lm; runMain probprog.RandomEffects; runMain probprog.SimulateMixtureModel; runMain probprog.MixtureModel"

plots:
	Rscript scripts/plots.r
