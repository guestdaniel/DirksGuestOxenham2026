library(dplyr)
library(tidyr)

# Load in model bootstraps
load(file=file.path('outputs', 'comp_model_bootstraps.RData'))

# Extract updown and samediff SSE
results = data.frame()
for (sim in 1:n_sim) {
	# Extract results for this simulation
	res_sensory = model_bootstraps[1, sim][[1]][[3]]
	res_criterion = model_bootstraps[2, sim][[1]][[3]]
	# Add y
	res_sensory$y = res_sensory$num_correct/res_sensory$num_trials
	res_criterion$y = res_criterion$num_correct/res_criterion$num_trials
	# Concatenate
	res_sensory$model = 'sensory'
	res_criterion$model = 'criterion'
	res = rbind(res_sensory, res_criterion)
	# Determine SSE
	temp = res %>% group_by(model, experiment) %>%
		summarize(SSE=sum((y-y_hat)^2))
	temp$idx = sim
	results = rbind(results, data.frame(temp))
}

# Calculate differece in SSE
comparison_SSE = results %>%
	group_by(experiment, idx) %>%
	pivot_wider(names_from='model', values_from='SSE') %>%
	summarize(diff=sensory-criterion)

# Extract all parameter estimates
results = data.frame()
pnames = c("p1", "p2", "p3", "p4", "p5")
for (sim in 1:n_sim) {
	# Extract results for this simulation
	res_sensory = model_bootstraps[1, sim][[1]][[2]]
	res_criterion = model_bootstraps[2, sim][[1]][[2]]
	# Loop through and add params
	for (ii in 1:5) {
		results = rbind(results, data.frame(parameter=pnames[ii], value=res_sensory[ii], model="sensory"))
		results = rbind(results, data.frame(parameter=pnames[ii], value=res_criterion[ii], model="criterion"))
	}
}

# Outputs
sink(file.path('outputs', 'bootstrap_analysis.txt'))

# Calculate quantiles of SSE for two experiemnts
print("SSE difference")
quantile(as.numeric(comparison_SSE[comparison_SSE$experiment == 'samediff', ]$diff), c(0.05, 0.5, 0.95))
quantile(as.numeric(comparison_SSE[comparison_SSE$experiment == 'updown', ]$diff), c(0.05, 0.5, 0.95))

# Output mean + quantiles
results %>% group_by(model, parameter) %>% summarize(mean=mean(value), lower=quantile(value, c(0.05)), upper=quantile(value, c(0.95)))

sink()
