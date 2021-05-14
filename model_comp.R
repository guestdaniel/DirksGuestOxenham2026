library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)

# Set settings
font_scale=10
n_sim = 1000

# Load data

# Configure directories
if (Sys.info()['nodename'] == 'daniel-desktop'){
	load('/home/daniel/apc_store/pitchbias/data/2020-05-18/clean_data.RData')
	data_updown = data
	load('/home/daniel/apc_store/pitchbias/data/2020-05-04/clean_same_diff_data.RData')
	data_samediff = data
	model_dir = '/home/daniel/apc_store/pitchbias/model/comp'
	plot_dir = '/home/daniel/apc_store/pitchbias/figures'
	source('~/pitchbias/fit_criterion_shift_model.R')
	source('~/pitchbias/fit_sensory_bias_model.R')
} else {
	# Coral can put her directories here
}
dir.create(file.path(model_dir, Sys.Date()))

# Create function to perform model boostrapping
boot_models <- function(data_updown, data_samediff) {
	print('Resampling updown data')
	subset_updown = data_updown[sample.int(n=nrow(data_updown), size=nrow(data_updown), replace=TRUE), ] %>%
		group_by(freq_diff, tone_freq_int1, tone_freq_int2, target_tone, sign) %>%
		summarize(num_trials = sum(num_trials), num_correct=sum(num_correct))
	print('Resampling samediff data')
	subset_samediff = data_samediff[sample.int(n=nrow(data_samediff), size=nrow(data_samediff), replace=TRUE), ] %>%
		group_by(freq_diff, tone_freq_int1, tone_freq_int2, trial_type) %>%
		summarize(num_trials = sum(num_trials), num_correct=sum(num_correct))
	print('Fitting model')
	res_sensory = fit_sensory_model(subset_updown, subset_samediff, version=2)
	res_criterion = fit_criterion_model(subset_updown, subset_samediff, version=3)
	return(list(list(res_sensory[[1]]$fmin, res_sensory[[1]]$x, res_sensory[[2]]),
		    list(res_criterion[[1]]$fmin, res_criterion[[1]]$x, res_criterion[[2]])))
}

# Bootstrap models
#model_bootstraps = replicate(n=n_sim, boot_models(data_updown, data_samediff))

# Save/load model bootstrapping results
#save(model_bootstraps, file=file.path(model_dir, Sys.Date(), 'bootstraps.RData'))
#load(file=file.path(model_dir, Sys.Date(), 'bootstraps.RData'))
load(file=file.path(model_dir, '2020-08-27', 'bootstraps.RData'))

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
		summarize(SSE=sum((y-y_hat)^2),
		          LL=sum(-1*(num_correct * log10(y_hat) + (num_trials - num_correct) * log10(1 - y_hat))))
	temp$idx = sim
	results = rbind(results, data.frame(temp))
}

# Calculate differece in SSE
comparison_SSE = results %>% select(-LL) %>%
	group_by(experiment, idx) %>% 
	pivot_wider(names_from='model', values_from='SSE') %>%
	summarize(diff=sensory-criterion)

# Calculate difference in LLR
comparison_LLR = results %>% select(-SSE) %>%
	group_by(experiment, idx) %>% 
	pivot_wider(names_from='model', values_from='LL') %>%
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
sink(file.path(model_dir, Sys.Date(), "results.txt"))

# Calculate quantiles of SSE for two experiemnts
print("SSE difference")
quantile(as.numeric(comparison_SSE[comparison_SSE$experiment == 'samediff', ]$diff), c(0.05, 0.5, 0.95))
quantile(as.numeric(comparison_SSE[comparison_SSE$experiment == 'updown', ]$diff), c(0.05, 0.5, 0.95))

# Calculate quantiles of LLR for two experiemnts
print("LLR difference")
quantile(as.numeric(comparison_LLR[comparison_LLR$experiment == 'samediff', ]$diff), c(0.05, 0.5, 0.95))
quantile(as.numeric(comparison_LLR[comparison_LLR$experiment == 'updown', ]$diff), c(0.05, 0.5, 0.95))

# Output mean + quantiles
results %>% group_by(model, parameter) %>% summarize(mean=mean(value), lower=quantile(value, c(0.05)), upper=quantile(value, c(0.95)))

sink()
