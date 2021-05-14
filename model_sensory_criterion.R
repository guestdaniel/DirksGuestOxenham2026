library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)

# Configure directories
if (Sys.info()['nodename'] == 'daniel-desktop'){
	code_dir = '/home/daniel/pitchbias'
	data_dir = '/home/daniel/apc_store/pitchbias/data/2020-05-18'
	boot_dir = '/home/daniel/apc_store/pitchbias'
	output_path = '/home/daniel/apc_store/pitchbias/model_results.txt'
} else {
	# Coral can put her directories here
}

# Source
source(file.path(data_dir, 'fit_sensory_bias_model.R'))
source(file.path(data_dir, 'fit_criterion_shift_model.R'))

# Set settings
font_scale=10
n_sim = 1000

#############################################################################################################################
# FIT MODEL ONCE TO ORIGINAL DATASET AND SAVE RESULTS
#############################################################################################################################

# Load data
load(file.path(data_dir, 'clean_data.RData'))
data_updown = data
load(file.path(data_dir, 'clean_data_same_diff.RData'))
data_samediff = data

# Peprocess data
subset_updown = data_updown %>%
	group_by(freq_diff, tone_freq_int1, tone_freq_int2, target_tone, sign) %>%
	summarize(num_trials = sum(num_trials), num_correct=sum(num_correct))
subset_samediff = data_samediff %>%
	group_by(freq_diff, tone_freq_int1, tone_freq_int2, trial_type) %>%
	summarize(num_trials = sum(num_trials), num_correct=sum(num_correct))

# Fit data
res_sensory = fit_sensory_model(subset_updown, subset_samediff, version=2)
res_criterion = fit_criterion_model(subset_updown, subset_samediff, version=3)

# Output estimates
sink(output_path)
print('Sensory bias model')
res_sensory[[1]]
print('Criterion shift model')
res_criterion[[1]]
sink()

#############################################################################################################################
# LOAD BOOTSTRAP SIMULATIONS AND SAVE BOOSTRAP ANALYSIS
#############################################################################################################################

load(file.path(boot_dir, 'boostraps.RData')) # CARE FOR TYPO

# Extract updown and samediff SSE
results = data.frame()
params = data.frame()
for (sim in 1:n_sim) {
	# Extract results for this simulation
	res_sensory = model_bootstraps[1, sim][[1]][[3]]
	param_sensory = model_bootstraps[1, sim][[1]][[2]]
	res_criterion = model_bootstraps[2, sim][[1]][[3]]
	param_criterion = model_bootstraps[2, sim][[1]][[2]]

	# HANDLE SSE/LL ANALYSIS
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

	# HANDLE PARAMS
	param = rbind(param_sensory, param_criterion)
	param = data.frame(param)
	colnames(param) = c("x1", "x2", "x3", "x4", "x5")
	param$model = c("Sensory", "Criterion")
	param$idx = sim
	params = rbind(params, param)
}

# Histograms
params %>% ggplot(aes(x=x3, fill=model)) + 
	geom_histogram() +
	facet_grid(model ~ . )
