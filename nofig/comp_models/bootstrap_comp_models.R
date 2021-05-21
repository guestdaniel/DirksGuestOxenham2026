library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)

# Set bootstrap parameters
n_sim = 100

# Load data
load(file.path('data', 'updown.RData'))
data_updown = data
load(file.path('data', 'samediff.RData'))
data_samediff = data

# Source scripts
source(file.path('scripts', 'fit_criterion_shift_model.R'))
source(file.path('scripts', 'fit_sensory_bias_model.R'))

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
model_bootstraps = replicate(n=n_sim, boot_models(data_updown, data_samediff))

# Save model bootstrapping results
save(model_bootstraps, file=file.path('outputs', 'comp_model_bootstraps.RData'))

