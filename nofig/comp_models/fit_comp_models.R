library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)

# Source scripts
source('scripts/fit_criterion_shift_model.R')
source('scripts/fit_sensory_bias_model.R')

# Load data
load(file.path('data', 'updown.RData'))
data_updown = data
load(file.path('data', 'samediff.RData'))
data_samediff = data

# Fit both sensory bias and criterion shift model with 5 free parameter
model_sensory_bias = fit_sensory_model(data_updown, data_samediff, version=2)
model_criterion_shift = fit_criterion_model(data_updown, data_samediff, version=3)

# Save results to disk
save(model_sensory_bias, file=file.path('outputs', 'fitted_comp_sensory_bias_model.RData'))
save(model_criterion_shift, file=file.path('outputs', 'fitted_comp_criterion_shift_model.RData'))

data_updown %>% group_by(center_freq, freq_diff, sign, target_tone, subj) %>%
  summarize(p_corr=mean(num_correct)) %>%
  group_by(center_freq, freq_diff, target_tone, sign) %>%
  summarize(p_corr=mean(p_corr)) %>%
  ggplot(aes(x=center_freq, y=p_corr, color=sign)) + geom_point() + geom_smooth() + facet_grid(target_tone ~ freq_diff)