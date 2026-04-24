# Quick and dirty script to remove Listener names from data files
load(file.path('outputs', 'fitted_comp_sensory_bias_model.RData'))
data = model_sensory_bias[[2]]
data$subj = as.factor(data$subj)
levels(data$subj) = 1:length(levels(data$subj))
model_sensory_bias[[2]] = data
save(model_sensory_bias, file=file.path('outputs', 'fitted_comp_sensory_bias_model_clean.RData'))

load(file.path('outputs', 'fitted_comp_criterion_shift_model.RData'))
data = model_criterion_shift[[2]]
data$subj = as.factor(data$subj)
levels(data$subj) = 1:length(levels(data$subj))
model_criterion_shift[[2]] = data
save(model_criterion_shift, file=file.path('outputs', 'fitted_comp_criterion_shift_model_clean.RData'))
