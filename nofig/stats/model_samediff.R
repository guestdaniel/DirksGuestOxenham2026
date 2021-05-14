# Libraries
library(ggplot2)
library(dplyr)
library(merTools)
library(car)
library(phia)
library(lmerTest)
library(effects)
library(optimx)

# Configure contrast settings
options(contrasts=c('contr.sum','contr.poly'))

# Load data
load(file.path('data', 'samediff.RData'))

# Prep the model data
model_data = data %>%
  dplyr::select(subj, trial_type, center_freq, freq_diff, num_trials, num_correct) %>%
  mutate(center_freq = center_freq-1000) %>%
  mutate(center_freq = center_freq/100) %>%
  mutate(freq_diff = factor(freq_diff))

# Model the data
model = glmer(cbind(num_correct, num_trials-num_correct) ~
                freq_diff*trial_type*center_freq + (1 + trial_type*center_freq|subj),
              data = model_data,
              family = binomial(link = 'logit'),
              control=glmerControl(optimizer='bobyqa'))

##### EVALUATE MODEL #####
# Residuals vs fitted values plot
png(file.path('outputs', 'statsmodel_samediff_residuals_vs_fitted_values.png'))
plot(model, 
type=c('p', 'smooth'), 
main='Residuals vs Fitted Values')
dev.off()

# Scale location plot
png(file.path('outputs', 'statsmodel_samediff_scale_location.png'))
plot(model, sqrt(abs(resid(.)))~fitted(.),
type=c('p', 'smooth'),
ylab=expression(sqrt(abs(resid))),
main='Scale Location Plot (sqrt(resid) vs fitted)')
dev.off()

# Binned residuals plot
png(file.path('outputs', 'statsmodel_sameidff_binned_residuals.png'))
arm::binnedplot(fitted(model), residuals(model))
dev.off()

# Create analysis plots
png(file.path('outputs', 'statsmodel_samediff_all_effects.png'))
plot(allEffects(model))
dev.off()

png(file.path('outputs', 'statsmodel_samediff_trial_type.png'))
plot(predictorEffect('trial_type', model))
dev.off()

png(file.path('outputs', 'statsmodel_samediff_center_freq.png'))
plot(predictorEffect('center_freq', model))
dev.off()

png(file.path('outputs', 'statsmodel_samediff_freq_diff.png'))
plot(predictorEffect('freq_diff', model))
dev.off()

##### EVALUATE MODEL #####
# Calcualte ANOVA
model_anova = Anova(model, type=2) # Note: for univariate responses, Anova does *not* correct p-values

# Hypothesis 1: Are the slopes significantly different than zero? 
test_1 = testInteractions(model, fixed=c('trial_type', 'freq_diff'), slope='center_freq', adjustment='none')

# Hypotheses 2: Does the bias exist?
test_2a = testInteractions(model, fixed='trial_type', slope='center_freq', adjustment='none')
test_2b = testInteractions(model, pairwise='trial_type', slope='center_freq', adjustment='none')

# Transform all test values to more suitable units (for simple factor contrasts, we get odds ratios)
test_1$Value = exp(test_1$Value)
test_2a$Value = exp(test_2a$Value)
test_2b$Value = exp(test_2b$Value)

# Correct p-values
# First correct ANOVA p-values
model_anova[, 'Pr(>Chisq)'] = p.adjust(model_anova[, 'Pr(>Chisq)'], method='holm')
# Second correct contrast p-values jointly
vals = p.adjust(c(test_1[, 'Pr(>Chisq)'], test_2a[, 'Pr(>Chisq)'], test_2b[, 'Pr(>Chisq)']), method='holm')
test_1[, 'Pr(>Chisq)'] = vals[1:12]
test_2a[, 'Pr(>Chisq)'] = vals[13:15]
test_2b[, 'Pr(>Chisq'] = vals[16:18]

# Report analyses
sink(file.path('outputs', 'statsmodel_samediff_results.txt'))
# Report ANOVA
print('#####################################################################################')
print('ANOVA, likelihood-ratio chisq tests, corrected by Holm-Bonferroni')
print('#####################################################################################')
model_anova
print('#####################################################################################')
print('Linear contrast tests for hypothesis 1 Are any center_freq slopes greater than zero?')
print('Significance test of center_freq slope in each condition, likelihood-ratio chisq tests (???), corrected by Holm-Bonferroni')
print(test_1)
print('#####################################################################################')
print('#####################################################################################')
print('Linear contrast tests for hypothesis 2 Does the bias exist, subtype 1')
print('Significance test of center_freq slope in each sign, likelihood-ratio chisq tests (???), corrected by Holm-Bonferroni')
print('#####################################################################################')
print(test_2a)
print('')
print('#####################################################################################')
print('Linear contrast tests for hypothesis 2 Does the bias exist, subtype 2')
print('Interaction contrast test of center_freq slope against sign, likelihood-ratio chisq tests (???), corrected by Holm-Bonferroni')
print('#####################################################################################')
print(test_2b)
print('')
sink()
