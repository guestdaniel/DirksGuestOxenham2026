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
options(contrasts=c("contr.sum","contr.poly"))

# Load data
load(file.path('data', 'updown.RData'))

# Prep the model data
model_data = data %>%
	# Select variables of interest
	dplyr::select(subj, target_tone, practice, sign, center_freq, freq_diff, num_trials, num_correct) %>%
	mutate(center_freq = center_freq-1000) %>%
	mutate(center_freq = center_freq/100)

# Model the data (each experiment separately)
models = list()
subsets = c('Unpracticed', 'Practice Session 1', 'Practice Session 2', 'Practice Session 3')
for (ii in 1:4) {
	models[[ii]] = glmer(cbind(num_correct, num_trials-num_correct) ~ 
		    		freq_diff*sign*target_tone*center_freq + (1 + sign + center_freq:sign|subj), 
			     data=model_data[model_data$practice == subsets[ii], ], 
			     family=binomial(link="logit"),
			     control=glmerControl(optimizer="bobyqa",
						  optCtrl=list(maxfun=100000)))
}

# Model the data (unpracticed vs practiced)
temp = model_data %>%
	filter(practice == "Unpracticed" | practice == "Practice Session 1")
models[[5]] = glmer(cbind(num_correct, num_trials-num_correct) ~ 
		    		freq_diff*sign*target_tone*center_freq*practice + (1 + sign + center_freq:sign|subj), 
			     data=temp,
			     family=binomial(link="logit"),
			     control=glmerControl(optimizer="bobyqa",
				optCtrl=list(maxfun=100000)))

# Model the data (practice session 2 vs practice session 3)
temp = model_data %>%
	filter(practice == "Practice Session 2" | practice == "Practice Session 3") %>%
	filter(freq_diff == 0.75 | freq_diff == 1.5)
models[[6]] = glmer(cbind(num_correct, num_trials-num_correct) ~ 
	    			freq_diff*sign*target_tone*center_freq*practice + (1 + sign + center_freq:sign|subj), 
			     data=temp,
			     family=binomial(link="logit"),
			     control=glmerControl(optimizer="bobyqa",
				optCtrl=list(maxfun=100000)))

# Model the data (joint)
models[[7]] = glmer(cbind(num_correct, num_trials-num_correct) ~ 
	    		freq_diff*sign*target_tone*center_freq + (1 + sign + center_freq:sign|subj), 
		     data=model_data, 
		     family=binomial(link="logit"),
		     control=glmerControl(optimizer="bobyqa",
					  optCtrl=list(maxfun=100000)))


# Create empty storage for test results
tests_h1 = list()
tests_h1_interaction = list()
tests_h3 = list()
anovas = list()

##### EVALUATE MODEL #####
for (ii in 1:7) {
	print(ii)
	# Extract model
	mod = models[[ii]]
	# Create model subdirectory
	model_dir_sub = file.path('outputs', paste0('statsmodel_updown_', as.character(ii)))
	dir.create(model_dir_sub, recursive=TRUE)

	# Create model fit plots
	# Residuals vs fitted values plot
	png(file.path(model_dir_sub, "residuals_vs_fitted_values.png"))
	plot(mod, 
		 type=c("p", "smooth"), 
		 main="Residuals vs Fitted Values")
	dev.off()
	# Absolute residual vs sqrt residuals (scale location)  plot
	png(file.path(model_dir_sub, "scale_location.png"))
	plot(mod, sqrt(abs(resid(.)))~fitted(.),
		 type=c("p", "smooth"),
		 ylab=expression(sqrt(abs(resid))),
		 main="Scale Location Plot (sqrt(resid) vs fitted)")
	dev.off()
	# QQ plot (of questionable utility for near-Bernoulli binomial regression)
	png(file.path(model_dir_sub, "qq.png"))
	qqnorm(scale(resid(mod)), 
		 main="QQ Plot of Normalized Resiudals")
	abline(c(0,1))
	dev.off()
	# Binned residuals plot
	png(file.path(model_dir_sub, "binned_residuals.png"))
	arm::binnedplot(fitted(models[[ii]]), residuals(models[[ii]]))
	dev.off()

	# Calculate analysis
	anovas[[ii]] = Anova(mod, type=2) # Note: implicitly is NOT correcting p-values

	# Hypothesis 1: Does the bias exist?
	tests_h1[[ii]] = testInteractions(models[[ii]], fixed="sign", slope="center_freq", adjustment="none", link=TRUE)
	tests_h1_interaction[[ii]] = testInteractions(models[[ii]], pairwise="sign", slope="center_freq", adjustment="none", link=TRUE)

	# Hypothesis 3: Does direction of bias depend on which tone is target tone
	tests_h3[[ii]] = testInteractions(models[[ii]], pairwise="target_tone", fixed="sign", slope="center_freq", adjustment="none", link=TRUE)
}

# Hypotheses 2: Does the magnitude of the bias change over time?
test_2a = testInteractions(models[[5]], pairwise="practice", fixed="sign", slope="center_freq", adjustment="none", link=TRUE)
test_2b = testInteractions(models[[6]], pairwise="practice", fixed="sign", slope="center_freq", adjustment="none", link=TRUE)

# Transform all test values to more suitable units (for simple factor contrasts, we get odds ratios)
for (ii in 1:7) {
	tests_h1[[ii]]$Value = exp(tests_h1[[ii]]$Value)
	tests_h1_interaction[[ii]]$Value = exp(tests_h1_interaction[[ii]]$Value)
	tests_h3[[ii]]$Value = exp(tests_h3[[ii]]$Value)
}
test_2a$Value = exp(test_2a$Value)
test_2b$Value = exp(test_2b$Value)

# Correct p-values
# First correct ANOVA values
for (ii in 1:7) {
	anovas[[ii]][, "Pr(>Chisq)"] = p.adjust(anovas[[ii]][, "Pr(>Chisq)"], method="holm")
}
# Next correct contrast p-values
for (ii in 1:7) {
	if (ii == 5) {
		vals = p.adjust(c(tests_h1[[ii]][, "Pr(>Chisq)"], tests_h1_interaction[[ii]][, "Pr(>Chisq)"], tests_h3[[ii]][, "Pr(>Chisq)"], test_2a[, "Pr(>Chisq)"]), method="holm")
		tests_h1[[ii]][, "Pr(>Chisq)"] = vals[1:2]
		tests_h1_interaction[[ii]][, "Pr(>Chisq)"] = vals[3:3]
		tests_h3[[ii]][, "Pr(>Chisq)"] = vals[4:5]
		test_2a[, "Pr(>Chisq)"] = vals[6:7]

	} else if (ii == 6) {
		vals = p.adjust(c(tests_h1[[ii]][, "Pr(>Chisq)"], tests_h1_interaction[[ii]][, "Pr(>Chisq)"], tests_h3[[ii]][, "Pr(>Chisq)"], test_2b[, "Pr(>Chisq)"]), method="holm")
		tests_h1[[ii]][, "Pr(>Chisq)"] = vals[1:2]
		tests_h1_interaction[[ii]][, "Pr(>Chisq)"] = vals[3:3]
		tests_h3[[ii]][, "Pr(>Chisq)"] = vals[4:5]
		test_2b[, "Pr(>Chisq)"] = vals[6:7]
	} else {
		vals = p.adjust(c(tests_h1[[ii]][, "Pr(>Chisq)"], tests_h1_interaction[[ii]][, "Pr(>Chisq)"], tests_h3[[ii]][, "Pr(>Chisq)"]), method="holm")
		tests_h1[[ii]][, "Pr(>Chisq)"] = vals[1:2]
		tests_h1_interaction[[ii]][, "Pr(>Chisq)"] = vals[3:3]
		tests_h3[[ii]][, "Pr(>Chisq)"] = vals[4:5]
	}
}

# Label models
labels = c('Unpracticed', 
	   'Practice Session 1', 
	   'Practice Session 2', 
	   'Practice Session 3', 
	   'Unpracticed + Practice Session 1', 
	   'Practice Session 2 + Practice Session 3, 0.75% and 1.5%',
	   'Joint');

# Report analyses
sink(file.path('outputs', "statsmodel_updown_results.txt"))
# Report ANOVA
print('#####################################################################################')
print('Per-model ANOVA, Wald Chisq test, corrected by Holm-Bonferroni')
print('#####################################################################################')
for (ii in 1:7){
	print(paste0('----- ', labels[[ii]], ' -----'))
	print(anovas[[ii]])
	print('')
}
# Report H1 Tests
print('#####################################################################################')
print('Linear contrast tests for hypothesis 1 "Does the bias exist", subtype 1')
print('Significance test of center_freq slope in each sign, Wald Chisq test, corrected by Holm-Bonferroni')
print('#####################################################################################')
for (ii in 1:7){
	print(paste0('----- ', labels[[ii]], ' -----'))
	print(tests_h1[[ii]])
	print('')
}
print('#####################################################################################')
print('Linear contrast tests for hypothesis 1 "Does the bias exist", subtype 2')
print('Interaction contrast test of center_freq slope against sign, Wald Chisq test, corrected by Holm-Bonferroni')
print('#####################################################################################')
for (ii in 1:7){
	print(paste0('----- ', labels[[ii]], ' -----'))
	print(tests_h1_interaction[[ii]])
	print('')
}
# Report H2 Tests
print('#####################################################################################')
print('Linear contrast tests for hypothesis 2 "Does the bias change over time')
print('Interaction contrast test of center_freq slope against practice in each sign, Wald Chisq test, corrected by Holm-Bonferroni')
print('#####################################################################################')
print(paste0('----- ', labels[[5]], ' -----'))
print(test_2a)
print('')
print(paste0('----- ', labels[[6]], ' -----'))
print(test_2b)
print('')
# Report H3 Tests
print('#####################################################################################')
print('Linear contrast tests for hypothesis 3 "Does the bias depend on which tone is target tone')
print('Interaction contrast test of center_freq slope against target_tone in each sign, Wald chisq test, corrected by Holm-Bonferroni')
print('#####################################################################################')
for (ii in 1:7){
	print(paste0('----- ', labels[[ii]], ' -----'))
	print(tests_h3[[ii]])
	print('')
}
# Close out file
sink()

