library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)
library(lme4)
library(car)
library(nlme)
library(phia)
options(contrasts=c("contr.sum","contr.poly"))

# Set settings
font_scale=10

# Load data
# Configure directories
if (Sys.info()['nodename'] == 'daniel-desktop'){
  load('/home/daniel/apc_store/pitchbias/data/2020-05-18/clean_data.RData')
  data_updown = data
  load('/home/daniel/apc_store/pitchbias/data/2020-05-04/clean_same_diff_data.RData')
  data_samediff = data
  data_dir = '~/apc_store/pitchbias/data'
  model_dir = '~/apc_store/pitchbias/model/jnd'
  source('~/pitchbias/fit_jnd.R')
} else if (Sys.info()["nodename"] == 'Ethans-MacBook-Air.local') {
  load('/Users/Ethan/Documents/GitHub/pitchbias/data/2020-06-12/clean_data.RData')
  data_updown = data
  load('/Users/Ethan/Documents/GitHub/pitchbias/data/2020-06-12/clean_same_diff_data.RData')
  data_samediff = data
  data_dir = '/Users/Ethan/Documents/GitHub/pitchbias/data'
  model_dir = '/Users/Ethan/Documents/GitHub/pitchbias/model/jnd'
  setwd('/Users/Ethan/Documents/GitHub/pitchbias')
} else {
  
}

# Create model directory
model_dir = file.path(model_dir, Sys.Date())
dir.create(model_dir, recursive=TRUE)

# Make a new data frame with columns of subject, test session, target tone, and estimated JND
newdf = data_updown %>% 
  select(subj, practice, TargetTone) %>%
  unique()
newdf$alpha = NaN
num_rows = dim(newdf)[1]

# Loop through columns in newdf and fit JND model to corresponding subset of behavioral data
new_fits = list()
for (kk in 1:num_rows) {
    temp = subset(data_updown, subj == newdf$subj[kk] & practice == newdf$practice[kk] & TargetTone == newdf$TargetTone[kk])
    new_fits[[kk]] = fit_jnd(temp, c(-2, 0.8))
    newdf$alpha[kk] = new_fits[[kk]][[1]][[1]][[1]]
}

# Exclude outliers!
newdf = newdf %>% filter(alpha < 0) # excluded two outliers

# Fit LME to alpha values and test hypothesis of changes across practice conditions
lm_mod = lmer(alpha ~ practice*TargetTone + (1|subj), data=newdf)
lm_mod_anova = Anova(lm_mod, type=2, test="F") 

# Estimate contrast tests between conditions with Holm-Bonferroni correction
tests_h1 = testInteractions(lm_mod, pairwise="practice", adjustment="holm", test="F")
# Transform value of tests into ratio bnetween conditions (by default, we have alpha_2 - alpha_1, since alpha_2 and
# alpha_1 are in log units, we have log(JND_2) - log(JND_1) = log(JND_2/JND_1)... exponentiate to get ratio of JNDs)
tests_h1$Value = 10^(tests_h1$Value) # transform Value into ratio instead of log10(ratio) == log10(alpha_2) - log10(alpha_1)

# Output to disk
# Report analyses
sink(file.path(model_dir, "results.txt"))
lm_mod
print('')
lm_mod_anova
print('')
tests_h1
sink()
