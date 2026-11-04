library(dplyr)

# Load data
load(file.path('data', 'samediff.RData'))

# Calculate same-different decision criterion tau from false-alarm rate in same-tone trials (Eq. 8)
tau = data %>%
	filter(trial_type == 'same tones') %>%
	group_by(freq_diff) %>%
	summarize(FA = 1 - sum(num_correct)/sum(num_trials)) %>%
	mutate(tau = -sqrt(2) * qnorm(FA/2))

print(as.data.frame(tau))
