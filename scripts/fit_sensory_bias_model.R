fit_sensory_model <- function(data_updown, data_samediff, version=1, starting_vals=c(3, 0.025, 0, 0.1, 0)) {
# Fit sensory bias model to data
#
# Arguments:
# 	data_updown (data.frame): data from the up-down discrimination experiment (output of preprocess_updown_data.R)
# 	data_samediff (data.frame): data from the same-diff discrimination experiment (output of preprocess_samediff.R)
# 	version (int): which version of the model to fit
# 		1: fix mean at 1000 Hz, fix performance during inattention to chance (rather than include parameter 5), 
# 			free parameters = (2) (3) (4), exclude (5)
# 		2: let mean be a free parameter, fix performance during attention to chance (rather than include parameter 5) 
# 			free parameters = (1) (2) (3) (4), exclude (5)
# 		3: fix mean at 1000 Hz, include parameter 5
# 			free parameters = (2) (3) (4) (5)
# 		4: let all parameters be free!
# 			free parameters = (1) (2) (3) (4) (5)
# 	starting_vals (numeric vector):  initial values for parameters in estimation procedure
#
# Returns:
# 	(list): contains two elements, first is the output of pracma::fminsearch and second is data.frame with data 
#		and prediction

# Create "second_correct" variable as numeric analogue of sign variable
data_updown[data_updown$sign == "High-Low", "second_correct"] = -1
data_updown[data_updown$sign == "Low-High", "second_correct"] = 1
# Label experiments and transform freq_diff into a numeric()
data_updown$experiment = "updown"
data_updown$freq_diff = as.numeric(as.character(data_updown$freq_diff))
data_samediff$experiment = "samediff"
# Bind updown and samediff data together
data = bind_rows(data_updown, data_samediff)

# Log transform frequency values in data_updown and data_samediff
data$tone_freq_int1 = log10(data$tone_freq_int1)
data$tone_freq_int2 = log10(data$tone_freq_int2)

# Define starting value and bounds bounds on free parameters:
# 	(1) perceived mean of frequencies, log10[Hz], (-infty, infty)
# 	(2) weight of the perceived mean against true mean, proportion, (0, 1)
# 	(3) frequency difference limen or discrimination threshold, log10(f'/f), (-infty, infty)
# 	(4) proportion of time not attending to the task, proportion, (0, 1)
# 	(5) discrimination threshold for the wrong question (whether tone pair above or below the mean), log10(f'/f) ? 

if (version == 4) { # Everything is free
	lower = c(1, 0, -3, 0.0, -3)
	upper = c(5, 1, 1, 0.3, 6)
} else if (version == 3) { # Perceived mean is fixed at 3 = log(1000)
	lower = c(3, 0, -3, 0.0, -3)
	upper = c(3, 1, 1, 0.3, 6)
} else if (version == 2) { # Inattention is chance
	lower = c(1, 0.005, -3, 0.0, 0)
	upper = c(5, 1, 1, 0.3, 0)
} else if (version == 1) { # Perceived mean is fixed at 3 = log(1000), inattention is chance
	lower = c(3, 0, -3, 0.0, 0)
	upper = c(3, 1, 1, 0.3, 0)
} else {
	print("That's not a version!")
}

# Define function that implements model
model_fun <- function(p, data, version, switch) {
	#########################################
	# Handle up-down data first 		    #
	#########################################
	data_updown = data[data$experiment == "updown", ]
	# First, calculate the perceived difference between f2 and f1
	internal_difference = (data_updown$tone_freq_int2-data_updown$tone_freq_int1-p[2]*(p[1]-data_updown$tone_freq_int1))
	# Normalize that difference by the sensitivity to f2-f1
	internal_difference = internal_difference/(10^p[3])/sqrt(2)
	# Second, calculate the perceived internal difference between mean of f2 and f1 and the perceived mean 
	internal_difference_mean = (1/2*(data_updown$tone_freq_int2+data_updown$tone_freq_int1)-p[1])
	# Normalize that difference by the sensitivity to deviance from mean f
	internal_difference_mean = internal_difference_mean/(10^p[5])/sqrt(2)
	# Calculate y_hat as mixture of percent correct from correct question answering and incorrect question answering
	if (version == 2 | version == 1) {
		y_hat_updown = (1-p[4])*pnorm(data_updown$second_correct*internal_difference) + p[4]*0.5
	} else {
		y_hat_updown = (1-p[4])*pnorm(data_updown$second_correct*internal_difference) + p[4]*pnorm(data_updown$second_correct*internal_difference_mean)
	}

	#########################################
	# Handle same-diff data 		        #
	#########################################
	data_samediff = data[data$experiment == "samediff", ]
	# First, calculate the perceived interval difference between f2 and f1
	internal_difference = (data_samediff$tone_freq_int2-data_samediff$tone_freq_int1-p[2]*(p[1]-data_samediff$tone_freq_int1))
	# Normalize that difference by the sensitivity to f2-f1
	internal_difference = internal_difference/(10^p[3])/sqrt(2)
	# Next, calculate k
	k = numeric(length(internal_difference))
	temp = data_samediff %>% group_by(freq_diff) %>% filter(trial_type == 'same tones') %>% summarize(FA = 1 - sum(num_correct)/sum(num_trials)) %>% mutate(tau = -sqrt(2) * qnorm(FA/2))
	k[data_samediff$freq_diff == 0.5] = temp[1,]$tau/sqrt(2)
	k[data_samediff$freq_diff == 1.0] = temp[2,]$tau/sqrt(2)
	k[data_samediff$freq_diff == 2.0] = temp[3,]$tau/sqrt(2)
	k[data_samediff$freq_diff == 3.0] = temp[4,]$tau/sqrt(2)
	k_same = k[data_samediff$trial_type == "same tones"]
	k_diff = k[data_samediff$trial_type != "same tones"]
	# Calculate y_hat as mixture of percent correct from correct question answering and incorrect question answering
	internal_difference_same = internal_difference[data_samediff$trial_type == "same tones"]
	internal_difference_diff = internal_difference[data_samediff$trial_type != "same tones"]
	y_hat_same = (1-p[4])*(pnorm(internal_difference_same + k_same) - pnorm(internal_difference_same - k_same)) +
		p[4]*(0.5)
	y_hat_diff = (1-p[4])*(1 - pnorm(internal_difference_diff + k_diff) + pnorm(internal_difference_diff - k_diff)) +
		p[4]*(0.5)
	y_hat_samediff = numeric(nrow(data_samediff))
	y_hat_samediff[data_samediff$trial_type == "same tones"] = y_hat_same
	y_hat_samediff[data_samediff$trial_type != "same tones"] = y_hat_diff

	#########################################
	# Combine up-down and samediff 		#
	#########################################
	y = data$num_correct/data$num_trials
	y_hat = c(y_hat_updown, y_hat_samediff)

	# Return SSE
	if (switch == 1) {
		return(sum((y - y_hat)^2))
	} else {
		return(y_hat)
	}
}

# Estimate model fit to data
fitted = pracma::fminsearch(fn=model_fun, x0=starting_vals, data=data, version=version, switch=1, lower=lower, upper=upper, method="Hooke-Jeeves")

# Extract y_hat from model with fitted parameters
y_hat = model_fun(fitted$xmin, data, version=version, switch=2)
data$y_hat = y_hat

# Return results
return(list(fitted, data))
}
