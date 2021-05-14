fit_jnd <- function(data, starting_vals=c(0, 0.8)) {
# Fit sensory bias model to data
#
# Arguments:
# data (data.frame): data from the up-down discrimination experiment (output of preprocess_updown_data.R)
# starting_vals (vector, numeric): vector of initial parameter values, where elements are:
#	(1) log10 of frequency difference limen 
# 	(2) proportion of time attending to the task
#
# Returns:
# (list): contains two elements:
#	(1) output of pracma::fminsearch and second is data.frame with data and prediction
#	(2) data.frame with data and predicted values  
  
# Select relevant columns from data and data_samediff
data$freq_diff = as.numeric(as.character(data$freq_diff))

# Add column indicating proportion correct
data$y = data$num_correct/data$num_trials
  
# Define upper and lower bounds on free parameters:
# 	(1) log10 of frequency difference limen
# 	(2) proportion of time attending to the task
lower = c(-3, 0.6)
upper = c(3, 1)

# Define function that implements model
model_fun <- function(p, data, switch) {

	# Calculate decision variable as frequency difference normalized by the sensitivity to f'/f
	internal_difference = log10(data$freq_diff/100 + 1)/10^(p[1])/sqrt(2)

	# Calculate y_hat as mixture of percent correct from correct question answering and incorrect question answering
	y_hat = p[2]*pnorm(internal_difference) + (1-p[2])*0.5

	# Return sum of squared errors
	if (switch == 1) {
	loglikelihood = sum((data$num_correct/data$num_trials - y_hat)^2)
		return(loglikelihood)
	} else {
		return(y_hat)
	}
}

# Estimate fit of model function to data
fitted = pracma::fminsearch(fn=model_fun, x0=starting_vals, data=data, switch=1, lower=lower, upper=upper, method="Hooke-Jeeves")

# Calcualte y_hat from results
y_hat = model_fun(fitted$xmin, data, switch=2)
data$y_hat = y_hat

# Return results
return(list(fitted, data))
}
