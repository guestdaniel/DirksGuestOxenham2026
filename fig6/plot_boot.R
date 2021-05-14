library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)

# Set settings
font_scale=10
n_sim = 1000

# Load data

# Configure directories
if (Sys.info()['nodename'] == 'daniel-desktop'){
	model_dir = '/home/daniel/apc_store/pitchbias/model/comp'
	plot_dir = '/home/daniel/apc_store/pitchbias/figures'
} else {
	# Coral can put her directories here
}

# Load in model bootstraps
load(file=file.path(model_dir, '2020-08-27', 'bootstraps.RData'))

# Extract updown and samediff SSE
results = data.frame()
for (sim in 1:n_sim) {
	# Extract results for this simulation
	res_sensory = model_bootstraps[1, sim][[1]][[3]]
	res_criterion = model_bootstraps[2, sim][[1]][[3]]
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
}

# Compute difference between models (SSE)
comparison_SSE = results %>% select(-LL) %>%
	group_by(experiment, idx) %>% 
	pivot_wider(names_from='model', values_from='SSE') %>%
	summarize(diff=sensory-criterion)
as.data.frame(comparison_SSE) %>% mutate(experiment=factor(experiment, labels=c("Same-Different", "Up-Down"))) %>%
	ggplot(aes(x=diff)) + 
	geom_histogram() + 
	geom_vline(xintercept=0, linetype='dashed', color='red') + 
	facet_grid(experiment ~ .) +
	theme_bw() +
	xlab('Difference in Sum of Squared Errors') +
	ylab('Count') + 
	xlim(c(0, 0.3)) 
ggsave(file.path(plot_dir, Sys.Date(), 'comp_model_SSE.png'))

# Compute difference between models (LLR)
comparison_LLR = results %>% select(-SSE) %>%
	group_by(experiment, idx) %>% 
	pivot_wider(names_from='model', values_from='LL') %>%
	summarize(diff=sensory-criterion)
as.data.frame(comparison_LLR) %>% mutate(experiment=factor(experiment, labels=c("Same-Different", "Up-Down"))) %>%
	ggplot(aes(x=diff)) + 
	geom_histogram() + 
	facet_grid(experiment ~ .) +
	theme_bw() +
	xlab('Log Likelihood Ratio') +
	ylab('Count')
ggsave(file.path(plot_dir, Sys.Date(), 'comp_model_LLR.png'))

# Visualize + outputs
results %>% ggplot(aes(x=value)) + geom_histogram() + facet_wrap(model ~ parameter, scales="free", nrow=2)
ggsave(file.path(plot_dir, Sys.Date(), 'comp_model_param_histograms1.png'))
results %>% ggplot(aes(x=value, color=model)) + geom_density() + facet_wrap(. ~ parameter, scales="free", nrow=1)
ggsave(file.path(plot_dir, Sys.Date(), 'comp_model_param_histograms2.png'))


