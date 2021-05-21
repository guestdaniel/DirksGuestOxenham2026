library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)
source('config.R')

# Load in model bootstraps
load(file=file.path('outputs', 'comp_model_bootstraps.RData'))

# Extract sum of squared errors (SSE) from both datasets across all bootstraps
results = data.frame()
for (sim in 1:dim(model_bootstraps)[2]) {
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
		summarize(SSE=sum((y-y_hat)^2))
	temp$idx = sim
	results = rbind(results, data.frame(temp))
}

# Compute difference between models in terms of difference of SSE
comparison_SSE = results %>%
	group_by(experiment, idx) %>% 
	pivot_wider(names_from='model', values_from='SSE') %>%
	summarize(diff=sensory-criterion)

# Visualize difference of SSE
as.data.frame(comparison_SSE) %>% mutate(experiment=factor(experiment, levels=c('samediff', 'updown'), labels=c('Up-down', 'Same-different'))) %>%
	ggplot(aes(x=diff)) + 
	geom_histogram() + 
	geom_vline(xintercept=0, linetype='dashed', color='red') + 
	facet_grid(experiment ~ .) +
	theme_bw() +
	theme(axis.text.y=element_text(size=1*font_scale),   # axis tick label font size
			axis.text.x=element_text(size=1*font_scale, angle=45, hjust=1),
			axis.title.y=element_text(size=1.2*font_scale),    # axis label font size
			axis.title.x=element_text(size=1.2*font_scale),
			legend.text=element_text(size=1*font_scale),     # legend text font size
			legend.title=element_text(size=1.2*font_scale),  # legend title font size
			strip.text.x=element_text(size=1*font_scale),    # facet label font size
			plot.title=element_text(size=1.4*font_scale),      # figure title font size
			panel.grid.major=element_blank(),
			panel.grid.minor = element_blank()) +
	xlab('Difference in Sum of Squared Errors') +
	ylab('Count') + 
	#xlim(c(-0.5, 0.5))  +
	scale_y_continuous(expand=c(0, 0))
ggsave(file.path('figs', 'fig6a.png'), width=3, height=4, dpi=300)

# Extract all parameter estimates
results = data.frame()
pnames = c("p1", "p2", "p3", "p4", "p5")
for (sim in 1:dim(model_bootstraps)[2]) {
	# Extract results for this simulation
	res_sensory = model_bootstraps[1, sim][[1]][[2]]
	res_criterion = model_bootstraps[2, sim][[1]][[2]]
	# Loop through and add params
	for (ii in 1:5) {
		results = rbind(results, data.frame(parameter=pnames[ii], value=res_sensory[ii], model="sensory"))
		results = rbind(results, data.frame(parameter=pnames[ii], value=res_criterion[ii], model="criterion"))
	}
}

# Rename the models to be publication-ready
results$model = factor(results$model, levels=c('sensory', 'criterion'), labels=c('Sensory Bias', 'Criterion Shift'))

# Visualize parameters 1, 3, and 4 side-by-side
results %>% filter(parameter == 'p1') %>%
	ggplot(aes(x=10^value)) +
		geom_histogram() +
		geom_vline(xintercept=1000, linetype='dashed', color='red') +
		facet_grid(model ~ .) +
		theme_bw() +
		  theme(axis.text.y=element_text(size=1*font_scale),   # axis tick label font size
			axis.text.x=element_text(size=1*font_scale, angle=45, hjust=1),
			axis.title.y=element_text(size=1.2*font_scale),    # axis label font size
			axis.title.x=element_text(size=1.2*font_scale),
			legend.text=element_text(size=1*font_scale),     # legend text font size
			legend.title=element_text(size=1.2*font_scale),  # legend title font size
			strip.text.x=element_text(size=1*font_scale),    # facet label font size
			plot.title=element_text(size=1.4*font_scale),      # figure title font size
			panel.grid.major=element_blank(),
			panel.grid.minor = element_blank()) +
		xlab('Frequency (Hz)') +
		ylab('Count') +
		xlim(c(850, 1150)) +
		scale_y_continuous(expand=c(0, 0))
ggsave(file.path('figs', 'fig6b.png'), width=3, height=4, dpi=300)

results %>% filter(parameter == 'p3') %>%
	ggplot(aes(x=100*(10^(10^(value)) - 1))) +
		geom_histogram() +
		facet_grid(model ~ .) +
		theme_bw() +
		  theme(axis.text.y=element_text(size=1*font_scale),   # axis tick label font size
			axis.text.x=element_text(size=1*font_scale, angle=45, hjust=1),
			axis.title.y=element_text(size=1.2*font_scale),    # axis label font size
			axis.title.x=element_text(size=1.2*font_scale),
			legend.text=element_text(size=1*font_scale),     # legend text font size
			legend.title=element_text(size=1.2*font_scale),  # legend title font size
			strip.text.x=element_text(size=1*font_scale),    # facet label font size
			plot.title=element_text(size=1.4*font_scale),      # figure title font size
			panel.grid.major=element_blank(),
			panel.grid.minor = element_blank()) +
		xlab('Alpha (%)') +
		ylab('Count') +
		xlim(c(0.65, 0.85)) +
		scale_y_continuous(expand=c(0, 0))
ggsave(file.path('figs', 'fig6c.png'), width=3, height=4, dpi=300)

results %>% filter(parameter == 'p4') %>%
	ggplot(aes(x=value)) +
		geom_histogram() +
		facet_grid(model ~ .) +
		theme_bw() +
		  theme(axis.text.y=element_text(size=1*font_scale),   # axis tick label font size
			axis.text.x=element_text(size=1*font_scale, angle=45, hjust=1),
			axis.title.y=element_text(size=1.2*font_scale),    # axis label font size
			axis.title.x=element_text(size=1.2*font_scale),
			legend.text=element_text(size=1*font_scale),     # legend text font size
			legend.title=element_text(size=1.2*font_scale),  # legend title font size
			strip.text.x=element_text(size=1*font_scale),    # facet label font size
			plot.title=element_text(size=1.4*font_scale),      # figure title font size
			panel.grid.major=element_blank(),
			panel.grid.minor = element_blank()) +
		xlab('Lambda (proportion)') +
		ylab('Count') +
		xlim(c(0.1, 0.2)) +
		scale_y_continuous(expand=c(0, 0))
ggsave(file.path('figs', 'fig6d.png'), width=3, height=4, dpi=300)
