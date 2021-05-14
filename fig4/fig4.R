library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)

# Source scripts
source('scripts/fit_criterion_shift_model.R')
source('scripts/fit_sensory_bias_model.R')

# Set settings
font_scale=8
three_part_colors = c('#66c2a5', '#fc8d62', '#8da0cb')

# Load data
load(file.path('data', 'updown.RData'))
data_updown = data
load(file.path('data', 'samediff'))
data_samediff = data

# Fit both models with 5 free parameter
model_sensory_bias = fit_sensory_model(data_updown, data_samediff, version=2)
model_criterion_shift = fit_criterion_model(data_updown, data_samediff, version=3)

# Save results to disk
save(model_sensory_bias, file='nofig/comp_models/results.RData')

# Plot fit (UPDOWN)
# Extract data frames w/ fit from model objects
data_sensory_bias = model_sensory_bias[[2]]
data_criterion_shift = model_criterion_shift[[2]]
# Create y
data_sensory_bias$y = data_sensory_bias$num_correct/data_sensory_bias$num_trials
# Append y_hat_shift from sensory bias model fit
data_sensory_bias$y_hat_shift = data_criterion_shift$y_hat
# Pivot to have raw data and sensory bias fit 
temp = data_sensory_bias[data_sensory_bias$exp == "updown", ]
temp = temp %>% pivot_longer(cols=c("y", "y_hat", "y_hat_shift"))
temp[temp$name == "y", "src"] = "Behavior"
temp[temp$name == "y_hat", "src"] = "Sensory Bias"
temp[temp$name == "y_hat_shift", "src"] = "Criterion Shift"
temp[temp$src == "Behavior", "supername"] = "Behavior"
temp[temp$src != "Behavior", "supername"] = "Model"
# Fix center freq so all are divisible by 2
temp[mod(temp$center_freq, 2) != 0, "center_freq"] = temp[mod(temp$center_freq, 2) != 0, "center_freq"] + 1
temp = temp %>%
	group_by(src, supername, center_freq, freq_diff, sign, subj) %>%
	summarize(p_corr=mean(value)) %>%
	group_by(src, supername, center_freq, freq_diff, sign) %>%
	summarize(p_corr_error=sd(p_corr)/sqrt(n()), p_corr=mean(p_corr))
temp %>% ggplot(aes(x=center_freq, y=p_corr, color=sign, ymin=p_corr-p_corr_error, ymax=p_corr+p_corr_error, linetype=src)) + 
		geom_point(data=temp[temp$src == "Behavior", ]) +
		geom_errorbar(width=0.15, linetype=1) + 
		geom_line(data=temp[temp$supername == "Model", ]) + 
		facet_grid(. ~ freq_diff) + 
		# Themes
		theme_bw() +
		theme(axis.text.y=element_text(size=1*font_scale),   # axis tick label font size
		  axis.text.x=element_text(size=1*font_scale, angle=45, hjust=1),
		  axis.title.y=element_text(size=1.2*font_scale),    # axis label font size
		  axis.title.x=element_text(size=1.2*font_scale),
		  legend.text=element_text(size=1*font_scale),     # legend text font size
		  legend.title=element_text(size=1.2*font_scale),  # legend title font size
		  strip.text.x=element_text(size=1*font_scale),    # facet label font size
		  plot.title=element_text(size=1.4*font_scale),      # figure title font size
		  legend.key.height=unit(0.4, 'cm'),
		  legend.key.size=unit(3, 'lines'),
		  panel.grid.major=element_blank(), 
		  panel.grid.minor = element_blank()) +
	        # Labels
		scale_x_continuous(breaks=c(800, 900, 1000, 1100, 1200), labels=c("800", "", "1000", "", "1200")) + 
		scale_linetype_manual(values=c("solid", "solid", "dashed")) + 
		ylim(c(0.2, 1)) + 
		labs(x="Center Frequency (Hz)", 
		     y="Proportion Correct", 
		     color="Tone Order", 
		     linetype="Source")
ggsave(file.path(figure_dir, 'comp_model_updown.png'), width=8, height=3)

# Plot fit (SAMEDIFF)
# Extract data frames w/ fit from model objects
data_sensory_bias = model_sensory_bias[[2]]
data_criterion_shift = model_criterion_shift[[2]]
# Create y
data_sensory_bias$y = data_sensory_bias$num_correct/data_sensory_bias$num_trials
# Append y_hat_shift from sensory bias model fit
data_sensory_bias$y_hat_shift = data_criterion_shift$y_hat
# Pivot to have raw data and sensory bias fit 
temp = data_sensory_bias[data_sensory_bias$exp == "samediff", ]
temp = temp %>% pivot_longer(cols=c("y", "y_hat", "y_hat_shift"))
temp[temp$name == "y", "src"] = "Behavior"
temp[temp$name == "y_hat", "src"] = "Sensory Bias"
temp[temp$name == "y_hat_shift", "src"] = "Criterion Shift"
temp[temp$src == "Behavior", "supername"] = "Behavior"
temp[temp$src != "Behavior", "supername"] = "Model"
temp = temp %>%
	group_by(src, supername, center_freq, freq_diff, trial_type, subj) %>%
	summarize(p_corr=mean(value)) %>%
	group_by(src, supername, center_freq, freq_diff, trial_type) %>%
	summarize(p_corr_error=sd(p_corr)/sqrt(n()), p_corr=mean(p_corr))
temp %>%
	ggplot(aes(x=center_freq, y=p_corr, color=trial_type, ymin=p_corr-p_corr_error, ymax=p_corr+p_corr_error, linetype=src, shape=trial_type, group=interaction(trial_type, src))) + 
		geom_point(data=temp[temp$src == "Behavior", ]) +
		#geom_errorbar(data=temp[temp$src == "Behavior", ], width=0.15, linetype=1) + 
		geom_line(data=temp[temp$supername == "Model", ]) + 
		facet_grid(. ~ freq_diff) + 
		# Themes
		theme_bw() +
		theme(axis.text.x=element_text(size=1*font_scale, angle=45, hjust=1),   # axis tick label font size
		  axis.text.y=element_text(size=1*font_scale),
		  axis.title.y=element_text(size=1.2*font_scale),    # axis label font size
		  axis.title.x=element_text(size=1.2*font_scale),
		  legend.text=element_text(size=1*font_scale),     # legend text font size
		  legend.title=element_text(size=1.2*font_scale),  # legend title font size
		  strip.text.x=element_text(size=1*font_scale),    # facet label font size
		  plot.title=element_text(size=1.4*font_scale),      # figure title font size
		  legend.key.height=unit(0.4, 'cm'),
		  legend.key.size=unit(3, 'lines'),
		  panel.grid.major=element_blank(), 
		  panel.grid.minor = element_blank()) +
	        # Labels
		scale_x_continuous(breaks=c(800, 900, 1000, 1100, 1200), labels=c("800", "", "1000", "", "1200")) + 
		scale_linetype_manual(values=c("solid", "solid", "dashed")) + 
		scale_color_manual(values=three_part_colors) + 
		ylim(c(0.2, 1)) + 
		labs(x="Center Frequency (Hz)", 
		     y="Proportion Correct", 
		     color="Tone Order", 
		     linetype="Source",
		     shape="Tone Order")
ggsave(file.path(figure_dir, 'comp_model_samediff.png'), width=6, height=3)
