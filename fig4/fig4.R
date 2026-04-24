library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)
source('config.R')

# Set settings
font_scale=8
three_part_colors = c('#66c2a5', '#fc8d62', '#8da0cb')

# Load data
load(file.path('data', 'updown.RData'))
data_updown = data
load(file.path('data', 'samediff.RData'))
data_samediff = data

# Save results to disk
load(file.path('outputs', 'fitted_comp_sensory_bias_model.RData'))
load(file.path('outputs', 'fitted_comp_criterion_shift_model.RData'))

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
	summarize(p_corr_error=sd(p_corr)/sqrt(n()), p_corr=mean(p_corr)) %>%
	mutate(freq_diff=factor(freq_diff, levels=c('0', '0.5', '0.75', '1', '1.5', '2', '5'),
	                        labels=c('0%', '0.5%', '0.75%', '1%', '1.5%', '2%', '5%')))

# Create  copy of temp to remove behavior category
temp_model = temp[temp$supername == "Model", ]
temp_model$src = factor(temp_model$src)
temp_model$model = temp_model$src

temp %>% ggplot(aes(x=center_freq, y=p_corr, color=sign, ymin=p_corr-p_corr_error, ymax=p_corr+p_corr_error)) + 
		geom_point(data=temp[temp$src == "Behavior", ]) +
		geom_errorbar(width=0.15, linetype=1) + 
		geom_line(aes(linetype=model), data=temp_model) + 
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
          legend.position="bottom",
          legend.box="horizontal",
          legend.direction="vertical",
          legend.spacing.x=unit(0.5, 'cm'),
		  panel.grid.major=element_blank(), 
		  panel.grid.minor = element_blank()) +
	        # Labels
		scale_x_continuous(breaks=c(800, 900, 1000, 1100, 1200), labels=c("800", "", "1000", "", "1200")) + 
		ylim(c(0.2, 1)) + 
		labs(x="Center Frequency (Hz)", 
		     y="Proportion Correct", 
		     color="Tone Order", 
		     linetype="Model") +
        guides(color=guide_legend(override.aes = list(linetype = 0)))
ggsave(file.path('figs', 'fig4.png'), width=font_scale*0.8, height=font_scale*0.4)

# # Supporting analysis (# subjects in each panel)
# temp = data_sensory_bias[data_sensory_bias$exp == "updown", ]
# temp = temp %>% pivot_longer(cols=c("y", "y_hat", "y_hat_shift"))
# temp[temp$name == "y", "src"] = "Behavior"
# temp[temp$name == "y_hat", "src"] = "Sensory Bias"
# temp[temp$name == "y_hat_shift", "src"] = "Criterion Shift"
# temp[temp$src == "Behavior", "supername"] = "Behavior"
# temp[temp$src != "Behavior", "supername"] = "Model"
# temp[mod(temp$center_freq, 2) != 0, "center_freq"] = temp[mod(temp$center_freq, 2) != 0, "center_freq"] + 1

# temp %>%
# 	group_by(src, supername, freq_diff) %>%
# 	summarize(N=length(unique(subj)))