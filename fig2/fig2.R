# Libraries
library(ggplot2)
library(dplyr)

# Save some variables to control details in plot
font_scale = 8
colormap = c('#f7fcfd','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6','#8c6bb1','#88419d','#810f7c','#4d004b')

# Load data
load(file.path('data', 'updown.RData'))

# Relabel sessions
data$practice = factor(data$practice, levels=c('Unpracticed', 'Practice Session 1', 'Practice Session 2', 'Practice Session 3'),
		             labels=c('Session 1', 'Session 2', 'Session 3', 'Session 4'))

# Plot
data %>%
	# Average data across conditions, calculate error bars 
	group_by(center_freq, practice, target_tone, freq_diff, sign, subj) %>%
	summarize(num_correct=mean(num_correct)) %>%
	group_by(center_freq, practice, target_tone, freq_diff, sign) %>%
	summarize(num_correct_error=sd(num_correct)/sqrt(n()), num_correct=mean(num_correct)) %>%
	mutate(p_corr=num_correct/2, p_corr_error=num_correct_error/2) %>%
	# Plot aesthetics
	ggplot(aes(x=center_freq, y=p_corr, color=freq_diff, shape=sign, linetype=sign, ymin=p_corr-p_corr_error, ymax=p_corr+p_corr_error)) + 
		# Geoms
		geom_point() +
		geom_errorbar(width=0.15, linetype=1) + 
		geom_smooth(method='lm', se=FALSE) +
		facet_grid(practice ~ target_tone) + 
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
		# Colors
		scale_color_manual(values=colormap[3:9]) + 
	        # Labels
		labs(x='Center Frequency (Hz)', 
		     y='Proportion Correct', 
		     color='Frequency\nDifference (%)', 
		     linetype='Order',
		     shape='Order')
# Save to disk
ggsave(file.path('figs', 'fig2.png'), width=5, height=7)
