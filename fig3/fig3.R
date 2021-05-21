# Libraries
library(ggplot2)
library(dplyr)
source('config.R')

# Save some variables to control details in plot
font_scale = 8
three_part_colors = c('#66c2a5', '#fc8d62', '#8da0cb')

# Load data
load(file.path('data', 'samediff.RData'))

# Change Listener to subj 
data$subj = factor(data$subj)

# Relabel freq diffs to be in percent
data$freq_diff = factor(data$freq_diff, levels=c('0.5', '1', '2', '3'), labels=c('0.5%', '1%', '2%', '3%'))

# Plot
data %>%
  # Average data across conditions, calculate error bars 
  group_by(center_freq, freq_diff, trial_type, subj) %>%
  summarize(num_correct=mean(num_correct)) %>%
  group_by(center_freq, freq_diff, trial_type) %>%
  summarize(num_correct_error=sd(num_correct)/sqrt(n()), num_correct=mean(num_correct)) %>%
  mutate(p_corr=num_correct/2, p_corr_error=num_correct_error/2) %>%
  # Aesthetics
  ggplot(aes(x=center_freq, y=p_corr, 
             color=trial_type, shape=trial_type, 
             ymin=p_corr-p_corr_error, ymax=p_corr+p_corr_error)) +
  # Geoms
  geom_smooth(method='lm', se=FALSE) +
  geom_point() +
  geom_errorbar(width=0.15, linetype=1) + 
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
  scale_x_continuous(breaks=c(800, 900, 1000, 1100, 1200), labels=c('800', '', '1000', '', '1200')) + 
  scale_color_manual(values=three_part_colors) + 
  ylim(c(0.2, 1)) + 
  labs(x='Center Frequency (Hz)', 
       y='Proportion Correct', 
       color='Tone Order', 
       shape='Tone Order')

# Save to disk
ggsave(file.path('figs', 'fig3.png'), width=font_scale*0.75, height=font_scale*0.3)
