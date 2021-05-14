library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)
library(lme4)
library(car)

# Set settings
font_scale=10

# Load data
load(file.path('data', 'updown.RData'))
data_updown = data
load(file.path('data', 'updown.RData'))
data_samediff = data
source('scripts/fit_jnd.R')

# Relabel levels of practice
data_updown$practice = factor(data_updown$practice, levels=levels(data_updown$practice), labels=c("Session 1", "Session 2", "Session 3", "Session 4"))

# Fit models to behavioral data
fits = list()
# Call fit_jnd from scripts/fit_jnd.R to estimate JNDs from up-down data
for (ii in 1:4) {
	fits[[ii]] = fit_jnd(data_updown[data_updown$practice == levels(data_updown$practice)[ii], ])
}

# Estimate model fit curves as a function of delta_f in percent
df_percent = seq(0, 5, length.out=50)
curves = data.frame(df_percent=numeric(), y_hat=numeric(), practice=character())
for (ii in 1:4) {
	lambda = fits[[ii]][[1]]$x[2]
	alpha = 10^(fits[[ii]][[1]]$x[1])
	curves = rbind(curves, data.frame(df_percent=df_percent, 
					  y_hat=lambda*pnorm(log10(df_percent/100 + 1)/alpha/sqrt(2)) + (1-lambda)*(1/2),
					  practice=levels(data_updown$practice)[ii]))
}
curves$freq_diff = curves$df_percent
curves$p_corr_mean = curves$y_hat
curves$p_corr_se = NA

# Transform data_updown$freq_diff to numeric
data_updown$freq_diff = as.numeric(as.character(data_updown$freq_diff))

# Construct dataframe containing average of behavioral data in different delta_f and practice conditions
temp = data_updown %>%
	group_by(freq_diff, practice, subj) %>%
        summarize(num_correct=sum(num_correct), num_trials=sum(num_trials)) %>%
        mutate(p_corr = num_correct/num_trials) %>%
	group_by(freq_diff, practice) %>%
	summarize(p_corr_se = sd(p_corr)/sqrt(n()), p_corr_mean = mean(p_corr))

# Plot data
temp %>%
  ggplot(aes(x=freq_diff, y=p_corr_mean, color=practice, ymin=p_corr_mean-1.96*p_corr_se, ymax=p_corr_mean+1.96*p_corr_se)) + 
  # Annotate Unpracticed
  annotate(geom="segment", x=100*(10^(sqrt(2) * 10^(fits[[1]][[1]]$xmin[1]) * qnorm((0.707 - 1/2*(1-fits[[1]][[1]]$xmin[2]))/(fits[[1]][[1]]$xmin[2]))) - 1),
	  		   xend=100*(10^(sqrt(2) * 10^(fits[[1]][[1]]$xmin[1]) * qnorm((0.707 - 1/2*(1-fits[[1]][[1]]$xmin[2]))/(fits[[1]][[1]]$xmin[2]))) - 1),
			   y=0.4, yend=0.707,
			   linetype="dashed",
			   size=1.0,
			   color="#F8766D") + 
  # Annotate Practiced 1
  annotate(geom="segment", x=100*(10^(sqrt(2) * 10^(fits[[2]][[1]]$xmin[1]) * qnorm((0.707 - 1/2*(1-fits[[2]][[1]]$xmin[2]))/(fits[[2]][[1]]$xmin[2]))) - 1),
	  		   xend=100*(10^(sqrt(2) * 10^(fits[[2]][[1]]$xmin[1]) * qnorm((0.707 - 1/2*(1-fits[[2]][[1]]$xmin[2]))/(fits[[2]][[1]]$xmin[2]))) - 1),
			   y=0.4, yend=0.707,
			   linetype="dashed",
			   size=1.0,
			   color="#7CAE00") + 
  # Annotate Practiced 2
  annotate(geom="segment", x=100*(10^(sqrt(2) * 10^(fits[[3]][[1]]$xmin[1]) * qnorm((0.707 - 1/2*(1-fits[[3]][[1]]$xmin[2]))/(fits[[3]][[1]]$xmin[2]))) - 1),
	  		   xend=100*(10^(sqrt(2) * 10^(fits[[3]][[1]]$xmin[1]) * qnorm((0.707 - 1/2*(1-fits[[3]][[1]]$xmin[2]))/(fits[[3]][[1]]$xmin[2]))) - 1),
			   y=0.4, yend=0.707,
			   linetype="dashed",
			   size=1.0,
			   color="#00BFC4") + 
  # Annotate Practiced 3
  annotate(geom="segment", x=100*(10^(sqrt(2) * 10^(fits[[4]][[1]]$xmin[1]) * qnorm((0.707 - 1/2*(1-fits[[4]][[1]]$xmin[2]))/(fits[[4]][[1]]$xmin[2]))) - 1),
	  		   xend=100*(10^(sqrt(2) * 10^(fits[[4]][[1]]$xmin[1]) * qnorm((0.707 - 1/2*(1-fits[[4]][[1]]$xmin[2]))/(fits[[4]][[1]]$xmin[2]))) - 1),
			   y=0.4, yend=0.707,
			   linetype="dashed",
			   size=1.0,
			   color="#C77CFF") + 
  # Handle geoms
  geom_line(data=curves, size=1.5) +
  geom_errorbar(width=0.15, size=1) + 
  geom_point(size=3) + 
  geom_hline(yintercept=0.707) + 
  # Handle themeing
  theme_bw() +
  theme(axis.text.y=element_text(size=1*font_scale),       # axis tick label font size
        axis.text.x=element_text(size=1*font_scale),
        axis.title.y=element_text(size=1.2*font_scale),    # axis label font size
        axis.title.x=element_text(size=1.2*font_scale),
        legend.text=element_text(size=1*font_scale),       # legend text font size
        legend.title=element_text(size=1.2*font_scale),    # legend title font size
        strip.text.x=element_text(size=1*font_scale),      # facet label font size
        plot.title=element_text(size=1.4*font_scale),      # figure title font size
        legend.key.height=unit(0.4, 'cm'),                 # Adjust legends
        legend.key.size=unit(3, 'lines'),
        panel.grid.major=element_blank(),                  # Hide panel grids
        panel.grid.minor = element_blank()) +
  # Labels
  ylim(c(0.4, 1)) +
	scale_y_continuous(expand=c(0, 0)) +
  labs(x="Frequency Difference (%)", 
       y="Proportion Correct", 
       color="Tone Order", 
       linetype="Source",
       shape="Source")

# Save to disk
ggsave(file.path('figs', 'fig1.png'), width=8, height=6)
