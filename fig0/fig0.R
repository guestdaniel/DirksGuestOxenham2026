# Create a schematic figure depicting the difference between the sensory and criterion-shfit models of the pitch bias effects
library(dplyr)
library(tidyr)
library(ggplot2)
source('config.R')

# Plot a single panel in the schematic, just showing a Gaussian differnce distribution
# Includes two vertical lines, one indicating an unbiased decision criterion and another
# showing a bias decision criterion
plot_difference_distribution = function(unbiased_k=0, biased_k=0, mu=0, sigma=1) {
    # Create  an x-axis to draw onto
	x = seq(-4*sigma - 1, 4*sigma + 1, length.out=500)
	dist = data.frame(x=x, y=dnorm(x, mean=mu, sd=sigma))

    # Create a dataframe to store the criterion daata (k)
	crit = data.frame(
		k=c(unbiased_k, biased_k),
		type=factor(c('unbiased k', 'biased k'), levels=c('unbiased k', 'biased k'))
	) %>% filter(!is.na(k))   # pass biased_k=NA to show only the unbiased criterion

	# Shade the "f2 higher" response region: everything above the criterion in use
	# (the biased k when present, otherwise the unbiased k)
	k_active = ifelse(is.na(biased_k), unbiased_k, biased_k)

    # Create the plot
	ggplot(dist, aes(x=x, y=y)) +
        # Draw filled area
		geom_area(data=filter(dist, x >= k_active), fill='#7FCDC4', alpha=0.5) +

        # Draw the Gaussian
		geom_line(color='#1F3A6E', linewidth=1) +

        # Draw the vertical lines
		geom_vline(data=crit, aes(xintercept=k, color=type, linetype=type), linewidth=1) +

        # Manually tweak the colors and linetypes to match Coral's first figure draft
		scale_color_manual(values=c('unbiased k'='gray40', 'biased k'='red')) +
		scale_linetype_manual(values=c('unbiased k'='dashed', 'biased k'='dotdash')) +

        # Scale and set themeing
		scale_x_continuous(breaks=0) +
		scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
		theme_void() +

        # Tweak axis
		theme(legend.position='none',
			  axis.line.x=element_line(color='black'),
			  axis.ticks.x=element_line(color='black'),
			  axis.ticks.length.x=unit(3, 'pt'),
			  axis.text.x=element_text(size=0.8*font_scale, margin=margin(t=2)),
			  axis.title.x=element_text(size=0.8*font_scale, margin=margin(t=4)),
			  plot.margin=margin(5, 10, 5, 10)) +
		xlab(expression(log[10](hat(f)[2]/hat(f)[1])))
}

# Plot a single panel in the same-different schematic, showing a Gaussian difference distribution
# Includes two vertical lines at +/- tau/2, the symmetric criteria for responding "different"
# (unlike k in the up-down task, these criteria do not depend on the direction of the difference)
plot_samediff_distribution = function(tau=2, mu=0, sigma=1) {
    # Create an x-axis to draw onto
	x = seq(-4*sigma - 1, 4*sigma + 1, length.out=500)
	dist = data.frame(x=x, y=dnorm(x, mean=mu, sd=sigma))

    # Create a dataframe to store the criterion data (+/- tau/2)
	crit = data.frame(k=c(-tau/2, tau/2))

    # Create the plot
	ggplot(dist, aes(x=x, y=y)) +
        # Draw filled areas (the "different" response region is both tails beyond +/- tau/2)
		geom_area(data=filter(dist, x <= -tau/2), fill='#7FCDC4', alpha=0.5) +
		geom_area(data=filter(dist, x >= tau/2), fill='#7FCDC4', alpha=0.5) +

        # Draw the Gaussian
		geom_line(color='#1F3A6E', linewidth=1) +

        # Draw the vertical lines (same style as the unbiased k in the up-down panels)
		geom_vline(data=crit, aes(xintercept=k), color='gray40', linetype='dashed', linewidth=1) +

        # Scale and set themeing
		scale_x_continuous(breaks=0) +
		scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
		theme_void() +

        # Tweak axis
		theme(legend.position='none',
			  axis.line.x=element_line(color='black'),
			  axis.ticks.x=element_line(color='black'),
			  axis.ticks.length.x=unit(3, 'pt'),
			  axis.text.x=element_text(size=0.8*font_scale, margin=margin(t=2)),
			  axis.title.x=element_text(size=0.8*font_scale, margin=margin(t=4)),
			  plot.margin=margin(5, 10, 5, 10)) +
		xlab(expression(log[10](hat(f)[2]/hat(f)[1])))
}

# Plot the inset schematic (simplified curve with only unbiased crtierion;
# labels to indicate meaning of shaded area are added separately in InkScape)
plot_inset_distribution = function(mu=0, sigma=1) {
	x = seq(mu - 4*sigma, mu + 4*sigma, length.out=500)
	dist = data.frame(x=x, y=dnorm(x, mean=mu, sd=sigma))
	ggplot(dist, aes(x=x, y=y)) +
		geom_area(data=filter(dist, x >= mu), fill='#7FCDC4', alpha=0.5) +   # "f2 higher" region
		geom_line(color='black', linewidth=1) +
		geom_vline(xintercept=mu, color='black', linewidth=1) +
		scale_y_continuous(expand=expansion(mult=c(0, 0.4))) +
		theme_void() +
		theme(plot.margin=margin(5, 10, 5, 10))
}

# Plot the same-different inset
plot_samediff_inset_distribution = function(tau=2, mu=0, sigma=1) {
	x = seq(mu - 4*sigma, mu + 4*sigma, length.out=500)
	dist = data.frame(x=x, y=dnorm(x, mean=mu, sd=sigma))
	ggplot(dist, aes(x=x, y=y)) +
		geom_area(data=filter(dist, x <= -tau/2), fill='#7FCDC4', alpha=0.5) +   # "different" region
		geom_area(data=filter(dist, x >= tau/2), fill='#7FCDC4', alpha=0.5) +    # "different" region
		geom_line(color='black', linewidth=1) +
		geom_vline(xintercept=c(-tau/2, tau/2), color='black', linewidth=1) +
		scale_y_continuous(expand=expansion(mult=c(0, 0.4))) +
		theme_void() +
		theme(plot.margin=margin(5, 10, 5, 10))
}

# Plot a small schematic of hypothesized performance (% correct) like how we plot it in the paper
plot_performance_schematic = function(slope_lowhigh=0.3, slope_highlow=-0.3,
									  intercept_lowhigh=0.5, intercept_highlow=0.5) {
    # Create a dataframe with the three positions relative to the perceptual mean
	pos = data.frame(
		x=c(-1, 0, 1),
		position=factor(c('below', 'near', 'above'), levels=c('below', 'near', 'above'))
	)

    # Compute hypothesized performance for each tone order at each position
	perf = rbind(
		pos %>% mutate(order='Low-High', y=intercept_lowhigh + slope_lowhigh*x),
		pos %>% mutate(order='High-Low', y=intercept_highlow + slope_highlow*x)
	) %>% mutate(order=factor(order, levels=c('Low-High', 'High-Low')))

    # Create the plot
	ggplot(perf, aes(x=x, y=y)) +
        # Draw the lines (dashed for Low-High, solid for High-Low, as in Figure 3)
		geom_line(aes(group=order, linetype=order), color='black', linewidth=0.8) +

        # Draw the dots, colored by position relative to the perceptual mean
		geom_point(aes(fill=position), shape=21, color='black', size=3.5, stroke=0.6) +

        # Manually set the colors and linetypes
		scale_fill_manual(values=c('below'='#CBB4DE', 'near'='#8E63B5', 'above'='#4B2A7B')) +
		scale_linetype_manual(values=c('Low-High'='dashed', 'High-Low'='solid')) +

        # Scale and set themeing
		scale_x_continuous(limits=c(-1.4, 1.4)) +
		scale_y_continuous(limits=c(0, 1)) +
		theme_void() +

        # Tweak axis (axis lines and titles only, no ticks or tick labels)
		theme(legend.position='none',
			  axis.line=element_line(color='black'),
			  axis.title.x=element_text(size=0.8*font_scale, margin=margin(t=4)),
			  axis.title.y=element_text(size=0.8*font_scale, angle=90, margin=margin(r=4)),
			  plot.margin=margin(5, 10, 5, 10)) +
		xlab('CF') +
		ylab('% correct')
}

# Plot the perceived frequency distributions for one example tone pair in the sensory bias model;
# show a ghost fill of the unbiased distribution estimate to make the shift clear
plot_sensory_bias_estimates = function(f=900, f_mean=1000, omega=0.33, sigma=0.015) {
    # Compute the biased f1-hat in log10 units (Eq. 5)
	log_f1_hat = log10(f) - omega*(log10(f) - log10(f_mean))

    # Create an x-axis to draw onto (in log10 units)
	x = seq(min(log10(f), log10(f_mean)) - 4*sigma, max(log10(f), log10(f_mean)) + 2*sigma, length.out=500)

    # Create a dataframe to store the distributions
	dists = rbind(
		data.frame(x=x, y=dnorm(x, log10(f), sigma), tone='f1', type='ghost'),
		data.frame(x=x, y=dnorm(x, log_f1_hat, sigma), tone='f1', type='estimate'),
		data.frame(x=x, y=dnorm(x, log10(f), sigma), tone='f2', type='estimate')
	) %>% mutate(tone=factor(tone, levels=c('f1', 'f2')), x_hz=10^x)

    # Create the plot
	ggplot(dists, aes(x=x_hz, y=y)) +
        # Draw the ghost
		geom_area(data=filter(dists, type == 'ghost'), fill='gray90') +

        # Draw the Gaussians
		geom_line(data=filter(dists, type == 'estimate'), color='#1F3A6E', linewidth=1) +

        # Draw the vertical line at the perceived mean
		geom_vline(xintercept=f_mean, color='black', linetype='dotted', linewidth=0.8) +

        # Split into one row per tone
		facet_grid(tone ~ .) +

        # Scale and set themeing
		scale_x_log10(breaks=c(f, f_mean)) +
		scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
		theme_void() +

        # Tweak axis
		theme(legend.position='none',
			  strip.text=element_blank(),
			  panel.spacing.y=unit(8, 'pt'),
			  axis.line.x=element_line(color='black'),
			  axis.ticks.x=element_line(color='black'),
			  axis.ticks.length.x=unit(3, 'pt'),
			  axis.text.x=element_text(size=0.8*font_scale, margin=margin(t=2)),
			  axis.title.x=element_text(size=0.8*font_scale, margin=margin(t=4)),
			  plot.margin=margin(5, 10, 5, 10)) +
		xlab('Perceived frequency (Hz, log scale)')
}

# Plot the difference distribution that results from the estimates above, same as above
# includ ea ghost
plot_sensory_bias_difference = function(f=900, f_mean=1000, omega=0.33, sigma=0.015) {
    # Compute the mean and SD of the difference distribution
	mu_diff = -omega*(log10(f_mean) - log10(f))
	sigma_diff = sqrt(2)*sigma

    # Create an x-axis to draw onto (centered on zero)
	x_max = abs(mu_diff) + 4*sigma_diff
	x = seq(-x_max, x_max, length.out=500)
	dist = data.frame(x=x, y=dnorm(x, mu_diff, sigma_diff))
	ghost = data.frame(x=x, y=dnorm(x, 0, sigma_diff))

    # Create the plot
	ggplot(dist, aes(x=x, y=y)) +
        # Draw the ghost
		geom_area(data=ghost, fill='gray90') +

        # Draw filled area (opaque version of the teal so it doesn't mix with the ghost)
		geom_area(data=filter(dist, x >= 0), fill='#BFE6E2') +

        # Draw the Gaussian
		geom_line(color='#1F3A6E', linewidth=1) +

        # Draw the vertical line
		geom_vline(xintercept=0, color='gray40', linetype='dashed', linewidth=1) +

        # Scale and set themeing
		scale_x_continuous(breaks=0) +
		scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
		theme_void() +

        # Tweak axis
		theme(legend.position='none',
			  axis.line.x=element_line(color='black'),
			  axis.ticks.x=element_line(color='black'),
			  axis.ticks.length.x=unit(3, 'pt'),
			  axis.text.x=element_text(size=0.8*font_scale, margin=margin(t=2)),
			  axis.title.x=element_text(size=0.8*font_scale, margin=margin(t=4)),
			  plot.margin=margin(5, 10, 5, 10)) +
		xlab(expression(log[10](hat(f)[2]/hat(f)[1])))
}

# Plot the perceived frequency distributions for one example tone pair in the criterion shift model
plot_criterion_shift_estimates = function(f=900, f_mean=1000, sigma=0.015) {
    # Create an x-axis to draw onto (in log10 units)
	x = seq(min(log10(f), log10(f_mean)) - 4*sigma, max(log10(f), log10(f_mean)) + 2*sigma, length.out=500)

    # Create a dataframe to store the distributions
	dists = rbind(
		data.frame(x=x, y=dnorm(x, log10(f), sigma), tone='f1'),
		data.frame(x=x, y=dnorm(x, log10(f), sigma), tone='f2')
	) %>% mutate(tone=factor(tone, levels=c('f1', 'f2')), x_hz=10^x)

    # Create the plot
	ggplot(dists, aes(x=x_hz, y=y)) +
        # Draw the Gaussians
		geom_line(color='#1F3A6E', linewidth=1) +

        # Draw the vertical line at the perceived mean
		geom_vline(xintercept=f_mean, color='black', linetype='dotted', linewidth=0.8) +

        # Split into one row per tone
		facet_grid(tone ~ .) +

        # Scale and set themeing
		scale_x_log10(breaks=c(f, f_mean)) +
		scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
		theme_void() +

        # Tweak axis
		theme(legend.position='none',
			  strip.text=element_blank(),
			  panel.spacing.y=unit(8, 'pt'),
			  axis.line.x=element_line(color='black'),
			  axis.ticks.x=element_line(color='black'),
			  axis.ticks.length.x=unit(3, 'pt'),
			  axis.text.x=element_text(size=0.8*font_scale, margin=margin(t=2)),
			  axis.title.x=element_text(size=0.8*font_scale, margin=margin(t=4)),
			  plot.margin=margin(5, 10, 5, 10)) +
		xlab('Perceived frequency (Hz, log scale)')
}

# Plot the difference distribution
plot_criterion_shift_difference = function(f=900, f_mean=1000, s=-0.33, sigma=0.015) {
    # Compute the biased criterion (Eq. 9) and the SD of the difference distribution
	biased_k = s*(log10(f) - log10(f_mean))
	sigma_diff = sqrt(2)*sigma

    # Create an x-axis to draw onto (centered on zero)
	x_max = abs(biased_k) + 4*sigma_diff
	x = seq(-x_max, x_max, length.out=500)
	dist = data.frame(x=x, y=dnorm(x, 0, sigma_diff))

    # Create a dataframe to store the criterion data (k)
	crit = data.frame(
		k=c(0, biased_k),
		type=factor(c('unbiased k', 'biased k'), levels=c('unbiased k', 'biased k'))
	)

    # Create the plot
	ggplot(dist, aes(x=x, y=y)) +
        # Draw filled area (same opaque teal as the sensory bias version)
		geom_area(data=filter(dist, x >= biased_k), fill='#BFE6E2') +

        # Draw the Gaussian
		geom_line(color='#1F3A6E', linewidth=1) +

        # Draw the vertical lines
		geom_vline(data=crit, aes(xintercept=k, color=type, linetype=type), linewidth=1) +

        # Manually tweak the colors and linetypes to match panel C
		scale_color_manual(values=c('unbiased k'='gray40', 'biased k'='red')) +
		scale_linetype_manual(values=c('unbiased k'='dashed', 'biased k'='dotdash')) +

        # Scale and set themeing
		scale_x_continuous(breaks=0) +
		scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
		theme_void() +

        # Tweak axis
		theme(legend.position='none',
			  axis.line.x=element_line(color='black'),
			  axis.ticks.x=element_line(color='black'),
			  axis.ticks.length.x=unit(3, 'pt'),
			  axis.text.x=element_text(size=0.8*font_scale, margin=margin(t=2)),
			  axis.title.x=element_text(size=0.8*font_scale, margin=margin(t=4)),
			  plot.margin=margin(5, 10, 5, 10)) +
		xlab(expression(log[10](hat(f)[2]/hat(f)[1])))
}

# Construct the three panels for the criterion shift model)
p1 = plot_difference_distribution(unbiased_k=0, biased_k=1)
p2 = plot_difference_distribution(unbiased_k=0, biased_k=0)
p3 = plot_difference_distribution(unbiased_k=0, biased_k=-1)
panel_width = 3.6  # in inches but we scale down a lot so the font sizes are kinda small
panel_height = 2
ggsave(file.path('figs', 'fig0a.png'), p1, width=panel_width, height=panel_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0b.png'), p2, width=panel_width, height=panel_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0c.png'), p3, width=panel_width, height=panel_height, dpi=300, bg='white')

# Construct the three panels for the sensory bias model (don't show a biased k)
p4 = plot_difference_distribution(unbiased_k=0, biased_k=NA, mu=-1)
p5 = plot_difference_distribution(unbiased_k=0, biased_k=NA, mu=0)
p6 = plot_difference_distribution(unbiased_k=0, biased_k=NA, mu=1)
ggsave(file.path('figs', 'fig0d.png'), p4, width=panel_width, height=panel_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0e.png'), p5, width=panel_width, height=panel_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0f.png'), p6, width=panel_width, height=panel_height, dpi=300, bg='white')

# Construct the three same-different panels for the criterion shift model (criteria and
# distribution both stay put, so no context effect is predicted)
p7 = plot_samediff_distribution(tau=2, mu=0)
p8 = plot_samediff_distribution(tau=2, mu=0)
p9 = plot_samediff_distribution(tau=2, mu=0)
ggsave(file.path('figs', 'fig0g.png'), p7, width=panel_width, height=panel_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0h.png'), p8, width=panel_width, height=panel_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0i.png'), p9, width=panel_width, height=panel_height, dpi=300, bg='white')

# Construct the three same-different panels for the sensory bias model (criteria stay put,
# but the distribution shifts just as in the up-down panels)
p10 = plot_samediff_distribution(tau=2, mu=-1)
p11 = plot_samediff_distribution(tau=2, mu=0)
p12 = plot_samediff_distribution(tau=2, mu=1)
ggsave(file.path('figs', 'fig0j.png'), p10, width=panel_width, height=panel_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0k.png'), p11, width=panel_width, height=panel_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0l.png'), p12, width=panel_width, height=panel_height, dpi=300, bg='white')

# Construct the four performance schematics (one per model and task)
perf_width = 1.8
perf_height = 1.6
p_perf_ud_criterion = plot_performance_schematic(slope_lowhigh=0.3, slope_highlow=-0.3)
p_perf_ud_sensory = plot_performance_schematic(slope_lowhigh=0.3, slope_highlow=-0.3)
p_perf_sd_criterion = plot_performance_schematic(slope_lowhigh=0, slope_highlow=0,
												 intercept_lowhigh=0.55, intercept_highlow=0.45)
p_perf_sd_sensory = plot_performance_schematic(slope_lowhigh=0.3, slope_highlow=-0.3)
ggsave(file.path('figs', 'fig0_perf_updown_criterion.png'), p_perf_ud_criterion, width=perf_width, height=perf_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0_perf_updown_sensory.png'), p_perf_ud_sensory, width=perf_width, height=perf_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0_perf_samediff_criterion.png'), p_perf_sd_criterion, width=perf_width, height=perf_height, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0_perf_samediff_sensory.png'), p_perf_sd_sensory, width=perf_width, height=perf_height, dpi=300, bg='white')

# Construct the two panels illustrating the sensory bias model (Eq. 5)
p_sensory_estimates = plot_sensory_bias_estimates(f=900, f_mean=1000, omega=0.33, sigma=0.015)
p_sensory_difference = plot_sensory_bias_difference(f=900, f_mean=1000, omega=0.33, sigma=0.015)
ggsave(file.path('figs', 'fig0_sensory_estimates.png'), p_sensory_estimates, width=panel_width, height=panel_height*1.5, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0_sensory_difference.png'), p_sensory_difference, width=panel_width, height=panel_height, dpi=300, bg='white')

# Construct the two panels illustrating the criterion shift model (Eq. 9)
p_criterion_estimates = plot_criterion_shift_estimates(f=900, f_mean=1000, sigma=0.015)
p_criterion_difference = plot_criterion_shift_difference(f=900, f_mean=1000, s=-0.33, sigma=0.015)
ggsave(file.path('figs', 'fig0_criterion_estimates.png'), p_criterion_estimates, width=panel_width, height=panel_height*1.5, dpi=300, bg='white')
ggsave(file.path('figs', 'fig0_criterion_difference.png'), p_criterion_difference, width=panel_width, height=panel_height, dpi=300, bg='white')

# Save the inset
p_inset = plot_inset_distribution()
ggsave(file.path('figs', 'fig0_inset.png'), p_inset, width=2, height=1.25, dpi=300, bg='white')

# Save the same-different inset
p_samediff_inset = plot_samediff_inset_distribution()
ggsave(file.path('figs', 'fig0_samediff_inset.png'), p_samediff_inset, width=2, height=1.25, dpi=300, bg='white')
