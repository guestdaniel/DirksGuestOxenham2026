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

# Plot the inset schematic (simplified curve with only unbiased)
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

# Construct the three panels for the criterion shift model)
p1 = plot_difference_distribution(unbiased_k=0, biased_k=1)
p2 = plot_difference_distribution(unbiased_k=0, biased_k=0)
p3 = plot_difference_distribution(unbiased_k=0, biased_k=-1)
panel_width = 3.6
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

# Save the inset
p_inset = plot_inset_distribution()
ggsave(file.path('figs', 'fig0_inset.png'), p_inset, width=2, height=1.25, dpi=300, bg='white')
