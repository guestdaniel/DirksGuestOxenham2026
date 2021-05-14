# Reproduces every figure in Dirks, Guest, and Oxenham (2021), including reproducing underlying simulations

# Get the behavioral data before we start running scripts
#wget LINKHERE -O data/data_archive.zip
unzip data/data_archive.zip
cp data_archive/* data
rm -r data_archive
rm data/data_archive.zip

# Figure 1
Rscript fig1/fig1.R

# Figure 2
Rscript fig2/fig2.R

# Figure 3
Rscript fig3/fig3.R

# Figure 4
Rscript nofig/comp_models/fit_comp_models.R
Rscript fig4/fig4.R

# Figure 5
# Rscript nofig/comp_models/fit_comp_models.R  # We skip this step as we assume this was already done above in Fig 4
Rscript fig5/fig5.R

# Figure 6
Rscript nofig/comp_models/bootstrap_comp_models.R
Rscript fig6/fig6.R

# Behavioral data analysis
Rscript nofig/stats/model_jnd.R
Rscript nofig/stats/model_samediff.R
Rscript nofig/stats/model_updown.R