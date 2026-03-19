# Shell script to reproduce figures and analyses in Dirks, Guest, and Oxenham (2026) 
# On Mac/Linux, you should be able to run this entire script in the shell. On Windows, you may have
# to run it line-by-line and adjust commands as needed.

# Run preprocessing commands to generate preprocessed behavioral data from raw behavioral data
# NOTE: Only the experimenters need to run this!
Rscript nofig/preprocess/preprocess_data_samediff_new.R
Rscript nofig/preprocess/preprocess_data_updown_new.R

# Get the preprocessed behavioral data from Zenodo 
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