# DirksGuestOxenham2021

This repository contains all code necessary for completely reproducing the analyses and figures in Dirks, Guest, and Oxenham (2021), in preparation. The entire codebase is in R. With a properly configured R interpreter, the entire manuscript can be reproduced with a single `run.sh` shell script.

The code in this repository is licensed under GNU GPLv3 (see `LICENSE.txt`).

## File structure

The file structure of this repository is shown in the file hierarchy below. Code required to generate each figure is stored in its own folder. These scripts may depend on the outputs of other scripts, usually run from the `nofig` folder with outputs stored in the `outputs` folder. Plots are always saved in the `figs` folder.

```
.  
├── data                     # Behavioral data goes here!
├── equations                # LaTeX and PDF output of brief explanation of model equations
├── fig1                     # Code for Figure 1
├── ...  
├── fig6                     # Code for Figure 6
├── figs                     # .png files of manuscript figures
├── nofig                    # Code that is needed but is not directly featured in a single manuscript figure
│   ├── comp_models          # Fit computational models to the behavioral data
│   ├── data                 # Preprocess the behavioral data
│   └── stats                # Analyze the behavioral data
├── ouiputs                  # Saved copies of fitted model objects, statistical tests, etc.
├── scripts                  # Scripts or function definitions called by other files
├── Dockerfile               # Dockerfile used to generate Docker image
├── LICENSE                  # License file for the code contained in this repository
└── README.md                # This README file
```

## Behavioral data

Preprocessed behavioral data are available separately at [Zenodo](link). The `.R` scripts used to generate these data from the raw behavioral data are available in `nofig`. We recommend downloading the behavioral data from Zenodo and unzipping the files into the empty `data` folder in this repository to run the code. 

The MATLAB code used to collect behavioral responses is available compressed into a single `.zip` file. This code depends on MATLAB and on the [`afc`](http://medi.uni-oldenburg.de/afc/) package and is not thoroughly documented. 

## Instructions

### Installation

You will need to install R to run the code in this repository. We tested on R 3.6.0 and R 4.0.3, but in theory any sufficiently recent version of R should suffice. The following packages are required (with recommended versions based on our install of R 4.0.3 in parentheses):

- `dplyr`
- `tidyr`
- `pracma`
- `ggplot2`
- `lme4`
- `car`
- `nlme`
- `merTools`
- `optimx`
- `phia`
- `effects`

### Behavioral data analyses

Behavioral data analyses can be reproduced by running the `.R` scripts in `nofig/stats`. Results are saved out as `.txt` files in the same folder.

### Figures

- Figure 1: Run `fig1/fig1.R`, output plot is saved as `figs/fig1.png`
- Figure 2: Run `fig2/fig2.R`, plot is saved as `figs/fig2.png`
- Figure 3: Run `fig3/fig3.R`, plot is saved as `figs/fig3.png`
- Figure 4: Run `nofig/comp_models/fit_comp_models.R`. This will fit the criterion shift and sensory models to the behavioral data and then save the fitted models in `outputs`. Then run `fig4/fig4.R`, the plot is saved in `figs/fig4.png`. 
- Figure 4: Run `nofig/comp_models/fit_comp_models.R`. This will fit the criterion shift and sensory models to the behavioral data and then save the fitted models in `outputs`. Then run `fig5/fig5.R`, the plot is saved in `figs/fig5.png`. 