# DirksGuestOxenham2026
This repository contains all code necessary for completely reproducing the analyses and figures in Dirks, Guest, and Oxenham (2026), in preparation. The entire codebase is in R. With a properly configured R interpreter, the entire manuscript can be reproduced with a single `run.sh` shell script. 

The code in this repository is licensed under GNU GPLv3 (see `LICENSE.txt`).

## File structure
The file structure of this repository is shown in the file hierarchy below. Code required to generate each figure is stored in its own folder. These scripts may depend on the outputs of other scripts, usually run from the `nofig` folder with outputs stored in the `outputs` folder. Plots are always saved in the `figs` folder.

```
.  
├── data/                    # Behavioral data goes here!
├── equations/               # LaTeX and PDF output of brief explanation of model equations
├── fig1 /                   # Code for Figure 1
├── ...  
├── fig6/                    # Code for Figure 6
├── figs/                    # .png files of manuscript figures
├── nofig                    # Code that is needed but is not directly featured in a single manuscript figure
│   ├── comp_models          # Fit computational models to the behavioral data
│   ├── data                 # Preprocess the behavioral data
│   └── stats                # Analyze the behavioral data
├── outputs/                 # Saved copies of fitted model objects, statistical tests, etc.
├── scripts/                 # Scripts or function definitions called by other files
├── config.R                 # Sourced by some .R scripts to access repostiory-wide parameters (e.g., font size)
├── LICENSE                  # License file for the code contained in this repository
├── README.md                # This README file
└── run.sh                   # Shell script to reproduce figures and analyses
```

## Behavioral data
Preprocessed behavioral data are available separately at [Zenodo](link). The `.R` scripts used to generate these data from the raw behavioral data are available in `nofig`. We recommend downloading the behavioral data from Zenodo and unzipping the files into the empty `data` folder in this repository to run the code. 

The MATLAB code used to collect behavioral responses is available upon request. 

## Installation and running
You will need to install R to run the code in this repository. We tested on R 3.6.0 and R 4.0.3, but in theory any sufficiently recent version of R should suffice. The following packages are required (with recommended versions based on our install of R 4.0.3 in parentheses):

- `dplyr` (1.0.5)
- `tidyr` (1.1.2)
- `pracma` (2.2.9)
- `ggplot2` (3.3.3)
- `lme4` (1.1-25)
- `car` (3.0-10)
- `nlme` (3.1-149)
- `merTools` (0.5.2)
- `optimx` (2020-4.2)
- `phia` (0.2-1)
- `effects` (4.2-0)


### Behavioral data analyses
Behavioral data analyses can be reproduced by running the `.R` scripts in `nofig/stats`. Results are saved out as `.png` and `.txt` files in the `outputs` folder. These files have names starting with `statsmodel_abc...` where `abc` is either `jnd`, `updown`, or `samediff`. `jnd` refers to the analyses of estimated JNDs reported in the beginning of the manuscript near Figure 1. `updown` and `samediff` refer to analyses of those tasks near Figures 2 and 3. 

### Figures
- Figure 1: Run `fig1/fig1.R`, output plot is saved as `figs/fig1.png`
- Figure 2: Run `fig2/fig2.R`, plot is saved as `figs/fig2.png`
- Figure 3: Run `fig3/fig3.R`, plot is saved as `figs/fig3.png`
- Figure 4: Run `nofig/comp_models/fit_comp_models.R`. This will fit the criterion shift and sensory models to the behavioral data and then save the fitted models in `outputs`. Then run `fig4/fig4.R`, the plot is saved in `figs/fig4.png`. 
- Figure 5: Run `nofig/comp_models/fit_comp_models.R`. This will fit the criterion shift and sensory models to the behavioral data and then save the fitted models in `outputs`. Then run `fig5/fig5.R`, the plot is saved in `figs/fig5.png`. 
- Figure 6: Run `nofig/comp_models/bootstrap_comp_models.R`, then run `fig6/fig6.R`. The subplots are saved in `figs/fig6a.png`, `figs/fig6b.png`, and so on. These subplots are combined in Inkscape to yield `figs/fig6.png`. 

### Shell script
`run.sh` in the top-level directory of the repository provides a shell script that should (probably) run on most POSIX-compliant shells. When run from this directory, the script will download and unpack the behavioral data from Zenodo, run all necessary behavioral and computational analyses, and generate all figures in the manuscript. 