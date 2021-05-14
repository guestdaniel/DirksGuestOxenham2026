# DirksGuestOxenham2021

This repository contains all code necessary for completely reproducing the analyses and figures in Dirks, Guest, and Oxenham (2021), in preparation. The entire codebase is in R. With a properly configured R interpreter, the entire manuscript can be reproduced with a single `run.sh` shell script.

This repository is licensed under GNU GPL v3. A copy of the license is available in `LICENSE.txt`.

## Behavioral data

Preprocessed behavioral data are available separately at [Zenodo](link). The `.R` scripts used to generate these data from the raw behavioral data are available in `nofig`. We recommend placing downloading the behavioral data from Zenodo and unzipping the files into the empty `data` folder in this repository to run the code.

## Instructions

### Installation

You will need to install R to run the code in this repository. We tested on R 3.6.0, but in theory any sufficiently recent version of R should suffice. The following packages are required (with recommended versions in parentheses):

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
- Figure 4: Run `fig3/llalalalaal`