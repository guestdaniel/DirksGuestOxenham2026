# DirksGuestOxenham2021

This repository contains all code necessary for completely reproducing the analyses and figures in Dirks, Guest, and Oxenham (2021), in preparation. The entire codebase is in R. With a properly configured R interpreter, the entire manuscript can be reproduced with a single `run.sh` shell script. We also provide a Docker image with a properly configured environment to enable users and readers to reproduce the manuscript with minimal impact on their own systems. Instructions for both a manual installation and using the Docker image are available below.

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
├── Dockerfile               # Dockerfile used to generate Docker image
├── LICENSE                  # License file for the code contained in this repository
├── README.md                # This README file
└── run.sh                   # Shell script to reproduce figures and analyses
```

## Behavioral data

Preprocessed behavioral data are available separately at [Zenodo](link). The `.R` scripts used to generate these data from the raw behavioral data are available in `nofig`. We recommend downloading the behavioral data from Zenodo and unzipping the files into the empty `data` folder in this repository to run the code. 

The MATLAB code used to collect behavioral responses is available compressed into a single `.zip` file. This code depends on MATLAB and on the [`afc`](http://medi.uni-oldenburg.de/afc/) package and is not thoroughly documented. 

## Instructions (Docker)

Docker is our recommended solution for reproducing the results from Dirks, Guest, and Oxenham (2021). If you are unfamiliar with Docker, you may want to [orient yourself](https://docs.docker.com/get-started/). The Docker image associated with this repository will allow you to start up a Linux container with R, all required packages/programs, and a copy of this repository. Inside this environment, which is sandboxed from the rest of your system, you can replicate the results of our paper with a single command. When you are done with this process and leave the environment, the environment will clean itself up and stop consuming system resources. The major advantage of using Docker in this way is that you do not have to install Python, R, or any other programs yourself.

To get started, make sure you have [Docker installed](https://docs.docker.com/get-docker/). Then, follow the instructions below. The instructions below are written for command line interface (such as PowerShell and Terminal) but equivalent commands likely exist in graphical user interface versions of the Docker software.

First, pull the image from our GitHub repository.

```
docker pull docker.pkg.github.com/guestdaniel/dirksguestoxenham2021/dirksguestoxenham2021:0.1.0
```

Next, use the image to create an interactive container.

```
docker run --rm -it dirksguestoxenham2021
```

- `--rm` flag tells Docker to "clean up" after itself and to remove any files generated while running the image after the container is closed
- `-it` tells Docker this is an interactive session 

This container starts with an interactive bash shell located in a copy of the present repository. From there, you can either manually run individual scripts using the `Rscript` command (e.g., `Rscript fig1/fig1.R` to generate Figure 1), or you can generate the all the figures in the paper via:

```
bash run.sh
```

Note that this may take quite some time, as this shell script runs all of the analyses in the manuscript, including some rather computationally expensive mixed models.

However, the figures will be saved out to the container's non-persistent storage and will be destroyed when you exit or end the container. To have permanent copies of the outputs figures saved to your disk, you can link the output `figs` directory inside the container to a preferred output location somewhere on your disk. First, exit the container with the `exit` command, then run the following:

```
docker run --rm -v /home/daniel/DirksGuestOxenham2021/figs:/DirksGuestOxenham2021/plots -it dirksguestoxenham2021
```

- `-v` flag tells Docker to link the `figs` folder on your disk (path to the left of `:`) with the `figs` folder in the container (path to the right of `:`). Obviously, you will need to adjust the path on the left to point to wherever you have stored your local copy of the repository.

Now, if you call `run.sh` or any of the individual plotting files (e.g., `fig1.R`), whatever is saved in the `figs` folder of the repository will be accessible on your hard drive (outside of the container) in the `figs` folder of this repository. This process can be repeated, if desired, for the `outputs` folder to capture the saved model fit objects and statistical reports. 

## Instructions (manual)

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

## Reproducing the paper

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