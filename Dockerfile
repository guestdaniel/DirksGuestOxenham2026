# Start with Ubuntu image
FROM ubuntu:18.04
ENV DEBIAN_FRONTEND=noninteractive

# Create a directory GuestOxenham2021 and start the container by default in that folder
WORKDIR /DirksGuestOxenham2021

# Install essential tools and repositories to get pre-compiled R packages
RUN apt-get update && apt-get install -y --no-install-recommends dirmngr gpg-agent software-properties-common gcc \
    build-essential git libcurl4-openssl-dev libv8-dev libpng-dev libjpeg-dev && \
    apt update -y -qq && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
    add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" && \
    add-apt-repository -y ppa:c2d4u.team/c2d4u4.0+

# Install R and R packages
RUN apt-get update && \
    apt-get install -y -f --no-install-recommends r-base r-cran-dplyr r-cran-tidyr r-cran-effects r-cran-lme4 r-cran-optimx \
    r-cran-car r-cran-ggplot2 r-cran-nlme
RUN R -e "install.packages(c('merTools', 'phia', 'lmerTest', 'pracma'), dependencies=TRUE)"

# Copy this folder from disk to image
COPY . /DirksGuestOxenham2021