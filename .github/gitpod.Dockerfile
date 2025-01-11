# Pull fixed Docker image from rocker/geospatial:4.3.0 SHA digest
FROM ghcr.io/rocker-org/geospatial:latest

ENV QUARTO_VERSION="1.6.40"
ENV RENV_VERSION="v1.0.11"
ENV RENV_CONFIG_PAK_ENABLED true

USER root

RUN apt-get update --quiet && \
    apt-get install --quiet --yes \
        gdebi-core \
        pandoc \
        && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/*

# Download stable release of quarto markdown for rendering project book & sites
RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb && \
    gdebi --non-interactive quarto-linux-amd64.deb

# R commands to install specific version of `renv` for R environment management
RUN R -e "install.packages(c('remotes', 'languageserver'), repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /workspace

COPY renv.lock renv.lock

RUN R -e "renv::restore(library = Sys.getenv('R_LIBS_SITE'))"

# Create the gitpod user. UID must be 33333.
RUN useradd -l -u 33333 -G sudo -md /home/gitpod -s /bin/bash -p gitpod gitpod

USER gitpod

CMD ["bash"]
