# Pull fixed Docker image from rocker/geospatial:4.3.0 SHA digest
FROM rocker/geospatial@sha256:1e9b96124f66f98c7dd0d2c41a990fb128fa335a658d869d799ad5779391a163

ENV QUARTO_VERSION="1.3.340"
ENV RENV_VERSION="v1.0.3"

USER root

RUN apt-get update --quiet && \
    apt-get install --quiet --yes \
        gdebi-core \
        pandoc \
        pandoc-citeproc \
        && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/*

RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb && \
    gdebi --non-interactive quarto-linux-amd64.deb

# R commands to install specific version of `renv` for R environment management
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /workspace
COPY renv.lock renv.lock
ENV RENV_PATHS_LIBRARY renv/library
ENV RENV_CONFIG_PAK_ENABLED true

RUN R -e "renv::restore(library = Sys.getenv('R_LIBS_SITE'), repos = c(CRAN = 'https://cloud.r-project.org'))"

# Create the gitpod user. UID must be 33333.
RUN useradd -l -u 33333 -G sudo -md /home/gitpod -s /bin/bash -p gitpod gitpod

USER gitpod

CMD ["bash"]
