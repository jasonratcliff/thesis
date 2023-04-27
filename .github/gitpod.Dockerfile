# Pull fixed Docker image from rocker/geospatial:4.3.0 SHA digest
FROM rocker/geospatial@sha256:1e9b96124f66f98c7dd0d2c41a990fb128fa335a658d869d799ad5779391a163

ENV QUARTO_VERSION="1.3.340"

USER root

RUN apt-get update --quiet && \
    apt-get install --quiet --yes \
        gdebi-core \
        pandoc \
        pandoc-citeproc \
        && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/*

RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb && \
    gdebi --non-interactive quarto-linux-amd64.deb

# Create the gitpod user. UID must be 33333.
RUN useradd -l -u 33333 -G sudo -md /home/gitpod -s /bin/bash -p gitpod gitpod

USER gitpod

CMD ["bash"]
