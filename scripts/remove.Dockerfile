FROM rocker/r-ver:4

COPY bin /usr/local/bin/

RUN apt-get update && apt-get -y install --no-install-recommends \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

RUN install_packages --repo "https://mrc-ide.r-universe.dev" \
        ckanr \
        docopt

RUN install_packages httr

COPY remove_estimates_datasets.R /usr/local/bin/

ENTRYPOINT ["remove_estimates_datasets.R"]
