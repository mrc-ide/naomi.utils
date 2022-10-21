FROM rocker/r-ver:4

COPY bin /usr/local/bin/

RUN apt-get update && apt-get -y install --no-install-recommends \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

RUN install_packages --repo "https://mrc-ide.r-universe.dev" \
        ckanr \
        docopt

COPY remove_2023_datasets.R /usr/local/bin/

ENTRYPOINT ["remove_2023_datasets.R"]
