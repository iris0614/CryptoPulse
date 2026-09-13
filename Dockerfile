FROM rocker/shiny:4.3.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('dplyr', 'plotly', 'tidyr', 'httr', 'jsonlite'), repos = 'https://cloud.r-project.org')"

WORKDIR /app
COPY data /app/data
COPY src /app/src

EXPOSE 3838

CMD ["Rscript", "/app/src/run.R"]
