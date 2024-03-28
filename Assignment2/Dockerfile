# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# System libraries of general use getting from:
# https://www.statworx.com/en/content-hub/blog/how-to-dockerize-shinyapps/
## Install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev
## Update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Copy necessary files
## renv.lock file
COPY /app-GLS/renv.lock ./renv.lock
## app folder
COPY /app-GLS ./app

# Install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# Expose port
EXPOSE 3838

# Run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
