# Start with R 4.4.0
FROM rocker/shiny:4.4.0

# Switch to root to install system dependencies
USER root

# Update system and install security updates
RUN apt-get update && apt-get upgrade -y && \
    apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Expose port 3838 (default Shiny port)
EXPOSE 3838

# Copy config file
COPY /deploy/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Switch back to shiny user for security
USER shiny

# Install necessary R packages
RUN cd /home/shiny
COPY /deploy/.Rprofile /home/shiny/
RUN mkdir /home/shiny/library
RUN mkdir /home/shiny/myapp
RUN mkdir /home/shiny/myapp/app_cache

# Install {renv}
RUN R -e "install.packages(c('renv'), repos='https://cloud.r-project.org/')"

# Copy the app files
COPY /R /home/shiny/myapp/R
COPY /src /home/shiny/myapp/src
COPY /server.R /home/shiny/myapp/
COPY /ui.R /home/shiny/myapp/
COPY /global.R /home/shiny/myapp/
COPY /renv.lock /home/shiny/myapp/

# Use renv to manage package dependencies if you have renv.lock
COPY renv.lock /srv/shiny-server/myapp/
RUN cd /home/shiny/myapp && \
    R -e "renv::restore()"

# Configure Shiny to listen on port 3838
CMD ["/usr/bin/shiny-server"]