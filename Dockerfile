# Base R Shiny image
FROM rocker/shiny:latest

# Set a specific working directory
WORKDIR /PROMISE

# Install R dependencies
RUN R -e "install.packages(c('dplyr', 'ggplot2', 'shiny', 'shinythemes', 'readr', 'shinyWidgets', 'htmltools', 'powerSurvEpi', 'plotly', 'tableHTML', 'rms'), repos='http://cran.rstudio.com/')"

# Copy the Shiny app code
COPY app.R /PROMISE/app.R
COPY ui.R /PROMISE/ui.R
COPY server.R /PROMISE/server.R
COPY index.html /PROMISE/index.html
COPY www/styles.css /PROMISE/www/styles.css

# Copy the data directory
COPY data /PROMISE/data

# Make the Shiny app directory readable and writable
RUN chmod -R 755 /PROMISE

# Expose port 3838 for Shiny
EXPOSE 3838

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/PROMISE/app.R', host='172.17.0.4', port=3838)"]
