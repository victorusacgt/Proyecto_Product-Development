FROM rocker/shiny:latest

RUN apt-get update \
    && apt-get install -y --no-install-recommends libmysqlclient-dev

RUN R -e "install.packages(c('RMySQL', 'dplyr', 'leaflet', 'RColorBrewer', 'lubridate'))"

EXPOSE 3838 3306 33061
