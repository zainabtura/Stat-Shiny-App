FROM rocker/shiny:4.4.0

RUN install2.r --error --skipinstalled \
    readr \
    readxl \
    DT \
    moments \
    corrplot \
    RColorBrewer \
    plotrix \
    shinycssloaders \
    shinyjs \
    car \
    lmtest \
    nortest \
    randtests

COPY app.R /srv/shiny-server/app/
COPY R/ /srv/shiny-server/app/R/
COPY datasets/ /srv/shiny-server/app/datasets/

RUN chown -R shiny:shiny /srv/shiny-server/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app', host='0.0.0.0', port=3838)"]
