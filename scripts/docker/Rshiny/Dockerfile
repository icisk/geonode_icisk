FROM rocker/shiny

RUN rm -vf /var/lib/apt/lists/*

RUN apt-get update

RUN apt-get install -y  make zlib1g-dev libpng-dev libicu-dev pandoc libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev libssl-dev libcurl4-openssl-dev libxml2-dev

#RUN apt-get install -y git

#RUN ls -la /run/


# RUN --mount=type=secret,id=token token="$(cat /run/secrets/token)" \
# && git clone "https://${token}@github.com/icisk/csmockups.git" /mockup

RUN R -q -e "install.packages(c('shiny', 'shinythemes', 'leaflet', 'ggplot2', 'dplyr', 'lubridate', 'rnaturalearth', 'rnaturalearthdata', 'ggrepel', 'meteospain', 'sf', 'sfheaders', 'curl', 'jsonlite'))" 

WORKDIR /mockup/LL1_Spain/RShinyApp
#RUN git pull
#ADD ./data ./data
#ADD ./app.R ./

CMD "Rscript" "./app.R"
#CMD "sleep" "1d"