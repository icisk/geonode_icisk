FROM rocker/shiny

RUN apt-get update \
 && apt-get install -y \
      gdal-bin \
      libcurl4-openssl-dev \
      libgdal-dev \
      libgeos-dev \
      libicu-dev \
      libpng-dev \
      libproj-dev \
      libsqlite3-dev \
      libssl-dev \
      libudunits2-dev \
      libxml2-dev \
      make \
      pandoc \
      zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

#RUN apt-get install -y git
#RUN ls -la /run/
# RUN --mount=type=secret,id=token token="$(cat /run/secrets/token)" \
# && git clone "https://${token}@github.com/icisk/csmockups.git" /mockup

RUN R -q -e "install.packages(c(\
  'curl',\
  'dplyr',\
  'ggplot2',\
  'ggrepel',\
  'jsonlite',\
  'leaflet',\
  'lubridate',\
  'meteospain',\
  'rnaturalearth',\
  'rnaturalearthdata',\
  'sf',\
  'sfheaders',\
  'shiny',\
  'shinythemes'\
  ))"

WORKDIR /mockup/LL1_Spain/RShinyApp
#RUN git pull
#ADD ./data ./data
#ADD ./app.R ./

CMD "Rscript" "./app.R"
#CMD "sleep" "1d"
