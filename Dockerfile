FROM jupyter/base-notebook
RUN R -e 'install.packages("leaflet")'
RUN R -e 'install.packages("ggplot2")'


