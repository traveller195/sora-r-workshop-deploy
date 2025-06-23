FROM jupyter/r-notebook:latest

# Step 1: Switch to root
USER root

# Install Quarto CLI
RUN apt-get update && \
    apt-get install -y wget gdebi-core && \
    wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.5.50/quarto-1.5.50-linux-amd64.deb && \
    gdebi -n quarto-1.5.50-linux-amd64.deb && \
    rm quarto-1.5.50-linux-amd64.deb

# Install R packages
RUN Rscript -e "install.packages(c('ggplot2', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('leaflet', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('httr2', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('utils', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('knitr', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('rmarkdown', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('testthat', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('cli', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('askpass', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('tibble', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('sf', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('jsonlite', 'IRkernel'), repos='https://cloud.r-project.org')" && \
    Rscript -e "install.packages(c('pak', 'IRkernel'), repos='https://cloud.r-project.org')"

# Register IRkernel for JupyterLab
RUN Rscript -e "IRkernel::installspec(user = FALSE)"

# Ensure Quarto works with R
ENV QUARTO_DENO_DIR="/opt/deno"

# Step 3: Switch back to unprivileged user
USER ${NB_USER}