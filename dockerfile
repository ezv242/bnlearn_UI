FROM rocker/r-ver:4.5.1

# Dependencias del sistema (Ampliadas con soporte crítico para V8/shiny.semantic)
RUN apt-get update && apt-get install -y \
    graphviz \
    graphviz-dev \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    make \
    gcc \
    g++ \
    libnode-dev \
    libv8-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar BiocManager primero
RUN R -e "install.packages('BiocManager', repos='https://cloud.r-project.org')"

# Instalar paquetes R obligando a fallar si hay errores
RUN R -e "pkgs <- c('shiny', 'shiny.semantic', 'bnlearn', 'visNetwork', 'readxl'); \
          install.packages(pkgs, repos='https://cloud.r-project.org'); \
          if (!all(pkgs %in% installed.packages()[, 'Package'])) stop('¡Fallo crítico instalando paquetes de R!')"

# Rgraphviz desde Bioconductor
RUN R -e "BiocManager::install('Rgraphviz', ask = FALSE, update = FALSE); \
          if (!'Rgraphviz' %in% installed.packages()[, 'Package']) stop('¡Fallo crítico en Rgraphviz!')"

# Carpeta de trabajo
WORKDIR /app

# Copiar app
COPY . /app

# Puerto Shiny
EXPOSE 3838

# Ejecutar app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]