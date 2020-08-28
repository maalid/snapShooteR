FROM rocker/r-ver:4.0.2
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libssh2-1-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.2")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.10")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("shinythemes",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "1.0")'
RUN Rscript -e 'remotes::install_version("base64enc",upgrade="never", version = "0.1-3")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("shinymanager",upgrade="never", version = "1.0.300")'
RUN Rscript -e 'remotes::install_version("shinyFiles",upgrade="never", version = "0.8.0")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.26")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.3")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');snapShooteR::run_app()"
