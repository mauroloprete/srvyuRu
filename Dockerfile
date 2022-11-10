FROM rocker/tidyverse

RUN Rscript -e 'remotes::install_gitlab("cognus/proyectos/anii/tidyeaii", auth_token = "glpat-vYUCPRF-p1zXesW871g_")'
