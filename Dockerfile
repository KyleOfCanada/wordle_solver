FROM r-base

WORKDIR /usr/scr/wordel

COPY . .

RUN Rscript -e "install.packages('readr')"
RUN Rscript -e "install.packages('stringr')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('magrittr')"
RUN Rscript -e "install.packages('here')"