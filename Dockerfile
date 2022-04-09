FROM r-base:4.1.3

WORKDIR /usr/scr/wordel

COPY . .

RUN install2.r readr stringr dplyr magrittr here
