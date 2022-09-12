FROM rocker/r-ver:4.2.1

ARG WHEN

RUN mkdir /home/analysis

RUN R -e "options(repos = \
  list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/${WHEN}')); \
  install.packages('tidystringdist')"