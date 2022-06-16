

# 
#
# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

# --- Elegant way to check for missing packages and install them

list_of_packages <- 
  c("DBI",
    "RSQLite",
    "dbplyr",
    "readr",
    "devtools",
    "here",
    "tidyverse",
    "corrplot",
    "plotly",
    "rgdal",
    "knitr",
    "tmap",
    "cluster",
    "dendextend",
    "factoextra",
    "fpc",
    "gridExtra",
    "readxl",
    "janitor",
    "skimr",
    "viridis"
  )


new_packages <- 
  list_of_packages[!(list_of_packages %in% 
                       installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)


# --- load packages
suppressMessages(library("DBI"))
suppressMessages(library("RSQLite"))
suppressMessages(library("dbplyr"))
suppressMessages(library("readr"))
suppressMessages(library("here"))
suppressMessages(library("corrplot"))
suppressMessages(library("plotly"))
suppressMessages(library("rgdal"))
suppressMessages(library("tmap"))
suppressMessages(library("cluster"))
suppressMessages(library("dendextend"))
suppressMessages(library("factoextra"))
suppressMessages(library("fpc"))
suppressMessages(library("gridExtra"))
suppressMessages(library("readxl"))
suppressMessages(library("janitor"))
suppressMessages(library("knitr"))
suppressMessages(library("skimr"))
suppressMessages(library("viridis"))
suppressMessages(library("tidyverse"))

# clear global enviroment
rm(list=ls())

