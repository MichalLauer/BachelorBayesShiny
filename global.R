# Shiny
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(bslib)
# Data wrangling
library(plotly)
library(distr6)
library(purrr)
library(dplyr)
library(tidyr)
library(glue)
# Paralel
library(waiter)
library(promises)
library(future)
plan(multisession, workers = availableCores() - 1,
     .skip = TRUE)

invisible(lapply(
  X = list.files(path = "R", full.names = TRUE, recursive = TRUE),
  FUN = source
))
