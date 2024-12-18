# Shiny
library(shiny)
library(shinyjs)
library(bslib)
# Data wrangling
library(plotly)
library(distr6)
library(purrr)
library(dplyr)
library(tidyr)
library(glue)

invisible(lapply(
  X = list.files(path = "R", full.names = TRUE, recursive = TRUE),
  FUN = source
))
