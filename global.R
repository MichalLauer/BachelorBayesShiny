library(shiny)
library(bslib)
library(plotly)
library(distr6)
library(purrr)
library(dplyr)
library(tidyr)

invisible(lapply(
  X = list.files(path = "R", full.names = TRUE), FUN = source
))
