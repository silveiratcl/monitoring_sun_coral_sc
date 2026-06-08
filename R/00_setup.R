################################################################################
# 00_setup.R
#
# Packages and helper functions used throughout the manuscript analyses
################################################################################

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sf)
library(ggplot2)
library(patchwork)

################################################################################
# Helper functions
################################################################################

clean_num <- function(x) {
  x |>
    as.character() |>
    str_trim() |>
    na_if("") |>
    na_if("Na") |>
    str_replace_all(",", ".") |>
    as.numeric()
}

max_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

################################################################################
# DAFOR manual weights
################################################################################

manual_weights <- c(
  `10` = 1.00,
  `8`  = 0.80,
  `6`  = 0.60,
  `4`  = 0.10,
  `2`  = 0.04,
  `0`  = 0.00
)

################################################################################
# Output folders
################################################################################

dir.create("outputs", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)