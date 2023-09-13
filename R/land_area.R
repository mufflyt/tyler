#' Square miles and kilometers by state.
#'
#' @name land_area_data
#' @export

library(tidyverse)
library(janitor)
library(dplyr)
library(magrittr)
land_area <- source("data-raw/land_area.R")
