# cityStateToLatLong.R

library(tidyverse)

cityStateToLatLong <- readr::read_csv("data-raw/cityStateToLatLong.csv")

# Add any tidying steps to this script if necessary
cityStateToLatLong <- readr::type_convert(cityStateToLatLong)

use_data(cityStateToLatLong, overwrite = TRUE)
