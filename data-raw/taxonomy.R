## code to prepare `taxonomy` dataset goes here
library(readr)

nucc_taxonomy_201 <- readr::read_csv("~/Dropbox (Personal)/workforce/Master_References/Taxonomy_codes/nucc_taxonomy_201.csv")
taxonomy <- nucc_taxonomy_201
taxonomy <- readr::type_convert(taxonomy)
usethis::use_data(taxonomy, overwrite = TRUE)
