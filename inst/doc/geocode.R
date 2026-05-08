eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  eval = eval_vignettes
)

if (eval_vignettes) {
  library(tyler)
}

library(tyler)
