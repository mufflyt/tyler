## code to prepare `acog_presidents` dataset goes here
library(dplyr)
library(exploratory)
library(tidyr)
library(stringr)
library(readr)
library(humaniformat)

acog_presidents <- exploratory::scrape_html_table("https://www.acog.org/about/leadership-and-governance/board-of-directors/past-presidents", 1, "TRUE" ,encoding="UTF-8") %>%
  exploratory::clean_data_frame() %>%
  dplyr::distinct(President, .keep_all = TRUE) %>%
  tidyr::separate(President, into = c("President", "honorrific"), sep = "\\s*\\,\\s*", convert = TRUE) %>%
  dplyr::mutate(first = humaniformat::first_name(President)) %>%
  dplyr::mutate(last = humaniformat::last_name(President)) %>%
  dplyr::mutate(middle = humaniformat::middle_name(President)) %>%
  dplyr::mutate(middle = stringr::str_remove_all(middle, "[[\\p{P}][\\p{S}]]")) %>%
  exploratory::reorder_cols(first, last, middle, President, honorrific, Presidency) %>%
  dplyr::mutate(Presidency = stringr::word(Presidency, 1, sep = "\\s*\\-\\s*")) %>%
  dplyr::mutate(Presidency = readr::parse_number(Presidency)) %>%
  dplyr::arrange(desc(Presidency)) %>%
  dplyr::mutate(first = str_remove_all(first, "[[\\p{P}][\\p{S}]]"))

#readr::write_csv(acog_presidents, "acog_presidents.csv")

usethis::use_data(acog_presidents, overwrite = TRUE)
