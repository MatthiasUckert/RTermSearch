## code to prepare `test_document` dataset goes here
library(tidyverse)
table_document <- readtext::readtext("data-raw/test_document.txt") %>%
  dplyr::mutate(
    doc_id = "doc-1",
    text = stringi::stri_enc_toascii(text)
    ) %>%
  tibble::as_tibble()


usethis::use_data(table_document, overwrite = TRUE)
