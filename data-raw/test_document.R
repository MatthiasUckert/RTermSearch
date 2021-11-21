## code to prepare `test_document` dataset goes here
library(tidyverse)
test_document <- readtext::readtext("data-raw/test_document.txt") %>%
  dplyr::mutate(
    doc_id = "doc-1",
    text = stringi::stri_escape_unicode(text)
    ) %>%
  tibble::as_tibble()


usethis::use_data(test_document, overwrite = TRUE)
