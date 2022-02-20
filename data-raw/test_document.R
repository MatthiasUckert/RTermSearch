## code to prepare `test_document` dataset goes here
library(tidyverse)
table_document_short <- readtext::readtext("data-raw/test_document.txt") %>%
  dplyr::mutate(
    doc_id = "doc-1",
    text = stringi::stri_enc_toascii(text)
    ) %>%
  tibble::as_tibble()

fil <- RFgen::lfc("data-raw/", "html")

table_document_long <- purrr::map_dfr(
  .x = fil,
  .f = ~ tibble(text = rvest::html_text2(rvest::read_html(.x)))
) %>% summarise(text = paste(text, collapse = "\f")) %>%
  mutate(doc_id = "doc-2") %>%
  select(doc_id, text)

usethis::use_data(table_document_short, overwrite = TRUE)
usethis::use_data(table_document_long, overwrite = TRUE)
