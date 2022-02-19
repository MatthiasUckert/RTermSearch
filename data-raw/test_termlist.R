## code to prepare `test_termlist` dataset goes here
library(tidyverse)
table_termlist_short <- tibble(
  term = c(
    "Language Processing", "Natural Language Processing", "Linguistics",
    "Computer Linguistics", "Lemmatization", "Lexical Semantics", "Semantics"
  )
) %>%
  mutate(tid = row_number()) %>%
  select(tid, term)

table_termlist_long <- table_termlist_short %>%
  bind_rows(tibble(
    term = c(
      "NLP", "Research", "Neural Network", "Statistics",
      "Machine Learning", "Natural"
    )
  )) %>%
  mutate(tid = row_number()) %>%
  select(tid, term)


usethis::use_data(table_termlist_short, overwrite = TRUE)
usethis::use_data(table_termlist_long, overwrite = TRUE)
