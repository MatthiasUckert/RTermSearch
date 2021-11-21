## code to prepare `test_termlist` dataset goes here
library(tidyverse)
test_termlist <- tribble(
  ~tid, ~term,
  1, "Language Processing",
  2, "Natural Language Processing",
  3, "Linguistics",
  4, "Computer Linguistics",
  5, "Lemmatization",
  6, "Lexical Semantics",
  7, "Semantics"
)

usethis::use_data(test_termlist, overwrite = TRUE)
