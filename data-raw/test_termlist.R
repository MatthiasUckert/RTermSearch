## code to prepare `test_termlist` dataset goes here
library(tidyverse)
table_termlist_short <- tribble(
  ~term,
  "Language Processing",
  "Natural Language Processing",
  "Linguistics",
  "Computer Linguistics",
  "Lemmatization",
  "Lexical Semantics",
  "Semantics"
)

table_termlist_long <- tribble(
  ~term,
  "Language Processing",
  "Natural Language Processing",
  "Linguistics",
  "Computer Linguistics",
  "Lemmatization",
  "Lexical Semantics",
  "NLP",
  "Semantics",
  "Research",
  "Neural Network",
  "Statistics",
  "Machine Learning",
  "Natural"

)

usethis::use_data(table_termlist_short, overwrite = TRUE)
usethis::use_data(table_termlist_long, overwrite = TRUE)
