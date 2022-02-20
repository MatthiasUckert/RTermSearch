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
      "Machine Learning", "Natural", "Word Count", "Text Mining",
      "Text Data", "Text Dataset", "Number of Words", "Tokenizing",
      "Tokening N-Grams", "Unigram", "Bigram", "Analyzing Bigram",
      "Sentiment Analysis", "Network", "Network Analysis", "Vizualization",
      "Document", "Document Metadata", "Metafata", "Sentiment Terms", "Negative",
      "Strongly Negative", "Positive", "Strongly Positive", "Positive Sentiment",
      "Negative Sentiment", "Latent Dirichlet Allocation", "LDA", "Word Topic",
      "Topic", "Multiple Topic", "Book", "Book Chapter", "Topic Modelling",
      "Unsupervised Learning", "Unsupervised Clustering", "Word Frequency",
      "Word Frequencies", "Stop Words", "Stop Word", "Total Number of Words"



    )
  )) %>%
  mutate(tid = row_number()) %>%
  select(tid, term)


usethis::use_data(table_termlist_short, overwrite = TRUE)
usethis::use_data(table_termlist_long, overwrite = TRUE)
