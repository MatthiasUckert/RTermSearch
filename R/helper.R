#' Find a sequence in another sequence
#'
#' @param .seq_find The sequence you want to find in another sequence
#' @param .seq_base The sequence to be searched
#'
#' @return An integer vector (vector has length zero if sequence is not found)
# # Find an integer sequence
# find_seq_in_seq(2:10, c(1:20, 1:20))
#
# # Find a character sequence
# find_seq_in_seq(c("C", "D"), LETTERS)
#
# # No sequence found
# find_seq_in_seq(c("R", "D"), LETTERS)

find_seq_in_seq <- function(.seq_find, .seq_base) {
  w <- seq_along(.seq_base)
  for (i in seq_along(.seq_find)) {
    w <- w[.seq_base[w + i - 1L] == .seq_find[i]]
    if (length(w) == 0) return(integer(0))
  }
  w <- w[!is.na(w)]
  return(w)
}

#' Helper Function for position_count()
#'
#' @param .row_terms A one row Dataframe prepared by prep_termlist()
#' @param .document A tokenized Dataframe prepared by prep_document()
#'
#' @return A Dataframe
h_position_count <- function(.row_terms, .document) {
  # Define Variables --------------------------------------------------------
  start <- tid <- ngram <- token <- check <- NULL

  vec_t_ <- unlist(.row_terms[["term"]])
  tab_d_ <- .document
  len_t_ <- length(vec_t_)

  if (len_t_== 1) {
    tab_pos_ <- tibble::tibble(
      start = which(tab_d_[["text"]] == vec_t_)
    )
  } else {
    tab_pos_ <- tibble::tibble(
      start = find_seq_in_seq(vec_t_, tab_d_[["token"]])
    )
  }

  tab_pos_ <- tab_pos_ %>%
    dplyr::mutate(
      tid   = .row_terms[["tid"]],
      stop  = start + len_t_ - 1L,
      ngram = len_t_,
    ) %>% dplyr::select(tid, ngram, start, stop)


  vec_check_ <- tab_d_ %>%
    dplyr::select(-token) %>%
    tidyr::unite(check, dplyr::everything(), sep = "-") %>%
    dplyr::pull(check)
  lgl_check_ <- vec_check_[tab_pos_[["start"]]] == vec_check_[tab_pos_[["stop"]]]

  tab_pos_ %>%
    dplyr::filter(lgl_check_) %>%
    dplyr::left_join(
      y  = dplyr::mutate(tab_d_, merging_id = dplyr::row_number()),
      by = c("start" = "merging_id")) %>%
    dplyr::select(-token)
}
