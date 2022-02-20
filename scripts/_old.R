#' Prepare Term List
#'
#' @param .tab
#' A Dataframe with at least 2 columns (tid: Term ID, term: Term)
#' @param .fun_std A function to standardize Strings. Default = NULL (no standardization use)
#' @return A Dataframe with a term list column
#' @export
#' @examples
#'
#' termlist <- table_termlist_short
#' prep_termlist(termlist)
# DEBUG
# .tab <- table_termlist_short
# .tab <- tibble::add_row(table_termlist_short, tid = 7, term = "Test")
# .tab <- tibble::add_row(table_termlist_short, tid = 8, term = "Linguistics")
prep_termlist_old <- function(.tab, .fun_std = NULL) {

  # Define Variables --------------------------------------------------------
  term <- tid <- n_dup <- token <- NULL

  # Check Columns in Dataframe ----------------------------------------------
  if (!all(c("tid", "term") %in% colnames(.tab))) {
    stop("Input MUST contain the columns 'tid' and 'term'", call. = FALSE)
  }

  if (any(duplicated(.tab[["tid"]]))) {
    stop("Column 'tid' MUST NOT contain duplicates", call. = FALSE)
  }

  if (any(duplicated(.tab[["term"]]))) {
    stop("Column 'term' MUST NOT contain duplicates", call. = FALSE)
  }


  if (!is.null(.fun_std)) {
    tab_ <- dplyr::mutate(.tab, term = .fun_std(term))
  } else {
    tab_ <- .tab
  }


  # Check for Unique TIDs ---------------------------------------------------
  dup_tid_ <- tab_ %>%
    dplyr::group_by(tid) %>%
    dplyr::summarise(term = list(term), .groups = "drop") %>%
    dplyr::mutate(n_dup = lengths(term)) %>%
    dplyr::filter(n_dup > 1) %>%
    dplyr::select(tid, term, n_dup)

  if (nrow(dup_tid_) > 0) {
    warning("Dataset contains duplicate term identifiers (column: tid). See Output for more Information.")
    return(dup_tid_)
  }

  # Check for Duplicates ----------------------------------------------------
  dup_term_ <- tab_ %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(tid = list(term), .groups = "drop")  %>%
    dplyr::mutate(n_dup = lengths(tid)) %>%
    dplyr::filter(n_dup > 1) %>%
    dplyr::select(tid, term, n_dup)

  if (nrow(dup_term_) > 0) {
    warning("Dataset contains duplicates terms (column: term). See Output for more Information.")
    return(dup_term_)
  }

  tab_ %>%
    dplyr::mutate(
      token = stringi::stri_split_fixed(term, " "),
      hash = purrr::map_chr(term, ~digest::digest(.x, algo = "xxhash32")),
      ngram = lengths(token)
    )

}

#' Retrieve Position of Terms in Documents
#'
#' @param .termlist A term list prepared by prep_termlist()
#' @param .document A tokenized Dataframe prepared by prep_document()
#' @param ... Any number of Columns that mark a separator of tokens (e.g. a sentence)
#'
#' @return A Dataframe
#' @export
#'
# DEBUG
# .termlist <- prep_termlist(table_termlist_short, string_standardization)
# .document <- prep_document(table_document_short, string_standardization)
# quos_ <- dplyr::quos(sen_id, pag_id)
position_count_old <- function(.termlist, .document, ...) {
  # Define Variables --------------------------------------------------------
  doc_id <- token <- ngram <- start <- idx <- id <- dup <- tid <- term <-
    tmp <- tok_id <- NULL

  # Get Quosures ------------------------------------------------------------
  quos_ <- dplyr::quos(...)
  quo_names_ <- as.character(unlist(quos_))

  if (grepl("tok_id|token", quo_names_)) {
    stop("tok_id or token can't be text separators", call. = FALSE)
  }

  # Adjust Term List --------------------------------------------------------
  vec_token <- unique(unlist(.termlist[["token"]]))
  lst_t_ <- split(.termlist, .termlist[["tid"]])

  # Adjust Document ---------------------------------------------------------
  tab_d_ <- .document %>%
    # Remove all tokens that are not in the term list
    dplyr::mutate(
      token = dplyr::if_else(!token %in% vec_token, "_rem_", token)
    ) %>%
    dplyr::filter(!(token == "_rem_" & dplyr::lag(token) == "_rem_")) %>%
    tidyr::unite("tmp", doc_id, !!!quos_, remove = FALSE, sep = "-") %>%
    dplyr::mutate(
      token = dplyr::if_else(tmp != dplyr::lead(tmp), paste0(token, "|_sep_"), token, token),
      token = stringi::stri_split_fixed(token, "|")
    ) %>% tidyr::unnest(token) %>%
    dplyr::select(doc_id, tok_id, token)


  out_ <- purrr::map_dfr(
    .x = lst_t_,
    .f = ~ h_position_count(.x, tab_d_)
  ) %>%
    dplyr::arrange(dplyr::desc(ngram)) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    # Change to real tok_id
    dplyr::mutate(
      stop = tok_id + (stop - start),
      start = tok_id
    )

  dup_ <- out_  %>%
    dplyr::mutate(
      idx = purrr::map2(start, stop, ~.x:.y),
    ) %>%
    tidyr::unnest(idx) %>%
    dplyr::mutate(dup = duplicated(idx)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dup = any(dup), .groups = "drop")

  dplyr::left_join(out_, dup_, by = "id") %>%
    dplyr::left_join(dplyr::select(.termlist, tid, term), by = "tid")  %>%
    dplyr::select(doc_id, tid, start, stop, dup) %>%
    dplyr::arrange(tid, start)
}
