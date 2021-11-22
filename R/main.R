#' Prepare Term List
#'
#' @param .tab
#' A Dataframe with at least 2 columns (tid: Term ID, term: Term)
#' @param .fun_std A function to standardize Strings. Default = NULL (no standardization use)
#' @return A Dataframe with a term list column
#' @export
#' @examples
#'
#' termlist <- test_termlist
#' prep_termlist(termlist)
# DEBUG
# .tab <- test_termlist
# .tab <- tibble::add_row(test_termlist, tid = 7, term = "Test")
# .tab <- tibble::add_row(test_termlist, tid = 8, term = "Linguistics")
prep_termlist <- function(.tab, .fun_std = NULL) {

  # Define Variables --------------------------------------------------------
  term <- tid <- n_dup <- NULL

  # Check Columns in Dataframe ----------------------------------------------
  if (!all(c("tid", "term") %in% colnames(.tab))) {
    stop("Input MUST contain the columns 'tid' and 'term'", call. = FALSE)
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

  dplyr::mutate(tab_, token = stringi::stri_split_fixed(term, " "))

}

#' Perpare Documents
#'
#' @param .tab
#' A Dataframe with at least two columns:\cr
#' doc_id: Unique document identifier\cr
#' text: Document text\cr
#'
#' IMPORTANT NOTE: Dataframe must contain only one row per doc_id
#' @param .fun_std A function to standardize Strings. Default = NULL (no standardization use)
#'
#' @return
#' A Dataframe with the following columns:\cr
#' doc_id: Unique document identifier\cr
#' pag_id: Page ID of token\cr
#' par_id: Paragraph ID of token\cr
#' sen_id: Sentence ID of token\cr
#' tok_id: Token ID\cr
#' token: Token (tokenized using white spaces)
#' @export
#'
#' @examples
#' doc <- prep_document(test_document, string_standardization)
# DEBUG
# .tab <- dplyr::bind_rows(test_document, dplyr::mutate(test_document, doc_id = "doc-2"))
# .fun_std <- string_standardization
prep_document <- function(.tab, .fun_std = NULL) {
  # Define Variables --------------------------------------------------------
  doc_id <- text <- token <- pag_id <- par_id <- sen_id <- tok_id <- NULL

  # Check Columns in Dataframe ----------------------------------------------
  if (!all(c("doc_id", "text") %in% colnames(.tab))) {
    stop("Input MUST contain the columns 'doc_id' and 'term'", call. = FALSE)
  }

  if (any(duplicated(.tab[["doc_id"]]))) {
    stop("doc_id MUST contain duplicates", call. = FALSE)
  }


  # Tokenize Dataframe ------------------------------------------------------
  tab_ <- .tab %>%
    tidytext::unnest_tokens(
      output = text,
      input = text,
      token = stringi::stri_split_regex, pattern = "\f",
      to_lower = FALSE
    ) %>%
    dplyr::group_by(doc_id) %>%
    dplyr::mutate(pag_id = dplyr::row_number(), .before = text) %>%
    dplyr::ungroup() %>%
    tidytext::unnest_tokens(
      output = text,
      input = text,
      token = stringi::stri_split_regex, pattern = "\n\n",
      to_lower = FALSE, drop = FALSE
    ) %>%
    dplyr::group_by(doc_id) %>%
    dplyr::mutate(par_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidytext::unnest_tokens(
      output = text,
      input = text,
      token = "sentences",
      to_lower = FALSE
    ) %>%
    dplyr::group_by(doc_id) %>%
    dplyr::mutate(sen_id = dplyr::row_number()) %>%
    dplyr::ungroup()

  if (!is.null(.fun_std)) {
    tab_ <- dplyr::mutate(tab_, text = .fun_std(text))
  }

  tab_ <- tab_ %>%
    tidytext::unnest_tokens(
      output = token,
      input = text,
      token = stringi::stri_split_fixed,
      pattern = " ",
      to_lower = FALSE
    ) %>%
    dplyr::filter(!token == "") %>%
    dplyr::group_by(doc_id) %>%
    dplyr::mutate(tok_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(doc_id, pag_id, par_id, sen_id, tok_id, token, dplyr::everything())
}

#
# .termlist <- prep_termlist(dplyr::mutate(test_termlist, term = tolower(term)))
# .document <- prep_document(test_document)
# quos_ <- dplyr::quos(sen_id)


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
# .termlist <- prep_termlist(test_termlist, string_standardization)
# .document <- prep_document(test_document, string_standardization)
# quos_ <- dplyr::quos(sen_id, pag_id)
position_count <- function(.termlist, .document, ...) {
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
    dplyr::select(doc_id, ngram, tid, start, stop, dup, term)
}


#' Basic String Standardization
#'
#' @param .str A string
#'
#' @return A string
#' @export
#'
#' @examples
#' string_standardization("  TesT String")
string_standardization <- function(.str) {
  .str %>%
    stringi::stri_escape_unicode() %>%
    stringi::stri_enc_toascii() %>%
    tolower() %>%
    stringi::stri_replace_all_regex("([[:blank:]]|[[:space:]])+", " ") %>%
    stringi::stri_replace_all_regex("[[:punct:]]", " ") %>%
    trimws()
}
