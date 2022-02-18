#' Check Termlist
#' @param .tab
#' A Dataframe with at least 1 column (term: Term)
#' @param .fun_std A
#'  function to standardize Strings. Default = NULL (no standardization use)
#'
#' @return Dataframe and Messages/Warnings
#' @export
#'
#' @examples
#' check_termlist(table_termlist_short, NULL)
#' check_termlist(table_termlist_short, string_standardization)
#'
#' termlist <- tibble::add_row(table_termlist_short, term = "Language-Processing")
#' check_termlist(termlist, NULL)
#' check_termlist(termlist, string_standardization)
check_termlist <- function(.tab, .fun_std = NULL) {
  term <- dup_id <- term_orig <- NULL

  # Check Columns in Dataframe ----------------------------------------------
  if (!"term" %in% colnames(.tab)) {
    stop("Input MUST contain the column 'term'", call. = FALSE)
  }

  # Standardize Terms -------------------------------------------------------
  if (!is.null(.fun_std)) {
    tab_ <- dplyr::mutate(.tab, term_orig = term, term = .fun_std(term))
  } else {
    tab_ <- dplyr::mutate(.tab, term_orig = term)
  }

  tab_dup_ <- tab_ %>%
    dplyr::group_by(term) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::mutate(dup_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::select(dup_id, term_orig, term)

  if (nrow(tab_dup_) > 0) {
    warning(
      "Termlist contains duplicated terms. Please ensure unique terms.",
      call. = FALSE
    )

    return(tab_dup_)
  } else {
    message("Termlist is valid :)")
  }

}

#' Prepare Termlist
#'
#' @param .tab
#' A Dataframe with at least 1 column (term: Term)
#' @param .fun_std A
#' Function to standardize Strings. Default = NULL (no standardization use)
#' @param .get_dep
#' Get Term List Dependencies (default = FALSE)
#'
#' @return A Dataframe
#' @export
#'
#' @examples
#'
#' prep_termlist(table_termlist_short, NULL)
#' prep_termlist(table_termlist_short, string_standardization)
#' prep_termlist(table_termlist_short, string_standardization, TRUE)
prep_termlist <- function(.tab, .fun_std = NULL, .get_dep = FALSE) {
  tab_ <- h_prep_termlist(.tab, .fun_std)

  if (.get_dep) {
    tab_ <- h_dependencies_termlist(tab_)
  }

  return(tab_)
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
#' doc <- prep_document(table_document, string_standardization)
# DEBUG
# .tab <- dplyr::bind_rows(table_document, dplyr::mutate(table_document, doc_id = "doc-2"))
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
# .termlist <- prep_termlist(dplyr::mutate(table_termlist_short, term = tolower(term)))
# .document <- prep_document(table_document)
# quos_ <- dplyr::quos(sen_id)


#' Get Position of Terms
#'
#' @param .termlist A term list prepared by prep_termlist()
#' @param .document A tokenized Dataframe prepared by prep_document()
#' @param ... Any number of Columns that mark a separator of tokens (e.g. a sentence)
#' @param .cache_terms If Terms should be cached in the dataframe ()
#' @param .tab_pos If already cached terms, only new terms will be calculated
#'
#' @return A Dataframe
#' @export
#'
#' @import data.table
#'
#' @examples
#' termlist_short <- prep_termlist(table_termlist_short, string_standardization)
#' termlist_long  <- prep_termlist(table_termlist_long, string_standardization)
#' document       <- prep_document(table_document, string_standardization)
#'
#' tab_pos_short <- position_count(
#'   termlist_short, document, sen_id, .cache_terms = TRUE, .tab_pos = NULL
#'   )
#'
#' tab_pos_short <- position_count(
#'   termlist_short, document, .cache_terms = TRUE, .tab_pos = NULL
#'   )
#'
#' tab_pos_long1  <- position_count(
#'   termlist_long, document, sen_id, .cache_terms = TRUE, .tab_pos = NULL
#' )
#'
#' tab_pos_long2  <- position_count(
#'   termlist_long, document, sen_id, .cache_terms = TRUE, .tab_pos = tab_pos_short
#' )
#' all.equal(tab_pos_long1, tab_pos_long2, check.attributes = FALSE)

# .termlist <- prep_termlist(table_termlist_short, string_standardization)
# .document <- prep_document(table_document, string_standardization)
# .cache_terms = TRUE
# .tab_pos = NULL
# quos_ <- dplyr::quos(sen_id)

position_count <- function(.termlist, .document, ..., .cache_terms = TRUE, .tab_pos = NULL) {


  hash <- token <- pos <- tok_id <- ngram <- term <- oid <- tmp1 <- tmp2 <- dup <-
    doc_id <- start <- tmp <- NULL

  # Get Quosures ------------------------------------------------------------
  quos_ <- dplyr::quos(...)
  quos_vec_ <- sort(gsub("~", "", as.character(unlist(quos_))))
  quos_reg_ <- paste(quos_vec_, collapse = "|")

  # Get cached terms --------------------------------------------------------
  if (!is.null(.tab_pos)) {
    hashes_ <- attr(.tab_pos, "cache-terms")
  } else {
    hashes_ <- NULL
  }

  # Filter Termlist for calculated Terms ------------------------------------
  term_list_ <- dplyr::filter(.termlist, !hash %in% hashes_)
  if (nrow(term_list_) == 0) {
    return(.tab_pos)
  }


  # Prep Input --------------------------------------------------------------
  vec_ <- unique(unlist(term_list_$token))
  vec_ <- purrr::set_names(vec_, vec_)
  doc_ <- dplyr::filter(.document, token %in% vec_)


  # get Positions -----------------------------------------------------------
  tab_pos_ <- purrr::map(vec_, ~ which(.x == doc_$token)) %>%
    tibble::enframe(name = "token", value = "pos") %>%
    tidyr::unnest(pos) %>%
    dplyr::mutate(pos = doc_[["tok_id"]][pos]) %>%
    dplyr::left_join(dplyr::select(doc_, pos = tok_id, !!!quos_), by = "pos")


  # Prepare Output ----------------------------------------------------------
  tab_ <- term_list_ %>%
    dplyr::select(hash, ngram, term, oid, token) %>%
    tidyr::unnest(c(oid, token)) %>%
    dtplyr::lazy_dt() %>%
    dplyr::inner_join(tab_pos_, by = "token") %>%
    dplyr::arrange(hash, pos, oid) %>%
    dplyr::group_by(hash, tmp1 = cumsum(oid == 1)) %>%
    dplyr::mutate(
      tmp2 = dplyr::if_else(dplyr::row_number() > ngram, NA_integer_, tmp1)
      ) %>%
    dplyr::filter(!is.na(tmp2)) %>%
    dplyr::filter(c(1, diff(oid)) == 1) %>%
    dplyr::filter(c(1, diff(pos)) == 1) %>%
    dplyr::filter(dplyr::n() == ngram) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(ngram), hash) %>%
    dplyr::mutate(dup = duplicated(pos)) %>%
    dplyr::group_by(hash, tmp2) %>%
    dplyr::summarise(
      start = dplyr::first(pos),
      stop = dplyr::last(pos),
      dup  = any(dup),
      doc_id = doc_[["doc_id"]][1],
      .groups = "drop"
    ) %>%
    tibble::as_tibble()

  if (!length(quos_) == 0) {
    tab_ <- tab_ %>%
      dplyr::mutate(dplyr::across(dplyr::matches(quos_reg_), ~ length(unique(.)) == 1)) %>%
      dplyr::filter(dplyr::if_all(dplyr::matches(quos_reg_)))
  }

  tab_ <- tab_ %>%
    dplyr::select(doc_id, hash, start, stop, dup) %>%
    dplyr::arrange(hash, start)


  # Cache Terms -------------------------------------------------------------
  if (.cache_terms) {
    attr(tab_, "cache-terms") <- unique(c(attr(tab_, "cache-terms"), term_list_[["hash"]]))
  }


  # Add Separators ----------------------------------------------------------
  attr(tab_, "separators") <- quos_vec_


  # Combine Results ---------------------------------------------------------
  if (!is.null(.tab_pos)) {
    tab_ <- dplyr::bind_rows(.tab_pos, tab_) %>%
      dplyr::arrange(dplyr::desc(stop - start), hash) %>%
      dplyr::mutate(
        tmp = purrr::map2(start, stop, ~.x:.y),
        dup = utils::relist(duplicated(unlist(tmp)), tmp),
        dup = purrr::map_lgl(dup, any)
      )  %>%
      dplyr::select(doc_id, hash, start, stop, dup) %>%
      dplyr::arrange(hash, start)

  }

  return(tab_)

}



#' Get Context
#'
#' @param .position A Dataframe produced by position_count()
#' @param .document A Dataframe produced by prep_document()
#' @param .n Number of context token to retrieve
#' @param .context either "word", "sentence", "paragraph", or "page"
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' termlist_short <- prep_termlist(table_termlist_short, string_standardization)
#' document       <- prep_document(table_document, string_standardization)
#'
#' tab_pos_short <- position_count(
#'   termlist_short, document, sen_id, .cache_terms = TRUE, .tab_pos = NULL
#' )
#'
#' tab_context_word <- get_context(tab_pos_short, document, 5, "word")
#' tab_context_sentence <- get_context(tab_pos_short, document, 1, "sentence")

get_context <- function(.position, .document, .n, .context = c("word", "sentence", "paragraph", "page")) {
  context_ <- match.arg(.context, c("word", "sentence", "paragraph", "page"))

  tok_id <- pre <- post <- hash <- hit <- point <- start <- token <- name <-
    term <- NULL


  quo_ <- switch(context_,
    "word" = "tok_id",
    "sentence" = "sen_id",
    "paragraph" = "par_id",
    "page" = "pag_id"
  )

  if (length(unique(.document[[quo_]])) == 1) {
    stop(
      paste0("Document contains only 1 ", context_, ". Not possible to get context.")
    )
  }

  doc_ <- dplyr::mutate(.document, pre = !!dplyr::sym(quo_), post = !!dplyr::sym(quo_))


  .position %>%
    dplyr::left_join(dplyr::select(doc_, tok_id, pre), by = c("start" = "tok_id")) %>%
    dplyr::left_join(dplyr::select(doc_, tok_id, post), by = c("start" = "tok_id")) %>%
    dplyr::mutate(
      point = purrr::map2(pre, post, ~ sort(.x:.y)),
      pre = purrr::map(pre, ~ sort((.x - 1):(.x - .n))),
      post = purrr::map(post, ~ sort((.x + 1):(.x + .n))),
      hit = dplyr::row_number()
    ) %>%
    dplyr::select(hash, hit, pre, point, post, start, stop) %>%
    tidyr::pivot_longer(c(pre, point, post), values_to = quo_) %>%
    tidyr::unnest(!!dplyr::sym(quo_)) %>%
    dplyr::left_join(dplyr::select(.document, !!dplyr::sym(quo_), tok_id, token), by = quo_) %>%
    dplyr::filter(!is.na(token)) %>%
    dplyr::mutate(
      name = dplyr::if_else(tok_id >= start & tok_id <= stop, "term", name),
      name = dplyr::if_else(name == "point", NA_character_, name)
    ) %>%
    tidyr::fill(name, .direction = "down") %>%
    dplyr::mutate(
      name = dplyr::if_else(!(tok_id >= start & tok_id <= stop) & name == "term", NA_character_, name)
    ) %>%
    tidyr::fill(name, .direction = "up") %>%
    dplyr::group_by(hash, hit, name, start, stop, !!dplyr::sym(quo_)) %>%
    dplyr::summarise(token = paste(token, collapse = " "), .groups = "drop_last") %>%
    dplyr::summarise(token = paste(token, collapse = " <> "), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = name, values_from = token) %>%
    dplyr::mutate(dplyr::across(c(pre, term, post), ~ dplyr::if_else(!is.na(.) & context_ == "word", gsub(" <> ", " ", .), .))) %>%
    dplyr::select(hash, start, stop, pre, term, post)
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
    stringi::stri_replace_all_regex("[[:punct:]]", " ") %>%
    stringi::stri_replace_all_regex("([[:blank:]]|[[:space:]])+", " ") %>%
    stringi::stri_escape_unicode() %>%
    stringi::stri_enc_toascii() %>%
    tolower() %>%
    trimws()
}
