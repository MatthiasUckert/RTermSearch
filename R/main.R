#' Check Termlist
#' @param .tab
#' A Dataframe with at least 1 column (term: Term)
#' @param .fun_std A
#' Function to standardize Strings.\cr
#' Default = NULL (no standardization use)\cr
#' Build-in Function .fun_std = string_standardization, can be used.
#'
#' @return
#' Case 1: No Duplicates: A Message with Dependency Factor (unique terms / all terms)\cr
#' Case 2: A Warning Message and a Dataframe with duplicated values (AFTER STANDARDIZATION)
#' @export
#'
#' @examples
#' # Example w/o standardization
#' check_termlist(table_termlist_short, NULL)
#' # Example with standardization
#' check_termlist(table_termlist_short, string_standardization)
#'
#' # Example w/o standardization and duplicated term
#' termlist <- tibble::add_row(table_termlist_short, term = "Language-Processing")
#' check_termlist(termlist, NULL)
#'
#' # Example with standardization and duplicated term
#' check_termlist(termlist, string_standardization)



# DEBUG: check_termlist() -------------------------------------------------
# .tab     <- table_termlist_short
# .fun_std <- string_standardization
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
    vec_all_ <- unlist(stringi::stri_split_fixed(tab_[["term"]], " "))
    vec_uni_ <- unique(vec_all_)
    dep_fac_ <- round(length(vec_uni_) / length(vec_all_), 4)

    message(paste0("Termlist is valid! Dependency Factor: ", dep_fac_))
  }

}

#' Prepare Termlist
#'
#' @param .tab
#' A Dataframe with at least 1 column (term: Term)
#' @param .fun_std A
#' Function to standardize Strings.\cr
#' Default = NULL (no standardization use)\cr
#' Build-in Function .fun_std = string_standardization, can be used.
#' @param .get_dep
#' Get Term List Dependencies (default = FALSE).\cr
#' Term List Dependencies show how n-grams in the term list are related.
#' @param ...
#' Any other column of the original Dataframe (.tab) you want to keep in the output
#'
#' @return
#' A Dataframe with minimum 6 columns:\cr
#' hash: Hashed value (xxhash32) of standardized term\cr
#' ngram: N-Gram of standardized term\cr
#' term_orig: Original term (as defined in parameter .tab)\cr
#' term: Standardized term (with .fun_std)\cr
#' oid: Order ID of token in column token\cr
#' token: Tokienized version of column term\cr
#' dep (optional): Dependencies of terms within the term list\cr
#' ...: Any other column specified by ...
#'
#' @export
#'
#' @examples
#'
#' prep_termlist(table_termlist_short, NULL)
#' prep_termlist(table_termlist_short, string_standardization)
#' prep_termlist(table_termlist_short, string_standardization, TRUE)
#' prep_termlist(table_termlist_short, string_standardization, TRUE, tid)


# DEBUG: prep_termlist() --------------------------------------------------
# .tab     <- table_termlist_short
# .fun_std <- string_standardization
# .get_dep = FALSE
# quos_ <- dplyr::quo(tid)

prep_termlist <- function(.tab, .fun_std = NULL, .get_dep = FALSE, ...) {

  # Get Quosures ------------------------------------------------------------
  quos_ <- dplyr::quos(...)

  tab_ <- h_prep_termlist(.tab, .fun_std, !!!quos_)

  if (.get_dep) {
    tab_ <- h_dependencies_termlist(tab_, !!!quos_)
  }

  return(tab_)
}



#' Prepare Documents
#'
#' @param .tab
#' A Dataframe with at least two columns:\cr
#' doc_id: Unique document identifier\cr
#' text: Document text\cr
#'
#' IMPORTANT NOTE: Dataframe must contain only one row per doc_id
#' @param .fun_std
#' A function to standardize Strings. Default = NULL (no standardization use)
#' @param .until c("tok", "sen", "par", "pag")
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
#' doc <- prep_document(table_document_short, string_standardization)


# DEBUG: prep_document() --------------------------------------------------
# .tab     <- table_document_short
# .fun_std <- string_standardization
# .until = c("tok", "sen", "par", "pag")
prep_document <- function(.tab, .fun_std = NULL, .until = c("tok", "sen", "par", "pag")) {
  until_ <- match.arg(.until, c("tok", "sen", "par", "pag"))

  # Define Variables --------------------------------------------------------
  doc_id <- text <- token <- pag_id <- par_id <- sen_id <- tok_id <- NULL

  # Check Columns in Dataframe ----------------------------------------------
  if (!all(c("doc_id", "text") %in% colnames(.tab))) {
    stop("Input MUST contain the columns 'doc_id' and 'text'", call. = FALSE)
  }

  if (any(duplicated(.tab[["doc_id"]]))) {
    stop("doc_id MUST contain duplicates", call. = FALSE)
  }


  # Tokenize Dataframe ------------------------------------------------------
  # To Page -----------------------------------------------------------------
  tab_ <- .tab %>%
    tidytext::unnest_tokens(
      output = text,
      input = text,
      token = stringi::stri_split_regex, pattern = "\f",
      to_lower = FALSE, drop = FALSE
    ) %>%
    dplyr::mutate(pag_id = dplyr::row_number())

  if (until_ == "pag") {
    if (!is.null(.fun_std)) {
      tab_ <- dplyr::mutate(tab_, text = .fun_std(text))
    }
    tab_ <- dplyr::select(tab_, doc_id, pag_id, token = text)
    return(tab_)
  }


  # To Paragraph ------------------------------------------------------------
  tab_ <- tab_ %>%
    tidytext::unnest_tokens(
      output = text,
      input = text,
      token = stringi::stri_split_regex, pattern = "\n\n",
      to_lower = FALSE, drop = FALSE
    ) %>%
    dplyr::mutate(par_id = dplyr::row_number())

  if (until_ == "par") {
    if (!is.null(.fun_std)) {
      tab_ <- dplyr::mutate(tab_, text = .fun_std(text))
    }
    tab_ <- dplyr::select(tab_, doc_id, pag_id, par_id, token = text)
    return(tab_)
  }


  # To Sentence -------------------------------------------------------------
  tab_ <- tab_ %>%
    tidytext::unnest_tokens(
      output = text,
      input = text,
      token = "sentences",
      to_lower = FALSE
    ) %>%
    dplyr::mutate(sen_id = dplyr::row_number())

  if (until_ == "sen") {
    if (!is.null(.fun_std)) {
      tab_ <- dplyr::mutate(tab_, text = .fun_std(text))
    }
    tab_ <- dplyr::select(tab_, doc_id, pag_id, par_id, sen_id, token = text)
    return(tab_)
  }

  if (!is.null(.fun_std)) {
    tab_ <- dplyr::mutate(tab_, text = .fun_std(text))
  }

  # To Token ----------------------------------------------------------------
  tab_ <- tab_ %>%
    tidytext::unnest_tokens(
      output = token,
      input = text,
      token = stringi::stri_split_fixed,
      pattern = " ",
      to_lower = FALSE
    ) %>%
    dplyr::filter(!token == "") %>%
    dplyr::mutate(tok_id = dplyr::row_number()) %>%
    dplyr::select(doc_id, pag_id, par_id, sen_id, tok_id, token)
}

#' Get Position of Terms
#'
#' @param .termlist A term list prepared by prep_termlist()
#' @param .document A tokenized Dataframe prepared by prep_document()
#' @param ... Any number of Columns that mark a separator of tokens (e.g. a sentence)
#' @param .cache_terms If Terms should be cached in the dataframe ()
#' @param .tab_pos If already cached terms, only new terms will be calculated
#'
#' @return
#' A Dataframe 5 columns:
#' doc_id: Document ID of the dataframe in argument .document prepared by prep_document()\cr
#' hash: Hash value of the terms in argument .termlist prepared by prep_termlist()\cr
#' start: Starting value (tok_id in .document) of the term\cr
#' stop: Ending value (tok_id in .document) of the term\cr\cr
#' dup: Logical indicator if term is contained in higher N-Gram and has already been found
#' @export
#'
#' @import data.table
#'
#' @examples
#' termlist_short <- prep_termlist(table_termlist_short, string_standardization, TRUE, tid)
#' termlist_long  <- prep_termlist(table_termlist_long, string_standardization)
#' document       <- prep_document(table_document_short, string_standardization)
#'
#' tab_pos_short <- position_count(
#'   termlist_short, document, .cache_terms = TRUE, .tab_pos = NULL
#'   )
#'
#' tab_pos_short <- position_count(
#'   termlist_short, document, sen_id, .cache_terms = TRUE, .tab_pos = NULL
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


# DEBUG: position_count() -------------------------------------------------
# check_termlist(table_termlist_long, string_standardization)
# .termlist    <- prep_termlist(table_termlist_long, string_standardization, TRUE, tid)
# duplicate_doc <- function(.tab, .n) {
#   tab_ <- dplyr::mutate(.tab, doc_id = "doc-0")
#   for (i in seq_len(.n)) {
#     tab_ <- dplyr::bind_rows(
#       tab_,
#       dplyr::mutate(.tab, doc_id = paste0("doc-", i))
#     )
#   }
#   return(tab_)
# }
#
# .document    <- prep_document(
#   .tab = duplicate_doc(.tab = table_document_long, .n = 10),
#   .fun_std = string_standardization
#   )
# .cache_terms <- TRUE
# .tab_pos     <- NULL
# quos_        <- dplyr::quos(sen_id)
#
# .termlist0   <- prep_termlist(table_termlist_short, string_standardization, TRUE, tid)
# .termlist1   <- prep_termlist(table_termlist_long, string_standardization, TRUE, tid)
# .tab_pos     <- position_count(.termlist0, .document, sen_id, .cache_terms = TRUE)
position_count <- function(.termlist, .document, ..., .cache_terms = TRUE, .tab_pos = NULL) {

  hash <- token <- pos <- tok_id <- ngram <- term <- oid <- tmp1 <- tmp2 <- dup <-
    doc_id <- start <- tmp <- sen_id <- sen_id.x <- sen_id.y <- par_id <- par_id.x <-
    par_id.y <- pag_id <- pag_id.x <- pag_id.y <- NULL

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
    dplyr::group_by(hash, tmp = cumsum(oid == 1)) %>%
    dplyr::filter(dplyr::row_number() <= ngram) %>%
    dplyr::filter(dplyr::n() == ngram) %>%
    dplyr::filter(all(c(1, diff(oid)) == 1) , all(c(1, diff(pos)) == 1)) %>%
    dplyr::arrange(dplyr::desc(ngram), hash)
  tab_[["dup"]] <- duplicated(tab_$pos)
  tab_ <- tab_ %>%
    dplyr::summarise(
      start = dplyr::first(pos),
      stop = dplyr::last(pos),
      dup  = any(dup),
      doc_id = doc_[["doc_id"]][1],
      .groups = "drop"
    ) %>%
    dplyr::select(-tmp)


  # Remove Hits spanning over Separators ------------------------------------
  if (!length(quos_) == 0) {
    doc_join_ <- dplyr::select(doc_, doc_id, tok_id, !!!quos_)

    tab_ <- tab_ %>%
      dplyr::left_join(doc_join_, by = c("doc_id", "start" = "tok_id")) %>%
      dplyr::left_join(doc_join_, by = c("doc_id", "stop" = "tok_id"))

    if ("sen_id" %in% quos_vec_) {
      tab_ <- tab_ %>%
        dplyr::mutate(sen_id = purrr::map2(sen_id.x, sen_id.y, ~ .x:.y)) %>%
        dplyr::filter(lengths(sen_id) == 1) %>%
        dplyr::select(-sen_id.x, -sen_id.y, -sen_id)
    }

    if ("par_id" %in% quos_vec_) {
      tab_ <- tab_ %>%
        dplyr::mutate(par_id = purrr::map2(par_id.x, par_id.y, ~ .x:.y)) %>%
        dplyr::filter(lengths(par_id) == 1) %>%
        dplyr::select(-par_id.x, -par_id.y, -par_id)
    }

    if ("pag_id" %in% quos_vec_) {
      tab_ <- tab_ %>%
        dplyr::mutate(pag_id = purrr::map2(pag_id.x, pag_id.y, ~ .x:.y)) %>%
        dplyr::filter(lengths(pag_id) == 1) %>%
        dplyr::select(-pag_id.x, -pag_id.y, -pag_id)
    }
  }



  # Combine Results ---------------------------------------------------------
  if (!is.null(.tab_pos)) {
    tab_ <- dplyr::bind_rows(.tab_pos, tibble::as_tibble(tab_)) %>%
      dplyr::arrange(dplyr::desc(stop - start), hash) %>%
      dplyr::mutate(
        tmp = purrr::map2(start, stop, ~.x:.y),
        dup = utils::relist(duplicated(unlist(tmp)), tmp),
        dup = purrr::map_lgl(dup, any)
      )  %>%
      dplyr::select(doc_id, hash, start, stop, dup) %>%
      dtplyr::lazy_dt()

  }


  # Retrieve additional Term List columns -----------------------------------
  cn_ <- colnames(.termlist)
  cn_ <- cn_[!cn_ %in% c("ngram", "term_orig", "term", "oid", "token", "parents", "children")]


  if (length(cn_) > 1) {
    termlist_ <- .termlist[, cn_]
    tab_ <- dplyr::left_join(tab_, termlist_, by = "hash") %>%
      dplyr::arrange(hash, start) %>%
      tibble::as_tibble()
  } else {
    tab_ <- tab_ %>%
      dplyr::arrange(hash, start) %>%
      tibble::as_tibble()
  }

  # Cache Terms -------------------------------------------------------------
  if (.cache_terms) {
    attr(tab_, "cache-terms") <- unique(c(attr(tab_, "cache-terms"), term_list_[["hash"]]))
  }


  # Add Separators ----------------------------------------------------------
  attr(tab_, "separators") <- quos_vec_

  dplyr::select(tab_, doc_id, hash, start, stop, dup, dplyr::everything())

}



#' Get Context
#'
#' @param .position
#' A Dataframe produced by position_count()
#' @param .document
#' A Dataframe produced by prep_document()
#' @param .n
#' Number of context token to retrieve, default = NA, which means no token context is retrieved
#' @param .context
#' either "word", "sentence", "paragraph", or "page"
#' @param .vars
#' Any combination of "sentence", "paragraph", and "page" (get the specific locations of hits)\cr
#' If not specified, no variable context is retrieved
#'
#' @return
#' A dataframe
#' @export
#'
#' @examples
#' termlist_short <- prep_termlist(table_termlist_short, string_standardization, TRUE, tid)
#' document       <- prep_document(table_document_short, string_standardization)
#'
#' tab_pos_short <- position_count(
#'   termlist_short, document, sen_id, .cache_terms = TRUE, .tab_pos = NULL
#' )
#'
#' tab_context_word     <- get_context(tab_pos_short, document, 5, "word")
#' tab_context_sentence <- get_context(tab_pos_short, document, 1, "sentence")
#' tab_context_vars     <- get_context(tab_pos_short, document, .vars = "page")

# DEBUG: get_context() ----------------------------------------------------
# .termlist <- prep_termlist(table_termlist_short, string_standardization, TRUE, tid)
# .document <- prep_document(table_document_short, string_standardization)
# .position <- position_count(.termlist, .document, sen_id)
# .n <- 0
# .context <- "sentence"
# .vars = c("sentence", "paragraph", "page")
get_context <- function(
  .position, .document, .n = NA,
  .context = c("word", "sentence", "paragraph", "page"),
  .vars = c("none", "sentence", "paragraph", "page")
  ) {

  tok_id <- pre <- post <- hash <- hit <- point <- start <- token <- name <-
    term <- doc_id <- n <- NULL

  context_ <- match.arg(.context, c("word", "sentence", "paragraph", "page"))
  vars_ <- stringi::stri_replace_all_fixed(
    .vars, c("sentence", "paragraph", "page"), c("sen_id", "par_id", "pag_id"), FALSE
    )

  quo_ <- switch(context_,
    "word" = "tok_id",
    "sentence" = "sen_id",
    "paragraph" = "par_id",
    "page" = "pag_id"
  )

  if (!is.na(.n) && context_ == "word" & .n == 0) {
    stop("With .context = word, .n must be greater than 0")
  }


  if (length(unique(.document[[quo_]])) == 1) {
    stop(
      paste0("Document contains only 1 ", context_, ". Not possible to get context.")
    )
  }

  doc_ <- .document %>%
    dplyr::mutate(pre = !!dplyr::sym(quo_), post = !!dplyr::sym(quo_), point = !!dplyr::sym(quo_))

  sep_ <- ifelse(context_ == "word", " ", "<>")


  # Retrieve Token Context --------------------------------------------------
  if (!is.na(.n)) {
    tab_ <- .position %>%
      dplyr::left_join(dplyr::select(doc_, tok_id, pre), by = c("start" = "tok_id")) %>%
      dplyr::left_join(dplyr::select(doc_, tok_id, post), by = c("start" = "tok_id")) %>%
      dplyr::mutate(
        hit = dplyr::row_number(),
        point = purrr::map2(pre, post, ~ (.x - .n):(.y + .n)),
        pre = NULL,
        post = NULL
      ) %>%
      tidyr::unnest(point) %>%
      dtplyr::lazy_dt() %>%
      dplyr::inner_join(dplyr::select(doc_, tok_id, point, token), by = "point") %>%
      dplyr::group_by(hit) %>%
      dplyr::mutate(
        col = dplyr::case_when(
          tok_id >= start & tok_id <= stop ~ "term",
          tok_id < start ~ "pre",
          tok_id > stop ~ "post"
        )
      ) %>%
      dplyr::group_by(doc_id, hash, start, stop, hit, col, point) %>%
      dplyr::summarise(token = paste(token, collapse = " "), .groups = "drop_last") %>%
      dplyr::summarise(token = paste(token, collapse = sep_), n = .n, .groups = "drop") %>%
      tidyr::pivot_wider(names_from = col, values_from = token) %>%
      dplyr::select(hash, start, stop, pre, term, post, n) %>%
      dplyr::arrange(hash, start)
  } else {
    tab_ <- dtplyr::lazy_dt(dplyr::select(.position, hash, start, stop))
  }


  # Retrieve Variable Context -----------------------------------------------
  if (!"none" %in% vars_) {
    doc_ <- .document[, c("tok_id", vars_)] %>%
      dplyr::rename(start = tok_id)

    tab_ <- tab_ %>%
      dplyr::left_join(doc_, by = "start") %>%
      dplyr::arrange(hash, start)
  }


  # Retrieve Additional Context ---------------------------------------------
  cn_ <- colnames(.position)
  cn_ <- cn_[!cn_ %in% c("doc_id", "hash", "start", "stop", "dup", "nterm")]


  if (length(cn_) > 0) {
    pos_ <- .position[, cn_]
    dplyr::bind_cols(tibble::as_tibble(tab_), pos_) %>%
      dplyr::mutate(doc_id = .position$doc_id[1]) %>%
      dplyr::select(doc_id, dplyr::everything())
  } else {
    tibble::as_tibble(tab_) %>%
      dplyr::mutate(doc_id = .position$doc_id[1]) %>%
      dplyr::select(doc_id, dplyr::everything())
  }

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
