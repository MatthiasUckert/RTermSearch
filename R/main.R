#' Prepare Term List
#'
#' @param .tab
#' A Dataframe with at least 2 columns (tid: Term ID, term: Term)
#' @return A Dataframe with a term list column
#' @export
#' @examples
#'
#' termlist <- test_termlist
#' prep_termlist(termlist)
# DEBUG
# .tab <- dplyr::bind_rows(test_termlist, test_termlist[1:4, ])
prep_termlist <- function(.tab) {

  # Define Variables --------------------------------------------------------
  term <- tid <- n_dup <- NULL

  # Check Columns in Dataframe ----------------------------------------------
  if (!all(c("tid", "term") %in% colnames(.tab))) {
    stop("Input MUST contain the columns 'tid' (Term ID) and 'term'", call. = FALSE)
  }


  # Check for Unique TIDs ---------------------------------------------------
  dup_tid_ <- .tab %>%
    dplyr::group_by(tid) %>%
    dplyr::summarise(term = list(term), .groups = "drop") %>%
    dplyr::mutate(n_dup = lengths(term)) %>%
    dplyr::filter(n_dup > 1) %>%
    dplyr::select(tid, term, n_dup)

  if (nrow(dup_tid_) > 0) {
    warning("Dataset contains duplicate term identifiers (see output)")
    return(dup_tid_)
  }

  # Check for Duplicates ----------------------------------------------------
  dup_term_ <- .tab %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(tid = list(term), .groups = "drop")  %>%
    dplyr::mutate(n_dup = lengths(term)) %>%
    dplyr::filter(n_dup > 1) %>%
    dplyr::select(tid, term, n_dup)

  if (nrow(dup_term_) > 0) {
    warning("Dataset contains duplicates terms (see output)")
    return(dup_term_)
  }

  dplyr::mutate(.tab, term = stringi::stri_split_fixed(term, " "))

}


#' #' Prepare Term List Table for position_count()
#' #'
#' #' @param .table_text
#' #' A Dataframe with at least 2 columns (doc_id: A Document Identifier, text: Text) \cr
#' #' @param .use_udpipe
#' #' Use UDPipe for tokenization (if FALSE the default, other parameters are not important)
#' #' @param .lan UDPipe language (see ?udpipe_download_model)
#' #' @param .dir_mod UDPipe model directory
#' #' @param .tagger one of c("default", "none")
#' #' @param .parser one of c("none", "default")
#' #' @param .return_raw return the UDPipe raw output
#' #' @return A tokenized Dataframe
#' #' @export
#' #' @importFrom rlang .data
#' #' @examples
#' prepare_table_text <- function(.table_text, .use_udpipe = FALSE,
#'                                .lan = "english-ewt", .dir_mod = getwd(),
#'                                .tagger = c("default", "none"),
#'                                .parser = c("none", "default"),
#'                                .return_raw = FALSE) {
#'   text <- token <- NULL
#'   # Check if Inputs are Dataframes ------------------------------------------
#'   if (!is.data.frame(.table_text)) {
#'     stop("'.table_terms' MUST be a dataframe", call. = FALSE)
#'   }
#'
#'   # Check Columns in Dataframe ----------------------------------------------
#'   if (!all(c("doc_id", "text") %in% colnames(.table_text))) {
#'     stop("'.table_text' MUST contain the columns 'doc_id' and 'text'", call. = FALSE)
#'   }
#'
#'   if (!.use_udpipe) {
#'     tab_token <- .table_text %>%
#'       tidytext::unnest_tokens(
#'         text, .data$text,
#'         token = stringi::stri_split_regex, pattern = "\n\n", to_lower = FALSE
#'       ) %>%
#'       dplyr::group_by(.data$doc_id) %>%
#'       dplyr::mutate(par_id = dplyr::row_number()) %>%
#'       dplyr::ungroup() %>%
#'       tidytext::unnest_tokens(
#'         text, .data$text, token = "sentences", to_lower = FALSE
#'       ) %>%
#'       dplyr::group_by(.data$doc_id) %>%
#'       dplyr::mutate(sen_id = dplyr::row_number()) %>%
#'       dplyr::ungroup() %>%
#'       tidytext::unnest_tokens(token, .data$text) %>%
#'       dplyr::mutate(token = stringi::stri_replace_all_regex(token, "[[:punct:]]", "")) %>%
#'       dplyr::filter(!token == "") %>%
#'       dplyr::filter(!is.na(token)) %>%
#'       dplyr::group_by(.data$doc_id) %>%
#'       dplyr::mutate(tok_id = dplyr::row_number()) %>%
#'       dplyr::ungroup() %>%
#'       dplyr::select(.data$doc_id, .data$par_id, .data$sen_id, .data$tok_id, .data$token)
#'   } else {
#'     .parser <- match.arg(.parser)
#'     .tagger <- match.arg(.tagger)
#'     tab_mod <- udpipe::udpipe_download_model(.lan, .dir_mod)
#'     mod <- udpipe::udpipe_load_model(tab_mod$file_model)
#'
#'     tab_token <- .table_text %>%
#'       udpipe::udpipe(mod, parser = .parser, tagger = .tagger) %>%
#'       tibble::as_tibble()
#'
#'     if (!.return_raw) {
#'       tab_token <- tab_token %>%
#'         dplyr::filter(!.data$upos %in% c("PART", "PUNCT")) %>%
#'         dplyr::select(.data$doc_id,
#'                       par_id = .data$paragraph_id, sen_id = .data$sentence_id,
#'                       tok_id = .data$term_id, .data$token, .data$lemma
#'         )
#'     }
#'   }
#' }
