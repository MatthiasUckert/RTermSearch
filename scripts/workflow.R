devtools::load_all(".")




.termlist <- prep_termlist(test_termlist, string_standardization)
.document <- prep_document(test_document, string_standardization)
quos_ <- dplyr::quos(sen_id)
.cache_terms = TRUE
.tab_pos = NULL

.tab_pos <- position_count(.termlist, .document, sen_id, .cache_terms = TRUE, .tab_pos = NULL)
.termlist <- test_termlist %>%
  tibble::add_row(tid = 8, term = "Process") %>%
  prep_termlist(string_standardization)
# .termlist <- tab_
# .document <- doc_
position_count <- function(.termlist, .document, ..., .cache_terms = TRUE, .tab_pos = NULL) {

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
  term_list_ <- dplyr::filter(.termlist, !tid %in% hashes_)
  if (nrow(term_list_) == 0) {
    return(.tab_pos)
  }


  # Prep Input --------------------------------------------------------------
  vec_ <- unique(unlist(term_list_$token))
  vec_ <- purrr::set_names(vec_, vec_)
  doc_ <- dplyr::filter(.document, token %in% vec_)

  tab_pos_ <- purrr::map(vec_, ~ which(.x == doc_$token)) %>%
    tibble::enframe(name = "token", value = "pos") %>%
    tidyr::unnest(pos) %>%
    dplyr::mutate(pos = doc_[["tok_id"]][pos]) %>%
    dplyr::left_join(dplyr::select(doc_, pos = tok_id, !!!quos_), by = "pos")

  tab_ <- term_list_ %>%
    dplyr::select(tid, ngram, term, oid, token) %>%
    tidyr::unnest(c(oid, token)) %>%
    dplyr::left_join(tab_pos_, by = "token") %>%
    dplyr::arrange(tid, pos, oid) %>%
    dplyr::group_by(tid, group = cumsum(oid == 1)) %>%
    dplyr::filter(c(1, diff(oid)) == 1) %>%
    dplyr::filter(c(1, diff(pos)) == 1) %>%
    dplyr::filter(dplyr::n() == ngram) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(ngram), tid) %>%
    dplyr::mutate(dup = duplicated(pos)) %>%
    dplyr::group_by(tid, group) %>%
    dplyr::summarise(
      start = dplyr::first(pos),
      stop = dplyr::last(pos),
      dup  = any(dup),
      dplyr::across(dplyr::matches(quos_reg_), ~ length(unique(.)) == 1),
      doc_id = doc_[["doc_id"]][1],
      .groups = "drop"
    ) %>%
    dplyr::filter(dplyr::if_all(dplyr::matches(quos_reg_))) %>%
    dplyr::select(doc_id, tid, start, stop, dup) %>%
    dplyr::arrange(tid, start)

  if (.cache_terms) {
    attr(tab_, "cache-terms") <- unique(c(attr(tab_, "cache-terms"), term_list_[["tid"]]))
  }

  attr(tab_, "separators") <- quos_vec_


  if (!is.null(.tab_pos)) {
    tab_ <- dplyr::bind_rows(.tab_pos, tab_) %>%
      dplyr::arrange(dplyr::desc(stop - start), tid) %>%
      dplyr::mutate(
        tmp = purrr::map2(start, stop, ~.x:.y),
        dup = relist(duplicated(unlist(tmp)), tmp),
        dup = purrr::map_lgl(dup, any)
      )  %>%
      dplyr::select(doc_id, tid, start, stop, dup) %>%
      dplyr::arrange(tid, start)

  }

  return(tab_)

}


.tab_pos <- position_count(.termlist, .document, sen_id, .cache_terms = TRUE, .tab_pos = NULL)
.tab_token <- .document
.n <- 2
.context = "sentence"

get_context <- function(.tab_pos, .tab_token, .n, .context = c("word", "sentence", "paragraph", "page")) {
  context_ <- match.arg(.context, c("word", "sentence", "paragraph", "page"))

  quo_ <- switch (context_,
    "sentence" = "sen_id",
    "paragraph" = "par_id",
    "page" = "pag_id"
  )


  a <- .tab_pos %>%
    dplyr::left_join(dplyr::select(.tab_token, tok_id, pre = !!dplyr::sym(quo_)), by = c("start" = "tok_id")) %>%
    dplyr::left_join(dplyr::select(.tab_token, tok_id, post = !!dplyr::sym(quo_)), by = c("start" = "tok_id")) %>%
    dplyr::mutate(
      point = purrr::map2(pre, post, ~sort(.x:.y)),
      pre = purrr::map(pre, ~ sort((.x - 1):(.x - .n))),
      post = purrr::map(post, ~ sort((.x + 1):(.x + .n))),
      hit = dplyr::row_number()
    ) %>%
    dplyr::select(tid, hit, pre, point, post, start, stop) %>%
    tidyr::pivot_longer(c(pre, point, post), values_to = quo_) %>%
    tidyr::unnest(!!dplyr::sym(quo_)) %>%
    dplyr::left_join(dplyr::select(.tab_token, !!dplyr::sym(quo_), tok_id, token), by = quo_) %>%
    dplyr::filter(!is.na(tok_id)) %>%
    dplyr::mutate(
      name = dplyr::if_else(tok_id >= start & tok_id <= stop, "term", name),
      name = dplyr::if_else(name == "point", NA_character_, name)
      ) %>%
    tidyr::fill(name, .direction = "down") %>%
    dplyr::mutate(
      name = dplyr::if_else(!(tok_id >= start & tok_id <= stop) & name == "term", NA_character_, name)
    ) %>%
    tidyr::fill(name, .direction = "up") %>%
    dplyr::group_by(tid, hit, name, start, stop, !!dplyr::sym(quo_)) %>%
    dplyr::summarise(token = paste(token, collapse = " "), .groups = "drop_last") %>%
    dplyr::summarise(token = paste(token, collapse = " <> "), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = name, values_from = token) %>%
    dplyr::select(tid, start, stop, pre, term, post)


}



tab_term <- dplyr::bind_rows(
  test_termlist,
  dplyr::mutate(test_termlist, term = paste(term, "A")),
  dplyr::mutate(test_termlist, term = paste(term, "B")),
  dplyr::mutate(test_termlist, term = paste(term, "C")),
  dplyr::mutate(test_termlist, term = paste(term, "D")),
  dplyr::mutate(test_termlist, term = paste(term, "E")),
  dplyr::mutate(test_termlist, term = paste(term, "F"))
) %>% dplyr::mutate(tid = dplyr::row_number())

.termlist <- prep_termlist2(tab_term, string_standardization)
.document <- prep_document(test_document, string_standardization)
quos_ <- dplyr::quos(sen_id, pag_id)

tab1 <- position_count(.termlist, .document, sen_id)
tab2 <- position_count2(.termlist, .document, sen_id)


tab_bench <- bench::mark(
  old = position_count(.termlist, .document, sen_id),
  new = position_count2(.termlist, .document, sen_id)
) %>% dplyr::select(expression:total_time)


all.equal(tab1, tab2)


f_search <- function(.tab_terms, .vec_terms, .corpus) {
  tab_pos0 <- purrr::map(
    .x = .vec_terms,
    .f = ~ which(.x == .corpus)
  ) %>% tibble::enframe(name = "token", value = "pos")

  .tab_terms %>%
    tidyr::unnest(token) %>%
    dplyr::left_join(tab_pos0, by = "token") %>%
    dplyr::group_by(tid) %>%
    dplyr::mutate(oid = dplyr::row_number()) %>%
    tidyr::unnest(pos) %>%
    dplyr::arrange(pos, oid, .by_group = TRUE) %>%
    dplyr::mutate(group = cumsum(oid == 1)) %>%
    dplyr::group_by(tid, group) %>%
    dplyr::mutate(d_oid = c(1, diff(oid))) %>%
    dplyr::filter(d_oid == 1) %>%
    dplyr::mutate(d_pos = c(1, diff(pos))) %>%
    dplyr::filter(d_pos == 1) %>%
    dplyr::filter(dplyr::n() == ngram) %>%
    dplyr::filter(oid == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(tid, pos)
}


tab_terms <- list(
  c("A", "B", "C", "D"),
  c("A", "B", "C"),
  c("B", "C", "D"),
  c("B", "C")
) %>% tibble::enframe(name = "tid", value = "token") %>%
  dplyr::mutate(ngram = lengths(token))
vec_terms <- unique(unlist(lst_terms$token))
vec_terms <- setNames(vec_terms, vec_terms)

set.seed(123)
corpus <- c(
  "A", "B", "C", "B", "A", "B", "C", "D", "D", "B", "D", "A", "E", "B", "C", "D"
  )




ben <- bench::mark(
  f_search(tab_terms, vec_terms, corpus)
)



#
# %>%
#   dplyr::filter()
#
# %>%
#   dplyr::group_by(tid, aa) %>%
#   dplyr::summarise(dplyr::across(dplyr::everything(), list))
#
#
#   dplyr::mutate(
#     d_pos = c(NA_integer_, diff(pos)),
#     d_oid = c(NA_integer_, diff(oid)),
#     d_oid = dplyr::if_else(oid == 1L, 1L, d_oid)
#   ) %>%
#   tidyr::fill(c(d_pos, d_oid), .direction = "up") %>%
#   dplyr::mutate(
#     r_pos = RcppRoll::roll_suml(d_pos, n = 4)
#   )
#
#
#
#
#
#
#
#
#
#
#
# dplyr::group_by(tid) %>%
#   dplyr::summarise(dplyr::across(c(token, pos), list), .groups = "drop") %>%
#   dplyr::mutate(
#     pos = purrr::map(pos, ~ sort(unlist(.x)))
#   )
#
#
# vec_pos <- unlist(lst_pos)
#
#
#
#
#
#
# fil_doc <- RFgen::lft2("scripts/data/txt/", "txt")
# tab_txt <- tibble::as_tibble(readtext::readtext(fil_doc$path))
# tab_term <- openxlsx::read.xlsx("scripts/data/term_list.xlsx")
# term_list <- prep_termlist(tab_term, string_standardization)
#
# tmp1_ <- term_list %>%
#   tidyr::unnest(token) %>%
#   dplyr::group_by(tid) %>%
#   dplyr::mutate(oid = dplyr::row_number()) %>%
#   dplyr::ungroup()
#
# tmp2_ <- tibble::tibble(
#   tid = unique(tmp1_$tid),
#   token = rep("______", length(unique(tmp1_$tid)))
# ) %>% dplyr::mutate(token_id = 1000L)
#
# tmp3_ <- dplyr::bind_rows(tmp1_, tmp2_) %>%
#   dplyr::arrange(tid, token_id)
#
#
# tmp4_ <- term_list %>%
#   dplyr::mutate(
#     dep = purrr::map(token, ~ find_seq_in_seq(.x, tmp3_$token))
#   ) %>%
#   tidyr::unnest(dep) %>%
#   dplyr::mutate(
#     tid_dep = tmp3_$tid[dep],
#     oid_dep = tmp3_$oid[dep]
#     ) %>%
#   dplyr::mutate(
#     tid_dep = dplyr::if_else(tid_dep == tid, NA_real_, tid_dep),
#     oid_dep = dplyr::if_else(tid_dep == tid, NA_integer_, oid_dep)
#     ) %>%
#   dplyr::group_by(tid, term, token, hash) %>%
#   dplyr::summarise(
#     dplyr::across(c(tid_dep, oid_dep), ~ list(na.omit(.x))),
#     .groups = "drop"
#     )
#
# tab_tok <- prep_document(tab_txt) %>%
#   dplyr::rename(token_raw = token) %>%
#   dplyr::mutate(token = string_standardization(token_raw))
#
# token <- unique(unlist(term_list$token))
# token <- setNames(token, token)
#
# lst_pos <- purrr::map(
#   .x = token,
#   .f = ~ which(.x == tab_tok$token)
# ) %>% tibble::enframe(name = "token", value = "pos") %>%
#   tidyr::unnest(pos)
#
# tab_pos <- term_list %>%
#   dplyr::select(tid, token) %>%
#   tidyr::unnest(token) %>%
#   dplyr::left_join(lst_pos, by = "token") %>%
#   dplyr::arrange(tid, pos) %>%
#   dplyr::mutate(seq = c(NA, head(pos, -1)) + 1 == pos)
# tab_pos$seq[1] <- tab_pos$seq[2]
# run_lengths <- rle(tab_pos$seq)
# tab_pos$a <- rep.int(run_lengths$lengths, run_lengths$lengths)
#
# rep.int(c(1,2), c(3,3))
# i_ends <- cumsum(run_lengths$lengths)[run_lengths$values]
# i_starts <- c(1, head(i_ends, -1))
#
#
#
#
#
#
#
# %>%
#   dplyr::mutate(id = cumsum(c(1, abs(diff(pos)) == 1)))
#
#
# dplyr::group_by(tid) %>%
#   dplyr::summarise(
#     token = list(token),
#     pos = list(pos),
#     .groups = "drop"
#   ) %>%
#   dplyr::mutate(pos = purrr::map(pos, ~sort(unlist(.x))))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# %>%
#   dplyr::group_by(tid) %>%
#   dplyr::mutate(
#     id_ = dplyr::row_number(),
#     tmp_ = paste0(id_, "-", token)
#     ) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(aa = dplyr::row_number())
#
#
# tmp2_ <- tmp1_ %>%
#   dplyr::group_by(tid) %>%
#   dplyr::summarise(tmp_ = list(tmp_), .groups = "drop") %>%
#   dplyr::mutate(
#     aa = purrr::map(tmp_, ~ find_seq_in_seq(.x, tmp1_$tmp_))
#     ) %>%
#   tidyr::unnest(aa) %>%
#   dplyr::left_join(dplyr::select(tmp1_, aa, tid), by = "aa", suffix = c("1", "2")) %>%
#   dplyr::select(tid1, tid2, tmp_) %>%
#   dplyr::filter(!tid1 == tid2) %>%
#   dplyr::select(tid = tid1, dep_start = tid2) %>%
#   dplyr::group_by(tid) %>%
#   dplyr::summarise(dep_start = list(dep_start), .groups = "drop")
#
# term_list <- term_list %>%
#   dplyr::left_join(tmp2_) %>%
#   dplyr::arrange(dplyr::desc(lengths(dep_start)))
#
#
#
#
#
# find_seq_in_seq(term_list$token[[1]], tab_tok$token)
#
