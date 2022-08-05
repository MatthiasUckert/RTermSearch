check_termlist(table_termlist_long, string_standardization)
tl <- dplyr::mutate(table_termlist_long, tmp = tid  + 1)
.termlist    <- prep_termlist(tl, string_standardization, TRUE, tid, tmp)
duplicate_doc <- function(.tab, .n) {
  tab_ <- dplyr::mutate(.tab, doc_id = "doc-0")
  for (i in seq_len(.n)) {
    tab_ <- dplyr::bind_rows(
      tab_,
      dplyr::mutate(.tab, doc_id = paste0("doc-", i))
    )
  }
  return(tab_)
}

.document    <- prep_document(
  .tab = duplicate_doc(.tab = table_document_long, .n = 10),
  .fun_std = string_standardization
  )
.cache_terms <- TRUE
.tab_pos     <- NULL
quos_        <- dplyr::quos(sen_id)
