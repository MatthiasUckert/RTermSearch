#' Wikipedia Article about Natural Language Processing (NLP)
#'
#' A dataset containing an example text from wikipedia
#' \href{https://en.wikipedia.org/wiki/Natural_language_processing}{Wikipedia}
#'
#' @format A data frame one row and two columns
#' \describe{
#'   \item{doc_id}{A unique identifier of the document}
#'   \item{text}{Text of the document}
#' }
"table_document"

#' Example Term List (Short)
#'
#' A dataset containing example terms to be searched within a document
#'
#' @format A data frame 7 rows and 1 column
#' \describe{
#'   \item{term}{A Term}
#' }
"table_termlist_short"

#' Example Term List (Long)
#'
#' A dataset containing example terms to be searched within a document
#'
#' @format A data frame 12 rows and 1 column
#' \describe{
#'   \item{term}{A Term}
#' }
"table_termlist_long"
