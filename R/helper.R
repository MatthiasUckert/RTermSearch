#' Find a sequence in another sequence
#'
#' @param .seq_find The sequence you want to find in another sequence
#' @param .seq_base The sequence to be searched
#'
#' @return An integer vector (vector has length zero if sequence is not found)
#' @export
#'
#' @examples
#' # Find an integer sequence
#' find_seq_in_seq(2:10, c(1:20, 1:20))
#'
#' # Find a character sequence
#' find_seq_in_seq(c("C", "D"), LETTERS)
#'
#' # No sequence found
#' find_seq_in_seq(c("R", "D"), LETTERS)
#'
find_seq_in_seq <- function(.seq_find, .seq_base) {
  w <- seq_along(.seq_base)
  for (i in seq_along(.seq_find)) {
    w <- w[.seq_base[w + i - 1L] == .seq_find[i]]
    if (length(w) == 0) return(integer(0))
  }
  w <- w[!is.na(w)]
  return(w)
}
