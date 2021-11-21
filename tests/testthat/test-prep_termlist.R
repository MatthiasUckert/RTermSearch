test_that("warning on duplicates tid", {
  tab_ <- tibble::add_row(test_termlist, tid = 1, term = "A")
  expect_warning(prep_termlist(tab_))
})


test_that("warning on duplicated term", {
  tab_ <- tibble::add_row(test_termlist, tid = 10, term = "Linguistics")
  expect_warning(prep_termlist(tab_))
})

test_that("error on missing columns", {
  tab_ <- dplyr::rename(test_termlist, id = tid, word = term)
  expect_error(prep_termlist(tab_))
})
