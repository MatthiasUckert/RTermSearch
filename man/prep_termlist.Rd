% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{prep_termlist}
\alias{prep_termlist}
\title{Prepare Termlist}
\usage{
prep_termlist(.tab, .fun_std = NULL, .get_dep = FALSE, ...)
}
\arguments{
\item{.tab}{A Dataframe with at least 1 column (term: Term)}

\item{.fun_std}{A
Function to standardize Strings.\cr
Default = NULL (no standardization use)\cr
Build-in Function .fun_std = string_standardization, can be used.}

\item{.get_dep}{Get Term List Dependencies (default = FALSE).\cr
Term List Dependencies show how n-grams in the term list are related.}

\item{...}{Any other column of the original Dataframe (.tab) you want to keep in the output}
}
\value{
A Dataframe with minimum 6 columns:\cr
hash: Hashed value (xxhash32) of standardized term\cr
ngram: N-Gram of standardized term\cr
term_orig: Original term (as defined in parameter .tab)\cr
term: Standardized term (with .fun_std)\cr
oid: Order ID of token in column token\cr
token: Tokienized version of column term\cr
dep (optional): Dependencies of terms within the term list\cr
...: Any other column specified by ...
}
\description{
Prepare Termlist
}
\examples{

prep_termlist(table_termlist_short, NULL)
prep_termlist(table_termlist_short, string_standardization)
prep_termlist(table_termlist_short, string_standardization, TRUE)
prep_termlist(table_termlist_short, string_standardization, TRUE, tid)
}
