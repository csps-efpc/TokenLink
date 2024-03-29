% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/token_helpers.R
\name{token_most_common}
\alias{token_most_common}
\title{Returns a dataframe of common ngrams}
\usage{
token_most_common(
  .v,
  n_range = DEFAULT_RANGE_NGRAM_RANGE,
  token = "ngrams",
  n_ngrams_returns = DEFAULT_NUM_NGRAMS_RETURN,
  tokenizer = tokenizer_basic,
  ...
)
}
\arguments{
\item{.v}{a vector of strings}

\item{n_range}{range of ngrames to feed back : Default 1:4}

\item{token}{passed to tokenizer_basic which passes it to tidytext::unnest_tokens :Default 'ngrams'}

\item{n_ngrams_returns}{how many of each n to return for most common :Default 12}

\item{tokenizer}{a function that tokenizes a column of a data frame :Default tokenizer_basic}

\item{...}{passed to tokenizer_basic}
}
\value{
a dataframe showing the most common tokens
}
\description{
Returns a dataframe of common ngrams
}
\examples{
  mtcars |> tibble::rownames_to_column() |> dplyr::pull(rowname) |> token_most_common()
  library(tokenizers)
  mobydick |> stringr::str_split('\\\\.')   |> magrittr::extract2(1) |> token_most_common()

}
