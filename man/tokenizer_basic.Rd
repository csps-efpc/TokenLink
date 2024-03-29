% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenify.R
\name{tokenizer_basic}
\alias{tokenizer_basic}
\title{tokenizes a column in a dataframe}
\usage{
tokenizer_basic(
  dat,
  ...,
  col_nm,
  row_name_nm,
  token_type = col_nm,
  token_col_nm = "token",
  drop_col = TRUE,
  token_index = "",
  pre_token_clean_str = clean_str,
  post_token_clean_Str = clean_str_2
)
}
\arguments{
\item{dat}{dataframe. No default.}

\item{...}{passed to both clean_str and  tidytext::unnest_tokens}

\item{col_nm}{string, name of column to tokenize}

\item{row_name_nm}{string, name of column to put row_name into}

\item{token_type}{string of the type of token for the given column. Default is col_nm}

\item{token_col_nm}{String, column name of new tokens.}

\item{drop_col}{Boolean. If True drops the original column, default = TRUE}

\item{token_index}{String. name of column  that will have index of order of tokens in origional column, Default ""}

\item{pre_token_clean_str}{function. that takes vector of strings and ... cleans the string. will clean the string before tokenization. Default clean_str.}

\item{post_token_clean_Str}{function. that takes vector of strings and ... cleans the string. will clean the string before tokenization. Default clean_str_2.}
}
\description{
tokenizes a column in a dataframe
}
\examples{
dat_ceo <- readr::read_csv('https://tinyurl.com/2p8etjr6')
dat_ceo |> tokenizer_basic(col_nm = 'exec_fullname', row_name_nm = 'rn', drop_col = FALSE) |> dplyr::select_at(c('token', 'exec_fullname'))


}
