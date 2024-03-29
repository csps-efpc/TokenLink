% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenify.R
\name{tokenize_ations}
\alias{tokenize_ations}
\title{Takes a dataframe and tokenizes the columns indicated and then counts the tokens, and returns a list of dataframes, as well as some other parameters}
\usage{
tokenize_ations(dat, ..., col_nms)
}
\arguments{
\item{dat}{a dataframe}

\item{...}{passed to tokenize_df, and all arguments are added to the return list}

\item{col_nms}{vector of column names to be tokenized}
}
\description{
Takes a dataframe and tokenizes the columns indicated and then counts the tokens, and returns a list of dataframes, as well as some other parameters
}
\examples{
dat_ceo <- readr::read_csv('https://tinyurl.com/2p8etjr6')
toke_ceo <- dat_ceo |> tokenize_ations(col_nms = 'coname', token_types = 'company_name')

}
