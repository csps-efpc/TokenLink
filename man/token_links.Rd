% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenify.R
\name{token_links}
\alias{token_links}
\title{Returns the required information about the joint probability of the tokens in one object}
\usage{
token_links(
  dat_x,
  dat_y,
  col_nms_x = colnames(dplyr::select_if(dat_x, is.character)),
  col_nms_y = colnames(dplyr::select_if(dat_y, is.character)),
  args_x = list(),
  args_y = list(),
  row_name_nm = TOKENIZE_DEFAULT_ROW_NAME,
  suffix = TOKEN_SUFFIX_DEFAULT,
  token_count_join = TOKEN_TOKEN_TYPE_VEC,
  m_prob_func = calc_m_prob,
  ...
)
}
\arguments{
\item{dat_x}{dataframe}

\item{dat_y}{dataframe}

\item{col_nms_x}{vector of string column names to tokenize. Default all character columns}

\item{col_nms_y}{vector of string column names to tokenize. Default all character columns}

\item{args_x}{list of arguments passed to 'tokenize_ations'}

\item{args_y}{list of arguments passed to 'tokenize_ations'}

\item{row_name_nm}{string that is the name of the column to get the rownames. Default TOKENIZE_DEFAULT_ROW_NAME}

\item{suffix}{vector of length two with suffixs to add to column names as needed}

\item{token_count_join}{vector of column names to join the token count dataframes on. Default TOKEN_TOKEN_TYPE_VEC}

\item{m_prob_func}{a function that takes a dataframe with counts of tokens then returns a vector of m_probs}

\item{...}{arguments passed to 'tokenize_ations', note ... is  the lowest priority and all other passed first}
}
\description{
Returns the required information about the joint probability of the tokens in one object
}
\examples{
dat_x = readr::read_csv('https://tinyurl.com/2p8etjr6')
dat_y = readr::read_csv('https://tinyurl.com/2p8ap4ad' )
t_dat <- token_links(
     dat_x = dat_x,
     dat_y = dat_y,
     args_x = list(col_nms = 'coname'),
     args_y = list(col_nms = 'companyName'),
     token_types = 'company_name',
     token_index = '',
     suffix = c('ceo', 'alb')
)



}
