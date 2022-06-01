


library(tidyverse)
library(glue)
library(stringr)
source('R/tokenify.R')

data_dir = 'data'
generate_token_counts_CACHE <- NULL


#'Generates counts of tokens for a given token type, returns a datafrgenerate_random_column(ame with two columns token and n
#'
#' @param a_token_type as string/ Defaults to company_name
#'
#'
#'
generate_token_counts <- function(a_token_type = 'company_name'){
  if (a_token_type %in% c('company_name', 'address')){
    if (is.null(generate_token_counts_CACHE)){
      fn <- "data/companies_sorted.rds"
      generate_token_counts_CACHE <<- readr::read_rds(fn)
    }

    ret_val <-
      generate_token_counts_CACHE$token_counts |>
      filter(token_type == a_token_type) |>
      select(token, n)

    return(ret_val)
  }
  if (a_token_type %in% c('first_name')){
    ret_val <-
      file.path( data_dir , 'most-popular-baby-names-2005-current.csv') |>
      read_csv() |>
      group_by(Name) |>
      summarise(n = sum(COUNT)) |>
      rename(token := Name)

    return(ret_val)
  }
  if (a_token_type %in% c('last_name')){
    ret_val <-
      file.path( data_dir , 'us_last_names.txt') |>
      read_csv(col_names = FALSE) |>
      setNames('token') |>
      distinct(token) |>
      mutate(n = 1)
    return(ret_val)
  }
  if (a_token_type == 'age'){
    ret_val <-
      tibble(token = as.character(rep(1:85)),
             n = 1
             )
    return(ret_val)
  }

}




#'
#'Generates a single column dataframe
#'
#' @param n Number of rows returned in dataframe.
#' @param k_min Minimum number of words.
#' @param k_max Maximum number of words.
#' @param k a vector positive integers of length N
#' @param token_type string to indicate the type of token
#' @param token_counts counts tokens of that type
#'
generate_random_column <- function(n = 1000,
                                   k_min = 1,
                                   k_max = 4,
                                    k = round(runif(min = k_min, max = k_max, n = n), 0),
                                    token_type = 'company_name',
                                   token_counts = generate_token_counts(a_token_type = token_type)){
  if (length(k) == 1){
    k <- rep(k, n)
  }
  assertthat::are_equal(length(k), n)


  kn <- k |> sum()
  token_counts_samples <-
    token_counts |>
    dplyr::sample_n(kn, weight = n, replace = TRUE)

  i = 1
  purrr::map_dfr(k, ~{
    j <- i+.x-1
    ret <- token_counts_samples |>
      slice(i:j) |>
      dplyr::arrange(n) |>
      dplyr::pull(token) |>
      paste0(collapse = ' ') |>
      stringr::str_to_title()
    i <<- j +1
    tibble(!!sym(token_type) :=ret)
  })
}







#'
#' Get a single character out of a string
#'
#'
#' @param i  an integer
#' @param str  a string
#' @param ...  passed to substr
#'
#' @examples
#' \dontrun{
#' s2c(i = 1, "data Doo Doo")
#' }
s2c <- function(i, str, ...){
  substr(x = str, start = i,stop =  i, ...)
}


#'
#' Change a string to a vector
#'
#' @param str a string
#' @param ... passed to s2c in purrr::map
#'
map_s2c<- function(str, ... ){
  purrr::map(1:nchar(str), s2c, str = str, ...) |> unlist()
}

#'
#' Randomly edit some string, by adding deleteing or swapping characters randomly
#'
#' @param str A string
#' @param ... ignored
#' @param prob_char probability that a character gets edited
#' @param prob_del probability that a character gets deleted
#' @param prob_insert probability that a character gets insert
#' @param sub_in vector of characters to use for replacing
random_edit_char <- function(str,
                             ...,
                             prob_char = 0.02,
                             prob_del = 0.02,
                             prob_insert = 0.02,
                             sub_in = letters){
  str <- map_s2c(str)
  str <-
  if_else(
    sample(x = c(T, F), size = length(str), prob = c(prob_char,1-prob_char), replace = TRUE),
    sample(x = sub_in, size =  length(str), replace = TRUE),
    str)



  str <-
    if_else(sample(x = c(T, F), size = length(str), prob = c(prob_del,1-prob_del), replace = TRUE),
            '',
            str )


  str <-
    if_else(sample(x = c(T, F), size = length(str), prob = c(prob_insert,1-prob_insert), replace = TRUE),
            paste0(str, sample(x = sub_in, size =  length(str), replace = TRUE)),
            str ) |>
    paste0(collapse = "")

  return(str)
}

#'
#' Edit the words of the string maybe., by randomly inserting or deleting a word from the string
#'
#' @param str  A string
#' @param ... ignored
#' @param prob_del probability that a word gets edited
#' @param prob_insert  probability that a word gets inserted
random_edit_word <- function(str,
                             ...,
                             prob_del = 0.05,
                             prob_insert = 0.05){
  str <-
    str |>
    str_split(pattern = '\\s+', n = Inf, simplify = TRUE)

  str <-
    if_else(sample(x = c(T, F), size = length(str), prob = c(prob_del,1-prob_del), replace = TRUE),
          '',
          str
     )
  str <-
    if_else(sample(x = c(T, F), size = length(str), prob = c(prob_insert,1-prob_insert), replace = TRUE),
          paste(str, words::words |> sample_n(1) |> pull(word), sep = ' '),
          str,
          ) |>
    paste0(collapse = " ")

  return(str)
}



#'
#' Edit a number maybe? by moving it some small amount from it origional value.
#'
#' @param x a number, or convertible to a number
#' @param prob_small_move Fraction probability tha number will change. Default as 0.3
#' @param num_type Converts to a number type default to as.integer
#' @param frac_move what percentage to move if it is moved
#' @param ... ignored
random_edit_number <- function(x,
                               prob_small_move = 0.03,
                               num_type = as.integer,
                               frac_move = 0.1,
                               ...){
  x<- x |> num_type()

  x <-
    if_else(sample(x = c(T, F), size = length(x), prob = c(prob_small_move,1-prob_small_move), replace = TRUE),
            num_type(x+(runif(length(x)) - frac_move)*x),
            x
            )
  return(x)
}

#'
#' edit the characters and words of a string
#'
#'
#' @param str A string
#' @param ... passed to random_edit_char, and random_edit_word
random_edit <- function(str, ...){
  random_edit_char(random_edit_word(str = str, ...), ...)
}


#'
#' generate_random dataframe
#'
#' @param n number of rows to return
#'
generate_random_dat <- function(n = 1500
                                ){
  bind_cols(
    generate_random_column(n=n, token_type = 'company_name',k_min = 1, k_max = 5),
    generate_random_column(n=n, token_type = 'address', k_min = 1, k_max = 6),
    generate_random_column(n=n, token_type = 'first_name', k =1),
    generate_random_column(n=n, token_type = 'last_name', k =1),
    generate_random_column(n=n, token_type = 'age', k =1)
  )
}


#' Generate and save data and a pair that is edited randomly
#'
#' @param n number of rows to generate
#' @param fn file name to save the data to
#'
generate_random_dat_pairs <- function(n = 1500,
                                      fn = file.path(data_dir, 'generated_dataset.csv')){

  dat <- generate_random_dat(n)
  #dat <- orig_dat
  dat2 <-
    dat |>
    #sample_n(100) |>
    mutate(first_name = purrr::map(first_name, random_edit) |> unlist())  |>
    mutate(last_name    = purrr::map(last_name , random_edit) |> unlist())  |>
    mutate(address = purrr::map(address, random_edit_word, prob_del = 0.2,prob_insert = 0.2) |> unlist())  |>
    mutate(company_name = purrr::map(company_name, random_edit_word, prob_del = 0.2,prob_insert = 0.2) |> unlist())  |>
    mutate(age = random_edit_number(age))

  fn_edit <- fn |> stringr::str_replace('\\.csv$', '_random_edits.csv')
  dat |> write_csv(fn)
  dat2 |> write_csv(fn_edit)
#
  bind_cols(dat |> rename_all(function(x){paste0(x, '_x')}) ,
            dat2 |> rename_all(function(x){paste0(x, '_y')})
            ) |>
    filter(company_name_x != company_name_y) |>
    select(company_name_x,company_name_y)

}




