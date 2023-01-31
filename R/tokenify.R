
#' @import data.table
#' @import reclin
#' @import dplyr
#' @import readr
#' @import glue
#' @import janitor
#' @import assertthat
#' @import stringr
#' @import purrr
#' @import tidytext
#' @import rlang
#' @import tibble
#' @import testit




REPLACEMENT_TOKEN_DATA_DIR = 'helper'
REPLACEMENT_TOKEN_FN_GLUE = 'token_replace_{token_type}.csv'
TOKENIZE_DEFAULT_ROW_NAME = 'row_name'
TOKEN_TOKEN_TYPE_VEC = c('token', 'token_type')
M_PROB_MIN = 0.99
M_PROB_MAX = 0.999
m_PROB_LOG_BASE = 10
TOKEN_SUFFIX_DEFAULT = c('x','y')
TOKEN_MIN_UPROB_DEFAULT = 0.0000784
TOKEN_MAX_COMPARE_DEFAULT = 100000000
TOKEN_REMOVE_ZERO_COMARE = TRUE
MIN_POSTERIOR_ALL_EVIDENCE = 0.01
MIN_POSTERIOR_POSITIVE_EVIDENCE_ONLY = 0.3
PRIORI_DELTA = 0.01



#' get the name of the token replacement file
#'
#' @param token_type
#' @param data_dir
#' @param file_name_pattern
#'
#' @return
#' @export
#'
#' @examples
read_replacements_token_type_get_fn <- function(token_type,
                                                data_dir = REPLACEMENT_TOKEN_DATA_DIR,
                                                file_name_pattern = REPLACEMENT_TOKEN_FN_GLUE){

  file.path(data_dir, glue::glue(file_name_pattern))
}





#' Reads in a replacement token file
#'
#'@param token_type String. Type of token you are looking for, if NULL or replacement file not found returns NULL
#'@param data_dir String. the directory where we look for the token replacement file
#'
#'@examples
#' read_replacements_token_type('company_name')
#' read_replacements_token_type('poop')
#'
#'@export
read_replacements_token_type <- function(token_type = NULL,
                                         ...
                                         ){
  if (is.null(token_type))
    return(NULL)

  #fn <- file.path(data_dir, glue::glue('token_replace_{token_type}.csv'))
  fn <- read_replacements_token_type_get_fn(token_type, ...)

  if (!file.exists(fn)){
    warning(glue::glue('File not found in read_replacements_token_type, for "{token_type}". Returning NULL. No token replacements will be done. fn = "{fn}" '))
    return(NULL)
  }

  readr::read_csv(file =fn, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate_if(is.character, function(x){trimws(tolower(x))})
}







#' will apply func to x if bool is TRUE. Saves us from an ugly function
#'
#'
#' @param x anything really , just the first argument of func
#' @param bool Boolean. applies function if bool is True
#' @param func function that takes x and ...
#' @param ... passed to func
#'
#'@examples
#'  maybe_do(c(2, 2,100), TRUE, function(x){x*2})
#'  maybe_do(c(2, 2,100), FALSE, function(x){x*2})
#'
#'@export
maybe_do <- function(x, bool, func, ...){
  assertthat::assert_that(is.logical(bool))
  assertthat::assert_that(length(bool) == 1)


  if(bool)
    return(func(x, ...))

  return(x)
}


########################
#'
#'Replaces tokens, and cleans a string using regex stuff largely, and by doing search and replace.  This is the the default string cleaner used before tokenization
#'It can be overridden  in tokenizer_basic, tokenize_col, tokenize_df, etc by passing a new function as pre_token_clean_str.
#'
#'
#'@param x vector of strings
#'@param ... ignored, used to ensure pass by keyword
#'@param token_type used to to try and load a default token replacement. no default
#'@param rep dataframe with three columns indicating what to replace. default  read_replacements_token_type(token_type)
#'@param remove_accents bool. Default = TRUE.
#'@param remove_punctuation bool. Default = TRUE.
#'@param iconv_to passed to iconv as the to parameter if remove_accents is TRUE. Default = 'ASCII//TRANSLIT'
#'@param punc_remove_patern string regex that finds punctuation to remove if remove_punctuation is TRUE. Default "[^[:alnum:][:cntrl:][:space:]_]"
#'@param punc_replace string replaces all punctuation if remove_punctuation is TRUE. default " ",
#'@param new_token_wrapper string. Placed on both sides of the new token. Default = " ".
#'
#'@examples
#'c('Z.Y. do things inc', 'z. y. DO things montrèal', 'at&t') |> clean_str(token_type = 'company_name')
#'
#'@export
clean_str <- function(x,
                      ...,
                      token_type,
                      rep = read_replacements_token_type(token_type),
                      remove_accents = TRUE,
                      remove_punctuation = TRUE,
                      iconv_to = 'ASCII//TRANSLIT',
                      punc_remove_patern = '[^[:alnum:][:cntrl:][:space:]_]',
                      punc_replace = ' ',
                      new_token_wrapper = ' '){
  if (is.null(rep)){
    return(x)
  }

  x <-
    x |>
    maybe_do(remove_accents, iconv, to = iconv_to) |>
    #maybe_do(remove_dots, stringr::str_replace_all, pattern = '\\b(?:[a-zA-Z]\\.){2,}', replacement = '__ ')
    #maybe_do(remove_dots, stringr::str_replace_all, pattern = '\\s[A-Za-z]\\.\\s?', replacement = '__ ')
    tolower()


  rep |>
    purrr::pwalk(function(token, replacement, word_wrap, ...){

        if (word_wrap){
          x <<- stringr::str_replace_all(string = x,
                                   pattern = paste0('\\b',token,'\\b'),
                                   replacement = paste0(new_token_wrapper,replacement,new_token_wrapper))
        }else{
          x <<- stringr::str_replace_all(string = x,
                                   pattern = token,
                                   replacement = paste0(new_token_wrapper,replacement,new_token_wrapper))
        }
    })


  x |>
    #maybe_do(remove_dots, stringr::str_replace_all, pattern = '\\.\\s?', replacement = ' ') |>
    maybe_do(remove_punctuation, stringr::str_replace_all, pattern = punc_remove_patern, replacement = punc_replace) |>
    stringr::str_replace_all('\\s+', ' ')
}






########################
#'
#'Cleans a string using after it has been tokenized as a like final step. This is the the default string cleaner used after tokenization
#'It can be overriden in tokenizer_basic, tokenize_col, tokenize_df, etc by passing a new function as post_token_clean_str.
#'
#'
#'@param x vector of strings
#'@param ... ignored, used to ensure pass by keyword
#'
#'@examples
#'c('Z.Y. do things inc', 'z. y. DO things montrèal', 'at&t') |> clean_str_2(token_type = 'company_name')
#'
#'@export
clean_str_2 <-function(x, ...){
  x |>
    stringr::str_trim() |>
    stringr::str_to_lower()
}


#'
#' tokenizes a column in a dataframe
#'
#' @param dat dataframe. No default.
#' @param ... passed to both clean_str and  tidytext::unnest_tokens
#' @param col_nm string, name of column to tokenize
#' @param row_name_nm string, name of column to put row_name into
#' @param token_type string of the type of token for the given column. Default is col_nm
#' @param token_col_nm String, column name of new tokens.
#' @param drop_col Boolean. If True drops the original column, default = TRUE
#' @param token_index String. name of column  that will have index of order of tokens in origional column, Default ""
#' @param pre_token_clean_str function. that takes vector of strings and ... cleans the string. will clean the string before tokenization. Default clean_str.
#' @param post_token_clean_Str function. that takes vector of strings and ... cleans the string. will clean the string before tokenization. Default clean_str_2.
#'
#'@examples
#'dat_ceo <- readr::read_csv('https://tinyurl.com/2p8etjr6')
#'dat_ceo |> tokenizer_basic(col_nm = 'exec_fullname', row_name_nm = 'rn', drop_col = FALSE) |> dplyr::select_at(c('token', 'exec_fullname'))
#'
#'
#'@export
tokenizer_basic <- function(dat,
                            ...,
                            col_nm,
                            row_name_nm,
                            token_type = col_nm,
                            token_col_nm = 'token',
                            drop_col = TRUE,
                            token_index = '',
                            pre_token_clean_str = clean_str,
                            post_token_clean_Str = clean_str_2
                            ){
  dat |>
    dplyr::mutate(!!dplyr::sym(col_nm) := pre_token_clean_str(!!dplyr::sym(col_nm), token_type = token_type,  ...)) |>
    tidytext::unnest_tokens(output = !!rlang::sym(token_col_nm), input = col_nm, ...) |>
    {\(.) if (drop_col) {dplyr::select(., -col_nm)} else {.}}() |>
    {\(.) if (nchar(token_index) > 0) dplyr::group_by_at(., row_name_nm) |> dplyr::mutate(!!rlang::sym(token_index) := dplyr::row_number()) |> dplyr::ungroup() else .}() |>
    dplyr::mutate(!!dplyr::sym(token_col_nm) := post_token_clean_Str(!!dplyr::sym(token_col_nm), ...))

}



#' turns a column of strings into a tokenized dataframe this returned dataframe will have two or three columns
#'
#' @param dat dataframe
#' @param ... passed to tokenizer
#' @param col_nm column that will be tokenized.
#' @param row_name_nm name of a return column that has the rownames in the original dataframe default row_name
#' @param token_type name of column that has tokens in return dataframe. Default appends '_type' onto token_col_nm
#' @param tokenizer function that tokenzes the column. Default  tidytext::unnest_tokens
#'
#' @examples
#' dat_ceo <- readr::read_csv('https://tinyurl.com/2p8etjr6')
#' tokenize_col(dat = dat_ceo, col_nm = 'coname')
#' tokenize_col(dat = dat_ceo, col_nm = 'coname', token_type = 'company_name')
#'
#' @export
tokenize_col <- function(dat,
                         ...,
                  col_nm,
                  row_name_nm = TOKENIZE_DEFAULT_ROW_NAME,
                  token_type = glue::glue('{col_nm}'),
                  tokenizer = tokenizer_basic
){
  testit::assert(col_nm %in% colnames(dat))

  #mtcars |> rowid_to_column()

  dat |> #rowid_to_column()
    #tibble::rownames_to_column(var = row_name_nm) |>
    tibble::rowid_to_column(var = row_name_nm) |>
    dplyr::select(dplyr::all_of(c(row_name_nm, col_nm))) |>
    tokenizer(col_nm = col_nm, row_name_nm = row_name_nm, token_type = token_type, ...) |>
    dplyr::distinct() |>
    dplyr::mutate(token_type = token_type )
}







#' Tokenize a dataframe and multiple columns in the dataframe
#'
#'@param dat dataframe
#'@param ... passed to tokenize_col
#'@param col_nms vector of string. These strings are column names in dat to tokenize. Default None
#'@param token_types vector of strings. these are the type of tokens for each token column
#'
#'
#'@examples
#' temp_fn <- tempfile()
#' download.file("https://www150.statcan.gc.ca/n1/pub/37-26-0001/2021001/ODEF_v2.zip",temp_fn)
#' dat_odef <- readr::read_csv(unz(temp_fn, "ODEF_v2/ODEF_v2.csv"))
#' dat_odef |> tokenize_df(col_nms = c('Facility_Name','Facility_Type', 'Authority_Name', 'Full_Addr'), token_types = c('company_name', 'company_name', 'company_name', 'Address'))
#'
#'@export
tokenize_df <- function(dat,
                        ...,
                        col_nms,
                        token_types = col_nms
                        ){
  testit::assert(length(col_nms) == length(token_types))
  testit::assert(typeof(col_nms) == typeof(token_types))
  testit::assert(is.character(col_nms))


  purrr::map2_dfr(col_nms, token_types, function(.x, .y){
    dat |> tokenize_col(col_nm = .x, token_type = .y, ...)
  })
}


#' Takes a dataframe with columns from cols and counts the unique occurrences, returns a data frame with counts.
#'
#'
#' @param dat_tokens data frame with columns in cols.
#' @param cols vector of column names. Default token, token_type
#' @param .groups passed to summarize. Default 'drop'
#' @param ... not used
#'
#' @examples
#'
#' temp_fn <- tempfile()
#' download.file("https://www150.statcan.gc.ca/n1/pub/37-26-0001/2021001/ODEF_v2.zip",temp_fn)
#' dat_odef <- readr::read_csv(unz(temp_fn, "ODEF_v2/ODEF_v2.csv"))
#' toke_odef <- dat_odef |> tokenize_df(col_nms = c('Facility_Name','Facility_Type', 'Authority_Name', 'Full_Addr'), token_types = c('company_name', 'company_name', 'company_name', 'Address'))
#' toke_odef |> token_count()
#'
#' @export
token_count <- function(dat_tokens,
                        cols = TOKEN_TOKEN_TYPE_VEC,
                        .groups = 'drop',
                        ... ){
  dat_tokens |>
    dplyr::group_by_at(cols) |>
    dplyr::summarise(n = dplyr::n(), .groups = .groups) |>
    dplyr::arrange(dplyr::desc(n))
}



#' Takes a dataframe and tokenizes the columns indicated and then counts the tokens, and returns a list of dataframes, as well as some other parameters
#'
#'
#'
#'@param dat a dataframe
#'@param ... passed to tokenize_df, and all arguments are added to the return list
#'@param col_nms vector of column names to be tokenized
#'
#'@examples
#'dat_ceo <- readr::read_csv('https://tinyurl.com/2p8etjr6')
#'toke_ceo <- dat_ceo |> tokenize_ations(col_nms = 'coname', token_types = 'company_name')
#'
#'@export
tokenize_ations <- function(dat,
                            ...,
                            col_nms
                            ){
  # dat = args_x$dat
  # col_nms =args_x$col_nms
  # row_name_nm = args_x$row_name_nm
  #dat_tokens <- tokenize_df(dat = args_x$dat, col_nms = args_x$col_nms, row_name_nm = args_x$row_name_nm )
  dat_tokens <-
    dat |>
      tokenize_df(col_nms = col_nms, ...)

  token_counts <- token_count(dat_tokens)

  ret_obj <- list(dat = dat,
                 tokens = dat_tokens,
                 token_counts = token_counts,
                 col_nms = col_nms)

  args_lst <- list(...)
  purrr::walk(names(args_lst), function(nm){
    if (!nm %in% names(ret_obj))
      ret_obj[[nm]] <<- args_lst[[nm]]
  })
  ret_obj
}





#' Calculates what m_prob should be, takes in a dataframe and returns a vector of same length
#' this dataframe must have a column of name n_comparisons and it will return a vector. which indicate the m_probs
#' This is the default calculator for m prob of tokens it can be overwritten by injecting a diferent function into the
#' m_prob_func parameter into any of tokenize_ations_m_u_prob, or token_links when called.
#' This function must take a dataframe and return a vector. The dataframe passed in will have columns token, token_type, n.x, n.y, n_comparisons, u_prob
#'
#'
#' @param dat_token_info a dataframe with information about the tokens
#' @param min_m_prob minimum value of m_prob returned
#' @param max_m_prob maximum value of m_prob returned
#' @param log_base Number. Base of the log. Default 10
#' @param ... is ignored
#'
#' @examples
#'
#' dplyr::tibble(n_comparisons = sample.int(100, 10)) |> calc_m_prob()
#'
#'
#'@export
calc_m_prob <- function(dat_token_info,
                        min_m_prob = M_PROB_MIN,
                        max_m_prob = M_PROB_MAX,
                        log_base = m_PROB_LOG_BASE,
                        ...){


  #
  x <-
    dat_token_info |>
    dplyr::select(n_comparisons) |>
    dplyr::mutate(n_comparisons_log = log(n_comparisons, base = log_base)) |>
    dplyr::mutate(n_comparisons_log = dplyr::if_else(n_comparisons_log == -Inf , 0, n_comparisons_log)) |>
    #dplyr::filter(is.na(n_comparisons_log))
    dplyr::pull(n_comparisons_log)

  x[is.na(x)]
  rng <- max(x)-min(x)

  if (rng <=0)
    return (mean(c(max_m_prob, min_m_prob)))

  (1+(min(x)-x)/rng) * (max_m_prob - min_m_prob) + min_m_prob
}





#' adds value to lst with the key nm if nm is not already in lst
#'
#' @param lst a list
#' @param nm string that is a key
#' @param val a value to add to the list
#'
#' @examples
#'list(a = 1, b = 2) |> maybe_add('c', 4) |> maybe_add('a', 99)
#'
#'
#' @export
maybe_add <- function(lst, nm, val){
  if (!nm %in% names(lst) )
    lst[[nm]] <- val

  return(lst)
}


#' Generates a dataframe with the total counts of each tokens across both datasets as well as the m and u probs
#'
#' @param x_counts Counts of tokens from first dataset
#' @param y_counts Counts of tokens from second dataset
#' @param total_comparisons count of the number of comparisons that can happens normally is nrow(x_dat) * nrow(y_dat)
#' @param token_count_join String vector that joins the two token count dataframes. Default c('token','token_type')
#' @param suffix String vector of length 2. Helps identify which column the counts came from. Default c('x','y')
#' @param m_prob_func Function that takes a dataframe with columns token, token_type, n.x, n.y, n_comparisons, u_prob, and returns a vector of m_probs
#' @param ... not used
#'
#' @examples
#'
#' dat_ceo <- readr::read_csv('https://tinyurl.com/2p8etjr6')
#' dat_alb <- readr::read_csv('https://tinyurl.com/2p8ap4ad')
#' t_dat <- token_links(
#'   dat_x = dat_ceo,
#'   dat_y = dat_alb,
#'   args_x = list(col_nms = 'coname'),
#'   args_y = list(col_nms = 'companyName'),
#'   token_types = 'company_name',
#'   token_index = '',
#'   suffix = c('ceo', 'alb')
#' )
#' results <- generate_all_tokens(t_dat$x$token_counts, t_dat$y$token_counts, t_dat$total_comparisons)
#'
#'
#' @export
generate_all_tokens <- function(x_counts,
                               y_counts,
                               total_comparisons,
                               token_count_join = TOKEN_TOKEN_TYPE_VEC,
                               suffix = TOKEN_SUFFIX_DEFAULT,
                               m_prob_func = calc_m_prob,
                               ...
                               ){

  n_nms <- paste0('n.', suffix)
  n_nms_x <- n_nms[[1]]
  n_nms_y <- n_nms[[2]]


  dplyr::full_join(x_counts,# = t_dat$x$token_counts,
                   y_counts,# = t_dat$y$token_counts,
                   by = token_count_join,
                   suffix = paste0('.' ,suffix)
  ) |>
    dplyr::mutate(!!dplyr::sym(n_nms_x) := tidyr::replace_na(!!dplyr::sym(n_nms_x), 0)) |>
    dplyr::mutate(!!dplyr::sym(n_nms_y) := tidyr::replace_na(!!dplyr::sym(n_nms_y), 0)) |>
    #dplyr::mutate(n_comparisons = !!dplyr::sym(n_nms_x)*!!dplyr::sym(n_nms_y)) |>
    # as.double is added because of issues with the size of R's Integer
    dplyr::mutate(n_comparisons = as.double(!!dplyr::sym(n_nms_x))*as.double(!!dplyr::sym(n_nms_y))) |>
    dplyr::mutate(u_prob = (n_comparisons) / total_comparisons) |>
    dplyr::arrange(dplyr::desc(u_prob)) |>
    {\(.) dplyr::mutate(., m_prob = m_prob_func(.))}()


}


#' Joins two objects together that come back from the tokenize_ations function, joins the token counts together, and calculates the m and u probs for each token.
#'
#' @param x list returned from tokenize_ations
#' @param y list returned from tokenize_ations
#' @param suffix String vector of length 2, identifies which original dataframe a column in the result comes from. Default TOKEN_SUFFIX_DEFAULT
#' @param ... ignored
#'
#' @examples
#' dat_ceo <- readr::read_csv('https://tinyurl.com/2p8etjr6')
#' dat_alb <- readr::read_csv('https://tinyurl.com/2p8ap4ad')
#' tokenize_ations_m_u_prob(
#'   tokenize_ations(dat_ceo, col_nms = 'coname', token_types = 'TT'),
#'   tokenize_ations(dat_alb, col_nms = 'companyName', token_types = 'TT')
#' )
#'
#'
#'@export
tokenize_ations_m_u_prob <- function(x, y,
                                   ...,
                                   suffix = TOKEN_SUFFIX_DEFAULT
                                   ){

  t_dat <- list(x = x, y = y )

  ########################
  t_dat$x$suffix = suffix[[1]]
  t_dat$y$suffix = suffix[[2]]
  t_dat$total_comparisons <- as.double(nrow(x$dat)) * as.double(nrow(y$dat))
  #t_dat$lambda <- 1 /t_dat$total_comparisons



  #########################
  # calculate the m and u prob for each token
  t_dat$tokens_all <-
    generate_all_tokens(x_counts = t_dat$x$token_counts,
                        y_counts = t_dat$y$token_counts,
                        total_comparisons = t_dat$total_comparisons,
                        suffix = suffix,
                        ...
                        )

  t_dat
}


#' Returns the required information about the joint probability of the tokens in one object
#'
#' @param dat_x dataframe
#' @param dat_y dataframe
#' @param col_nms_x vector of string column names to tokenize. Default all character columns
#' @param col_nms_y vector of string column names to tokenize. Default all character columns
#' @param args_x list of arguments passed to 'tokenize_ations'
#' @param args_y list of arguments passed to 'tokenize_ations'
#' @param row_name_nm string that is the name of the column to get the rownames. Default TOKENIZE_DEFAULT_ROW_NAME
#' @param suffix vector of length two with suffixs to add to column names as needed
#' @param token_count_join vector of column names to join the token count dataframes on. Default TOKEN_TOKEN_TYPE_VEC
#' @param m_prob_func a function that takes a dataframe with counts of tokens then returns a vector of m_probs
#' @param ... arguments passed to 'tokenize_ations', note ... is  the lowest priority and all other passed first
#'
#'
#' @examples
#' dat_x = readr::read_csv('https://tinyurl.com/2p8etjr6')
#' dat_y = readr::read_csv('https://tinyurl.com/2p8ap4ad' )
#' t_dat <- token_links(
#'      dat_x = dat_x,
#'      dat_y = dat_y,
#'      args_x = list(col_nms = 'coname'),
#'      args_y = list(col_nms = 'companyName'),
#'      token_types = 'company_name',
#'      token_index = '',
#'      suffix = c('ceo', 'alb')
#' )
#'
#'
#'
#' @export
token_links <- function(dat_x, dat_y,
                            col_nms_x = dat_x |>
                              dplyr::select_if(is.character) |>
                              colnames(),
                            col_nms_y = dat_y |>
                              dplyr::select_if(is.character) |>
                              colnames(),
                            args_x = list(),
                            args_y = list(),
                        row_name_nm = TOKENIZE_DEFAULT_ROW_NAME,
                        suffix = TOKEN_SUFFIX_DEFAULT,
                        token_count_join = TOKEN_TOKEN_TYPE_VEC,
                        m_prob_func = calc_m_prob,
                        ...
                        ){

  ######################
  #  genarate arguments list for passing
  args_x <-
      args_x |>
      maybe_add('col_nms', col_nms_x) |>
      maybe_add('dat', dat_x) |>
      maybe_add('row_name_nm', row_name_nm)


  args_y <-
      args_y |>
      maybe_add('col_nms', col_nms_y) |>
      maybe_add('dat', dat_y) |>
      maybe_add('row_name_nm', row_name_nm)

  common_args <- list(...)
  #common_args <- list()
  purrr::map2(names(common_args), common_args,
       function(nm, val){
         args_x <<- args_x |> maybe_add(nm, val)
         args_y <<- args_y |> maybe_add(nm, val)
  })



  #tokenize_ations(dat = args_x$dat, col_nms = args_x$col_nms, row_name_nm = 'row')
  t_dat <- tokenize_ations_m_u_prob(x = do.call(tokenize_ations, args_x),
                                    y = do.call(tokenize_ations, args_y),
                                  suffix = suffix,
                                  ...
                                    )
  return(t_dat)
}


#' Generates vector of priori values one for each row in x_y_rec_checks,
#' This function is the default for creating priori values but,..
#' You can inject your own priori functions by passing a function into the priori_func parameter into the find_posterior function.
#' this function should be able to take all the parameters that this function takes
#'
#'
#' @param x_y_rec_checks Dataframe with records indicating row_numbers of each of the datasets.
#' @param n_x Integer. Number of records in first dataset
#' @param n_y Integer. Number of records in second dataset
#' @param total_comparisons Integer. Indicates total Comparisons that could be done between datasets#'
#' @param row_name_nm String. rowname to use for each dataset. Default TOKENIZE_DEFAULT_ROW_NAME
#' @param suffix String vector of length 2. Default TOKEN_SUFFIX_DEFAULT
#' @param ... ignored
#'
#' @examples
#'
#' calculate_priori(
#'                  x_y_rec_checks = dplyr::tibble(row_name.x = sample(1:1000, 100, replace=TRUE),
#'                         row_name.y = sample(1:1000000, 100, replace=TRUE)),
#'                  n_x = 100,
#'                  n_y = 1000000
#' )
#'
#' @export
calculate_priori <- function(x_y_rec_checks,
                             n_x,
                             n_y,
                             total_comparisons = NULL,
                             row_name_nm = TOKENIZE_DEFAULT_ROW_NAME,
                             suffix = TOKEN_SUFFIX_DEFAULT,
                             ...
                             ){

  total_comparisons <-
    if(is.null(total_comparisons)){
     as.double(n_x) * as.double(n_y)
    }else{total_comparisons}

  return(rep(1/total_comparisons,nrow(x_y_rec_checks)))
}






#' Given a dataframe of all tokens, object will return a dataframe of tokens that is a subset of the dataset
#'
#' @param tokens_all a dataframe normally from  t_dat$tokens_all
#' @param min_token_u_prob minimum u_prob to keep, can be NULL to not filter: Default TOKEN_MIN_UPROB_DEFAULT
#' @param max_total_comparisons maximum number of comparisons to allow it will pick tokens with the smallest number of n_comparisons first, NULL is also allowed to not filder : Default  25000000
#' @param remove_n_comparisons_zero Remove tokens that can not be included in comparisons: Default TRUE
#' @param ... Ignored
#'
#' @return
#' @export
#'
#' @examples
keep_tokens <- function(tokens_all,
                        min_token_u_prob = TOKEN_MIN_UPROB_DEFAULT,
                        max_total_comparisons = TOKEN_MAX_COMPARE_DEFAULT,
                        remove_n_comparisons_zero = TOKEN_REMOVE_ZERO_COMARE,
                        ...
){
  tokens_all |>
    maybe_do(remove_n_comparisons_zero, \(x){dplyr::filter(x, u_prob != 0)}) |>
    maybe_do(!is.null(min_token_u_prob), \(x){dplyr::filter(x, u_prob < min_token_u_prob)}) |> #head(20)
    #pull(n_comparisons) |> sum()
    maybe_do(!is.null(max_total_comparisons),
             \(x){
               x |>
                 dplyr::arrange(n_comparisons) |>
                 dplyr::filter(cumsum(n_comparisons) < max_total_comparisons) |>
                 dplyr::arrange(dplyr::desc(n_comparisons))
             })
}









#' Creates a subset of pairs to check in more detail.
#'
#' @param t_dat t_dat object
#' @param min_posterior filter posterior results above this value. Default 0.2.
#' @param token_join_by vector column names that joins the tokens. Default TOKEN_TOKEN_TYPE_VEC
#' @param tokens_to_keep NULL or dataframe with a list of tokens, and m_prob and u_prob for each token, in the case where it is NULL we use tokenized$tokens_all filtered by min_token_u_prob
#' @param return_all if TRUE it returns the whole object if FALSE it just returns the dataframe. Default TRUE
#' @param priori_func A function that will calculate the priori. Default calculate_priori
#' @param remove_identical_row_index
#' @param ... passed to keep_tokens() if tokens_to_keep is NULL
#'
#'@examples
#'token_links(
#'  dat_x = readr::read_csv('https://tinyurl.com/2p8etjr6'),
#'  dat_y = readr::read_csv('https://tinyurl.com/2p8ap4ad' ),
#'  args_x = list(col_nms = 'coname'),
#'  args_y = list(col_nms = 'companyName'),
#'  token_types = 'company_name',
#'  token_index = '',
#'  suffix = c('ceo', 'alb')
#') |> find_posterior_positive_evidence_only(return_all = FALSE)
#'
#'@export
find_posterior_positive_evidence_only <- function(t_dat,
                                                  min_posterior = MIN_POSTERIOR_POSITIVE_EVIDENCE_ONLY,
                                                  token_join_by = TOKEN_TOKEN_TYPE_VEC,
                                                  tokens_to_keep = NULL,
                                                  #min_token_u_prob = TOKEN_MIN_UPROB_DEFAULT,
                                                  return_all = TRUE,
                                                  priori_func = calculate_priori,
                                                  remove_identical_row_index = FALSE,
                                                  #remove_identical_strings = FALSE,
                                                  ...){

  suffix <-paste0(".",c(t_dat$x$suffix, t_dat$y$suffix))
  x_y_indexes <- c(t_dat$x$row_name_nm, t_dat$y$row_name_nm) |> paste0(suffix)


  tic <- Sys.time()


  #####################
  # Only keep useful tokens
  tokens_to_keep <-
    if (is.null(tokens_to_keep)){
      t_dat$tokens_all |> keep_tokens(...)
    }else{tokens_to_keep}


  message(glue::glue('Checking positive evidence only. {t_dat$x$suffix}={nrow(t_dat$x$dat)}, {t_dat$y$suffix}={nrow(t_dat$y$dat)}, tokens used {nrow(tokens_to_keep)}, expected comparison max = {sum(tokens_to_keep$n_comparisons)}.'))

  # tokens_to_keep <-
  #   if (is.null(tokens_to_keep)){
  #       t_dat$tokens_all |>
  #       dplyr::filter(u_prob != 0) |>
  #       dplyr::filter(u_prob < min_token_u_prob)
  #   }else{tokens_to_keep}

  # if (remove_identical_strings){
  #
  # }


  #positive_evidence_only <-

  all_common_token_values <-
    t_dat$x$tokens |>
    dplyr::inner_join(tokens_to_keep,
                      by = token_join_by,
                      suffix = suffix) |>
    dplyr::left_join(t_dat$y$tokens,
                     by = token_join_by,
                     suffix = suffix) |>
    dplyr::select(dplyr::all_of(c(x_y_indexes, 'u_prob', 'm_prob')))  |>
    data.table::setDT()


  indicies_n <- all_common_token_values[, .(n = .N), by = x_y_indexes]

  indicies_one <-
    indicies_n[indicies_n[['n']] == 1]

  all_common_token_values_many <-
    indicies_n[indicies_n[['n']] > 1] |>
    merge(all_common_token_values, by = x_y_indexes)


  indicies_many <-
    all_common_token_values_many[, .(n = .N,
                                     u_prob_prod = prod(u_prob),
                                     m_prob_prod = prod(m_prob)),
                                     by = x_y_indexes]

  indicies_one_b <-
    merge(indicies_one,
          all_common_token_values,
          by = x_y_indexes,
          suffixes = c("", "_prod")
          )


  indicies_one_b[['u_prob_prod']] <- indicies_one_b[['u_prob']]
  indicies_one_b[['u_prob']] <- NULL

  indicies_one_b[['m_prob_prod']] <- indicies_one_b[['m_prob']]
  indicies_one_b[['m_prob']] <- NULL

  positive_evidence_only <-
    rbind(indicies_one_b, indicies_many) |>
    {\(.) dplyr::mutate(., priori = priori_func(.,
                                                n_x = nrow(t_dat$x$dat),
                                                n_y = nrow(t_dat$y$dat),
                                                total_comparisons = as.double(nrow(t_dat$y$dat)) * as.double(nrow(t_dat$y$dat)),# = t_dat$total_comparisons,
                                                suffix = suffix,
                                                row_name_nm = c(t_dat$x$row_name_nm, t_dat$y$row_name_nm)
    ))}() |>
    dplyr::mutate(m_prob_prod_lambda = m_prob_prod * priori) |>
    #dplyr::mutate(m_prob_prod_lambda = m_prob_prod * t_dat$lambda) |>
    dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-priori)) |>
    dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
    #dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + (1- t_dat$lambda) * u_prob_prod) )  ) |>
    #t_dat$positive_evidence_only |>
    dplyr::select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |>
    {\(.) if(is.null(min_posterior)) . else dplyr::filter(., posterior > min_posterior)}() |>
    #dplyr::filter(posterior >= min_posterior) |>
    dplyr::distinct()

  positive_evidence_only <-  positive_evidence_only |> dplyr::as_tibble()

  #
  # indicies_one_b
  # indicies_one
  #
  #
  # indicies_many |> nrow()
  # all_common_token_values_many |> nrow()
  # all_common_token_values |> nrow()
  # indicies_one_b |> nrow()
  #
  # indicies <- for_matrixify[, .(n = .N), by = x_y_indexes]
  #
  #   dplyr::group_by_at(x_y_indexes)
  #
  # for_matrixify[, .(n = .N, pro), by = x_y_indexes]
  #
  #
  # indicies_many |> nrow()
  # indicies_one |> nrow()
  # for_matrixify
  # indicies <- for_matrixify |> distinct_at(x_y_indexes)
  # dat[, .(count = .N, var = sum(VAR)), by = MNTH]
  #
  # iris_x <- as.tibble(mtcars)
  # setDT(iris_x)
  # iris_x[, .(n = .N), by = c('vs', 'am')]
  #
  # iris_x |> count(Species)
  # library(data.table)
  # for_matrixify  |> setDT()
  # setDT(iris_tib)
  #
  #
  # indicies |>
  #   set_names(c(".x", ".y")) |>
  #   sample_n(1000) |>
  #   purrr::pmap(\(.x, .y){
  #     print(.x)
  #   })
  #
  # indicies <- for_matrixify |> dplyr::count(!!rlang::sym(x_y_indexes))
  #
  # for_matrixify |> write_feather('for_matrixify.feather')
  #
  # gc()
  #
  # for_matrixify <-
  #   for_matrixify |>
  #   dplyr::group_by_at(x_y_indexes)
  #
  #
  # gc()
  # but I
  #   # {\(.){
  #   #   if (check_identical_strings){
  #   #     .
  #   #   }else{
  #   #     #filter(., !!sym(t_dat$x$row_name_nm) != !!sym(t_dat$y$row_name_nm))
  #   #     dplyr::filter(., !!rlang::sym(t_dat$x$col_nms) != !!rlang::sym(t_dat$y$col_nms))
  #   #   }
  #   # }}() |>
  #   maybe_do(remove_identical_row_index, \(x){dplyr::filter(x, !!rlang::sym(x_y_indexes[[1]]) != !!rlang::sym(x_y_indexes[[2]]))}) |>
  #
  #   #filter(row_name.x == '101') %>%
  #   select(all_of(c(x_y_indexes, 'u_prob', 'm_prob')))#|>
  #
  #
  # library(bigmemory)
  # library(DBI)
  # library(ldat)
  #
  # for_matrixify ::
  #
  # m <-
  #   for_matrixify |>
  #   as_ldat()
  #
  #
  # m
  #
  # m |>
  #   group_by(x_y_indexes)
  #
  #
  #
  #   mutate_all(as.double) |>
  #   as.matrix() |>
  #   bigmemory::as.big.matrix()
  #
  # reclin::add_from_y()
  # n <-
  #   m |>
  #   group_by(row_name.vend,row_name.ised)
  #   dplyr::group_by_at(x_y_indexes) |>
  #   #select(all_of(c(x_y_indexes, 'u_prob', 'm_prob'))) |>
  #   dplyr::summarise(u_prob_prod = prod(u_prob),
  #                    m_prob_prod = prod(m_prob),
  #                    n_tokens = dplyr::n(),
  #                    .groups = 'drop'
  #   ) |>
  #   {\(.) dplyr::mutate(., priori = priori_func(.,
  #                                     n_x = nrow(t_dat$x$dat),
  #                                     n_y = nrow(t_dat$y$dat),
  #                                     total_comparisons = as.double(nrow(t_dat$y$dat)) * as.double(nrow(t_dat$y$dat)),# = t_dat$total_comparisons,
  #                                     suffix = suffix,
  #                                     row_name_nm = c(t_dat$x$row_name_nm, t_dat$y$row_name_nm)
  #                                     ))}() |>
  #   dplyr::mutate(m_prob_prod_lambda = m_prob_prod * priori) |>
  #   #dplyr::mutate(m_prob_prod_lambda = m_prob_prod * t_dat$lambda) |>
  #   dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-priori)) |>
  #   dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
  #   #dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + (1- t_dat$lambda) * u_prob_prod) )  ) |>
  #   #t_dat$positive_evidence_only |>
  #   dplyr::select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |>
  #   {\(.) if(is.null(min_posterior)) . else dplyr::filter(., posterior > min_posterior)}() |>
  #   #dplyr::filter(posterior >= min_posterior) |>
  #   dplyr::distinct()


  toc <- Sys.time()
  message(glue::glue('found {nrow(positive_evidence_only)} records in {round(difftime(toc,tic, units = "mins"),1)} minutes'))




  if (return_all){
    t_dat$posterior_threshold_positive_only <- min_posterior
    #t_dat$min_token_u_prob <- min_token_u_prob
    t_dat$positive_evidence_only  <- positive_evidence_only
    return(t_dat)
  }else{
    return(positive_evidence_only)
  }
}







#' t_dat should have been run through find_posterior_positive_evidence_only() first to create the list of candidates
#'
#' @param t_dat t_dat object
#' @param min_posterior Filter results below this threshold Default = 0.01
#' @param token_join_by  What to join the all_tokens by. Default = TOKEN_TOKEN_TYPE_VEC,
#' @param positive_evidence_only, NULL or a dataframe with at least two columns indicating the row_names to be checked in each of the datasets
#' @param return_all If TRUE it returns the whole object if FALSE it just returns the dataframe. Default TRUE
#' @param priori_func A function that will calculate the priori. Default calculate_priori
#' @param ... Not used
#'
#'
#'@examples
#'
#'
#' t_dat <-
#'   token_links(
#'      dat_x = readr::read_csv('https://tinyurl.com/2p8etjr6'),
#'      dat_y = readr::read_csv('https://tinyurl.com/2p8ap4ad'),
#'      args_x = list(col_nms = 'coname'),
#'      args_y = list(col_nms = 'companyName'),
#'      token_types = 'company_name',
#'      token_index = '',
#'      suffix = c('ceo', 'alb')
#'   )
#'
#' results <-
#'   t_dat |>
#'     find_posterior_positive_evidence_only() |>
#'     find_posterior_all_evidence(return_all = FALSE)
#'
#'
#'@export
find_posterior_all_evidence <- function(t_dat,
                                        min_posterior = MIN_POSTERIOR_ALL_EVIDENCE,
                                        token_join_by = TOKEN_TOKEN_TYPE_VEC,
                                        positive_evidence_only = NULL,
                                        return_all = TRUE,
                                        priori_func = calculate_priori,
                                        ...
                                        ){
  suffix0 <-paste0(".",c(t_dat$x$suffix, t_dat$y$suffix))
  suffix1 <-paste0("_",c(t_dat$x$suffix, t_dat$y$suffix))
  x_y_indexes <- c(t_dat$x$row_name_nm, t_dat$y$row_name_nm) |> paste0(suffix0)
  x_y_indexes1 <- c(t_dat$x$row_name_nm, t_dat$y$row_name_nm) |> paste0(suffix1)
  x_indexes <- x_y_indexes[[1]]
  y_indexes <- x_y_indexes[[2]]
  x_indexes1 <- x_y_indexes1[[1]]
  y_indexes1 <- x_y_indexes1[[2]]

  x_row_nm <- t_dat$x$row_name_nm
  y_row_nm <- t_dat$y$row_name_nm


  positive_evidence_only <-
    if (is.null(positive_evidence_only)){
      t_dat$positive_evidence_only
    }else{positive_evidence_only}






  all_evidence_index_tokens <-
    dplyr::bind_rows(
      dplyr::inner_join(
        t_dat$x$tokens |> dplyr::rename(!!rlang::sym(x_indexes) := x_row_nm), # rename_all(~{paste0(.x, suffix0[[1]])}),
        positive_evidence_only |> dplyr::distinct_at(x_y_indexes),
        by = x_indexes
      ),
      dplyr::inner_join(
        t_dat$y$tokens |> dplyr::rename(!!rlang::sym(y_indexes) := y_row_nm), #rename_all(~{paste0(.x, suffix0[[2]])}),
        positive_evidence_only |> dplyr::distinct_at(x_y_indexes),
        by = y_indexes
    )) |>
    data.table::setDT()


  all_common_token_values_many <-
    all_evidence_index_tokens[, .(n_dat_set = .N),
                               by = c(x_y_indexes, token_join_by)]



  all_common_token_values_many[['evidence_in_favour']] <- all_common_token_values_many[['n_dat_set']] == 2

  x <- merge(all_common_token_values_many,
              t_dat$tokens_all,
              by = token_join_by
              )

  x[['m_prob']] <-  dplyr::if_else(x[['evidence_in_favour']] , x[['m_prob']], 1-x[['m_prob']])
  x[['u_prob']] <-  dplyr::if_else(x[['evidence_in_favour']] , x[['u_prob']], 1-x[['u_prob']])



  x_2 <-
    x[, .(u_prob_prod = prod(u_prob),
          m_prob_prod = prod(m_prob),
          #n_tokens = dplyr::n(),
          tokens_in_favour = sum(evidence_in_favour),
          tokens_against = sum(!evidence_in_favour)), by = x_y_indexes]




  all_evidence <-
    x_2 |>
    {\(.) dplyr::mutate(., priori = priori_func(.,
                                                n_x = nrow(t_dat$x$dat),
                                                n_y = nrow(t_dat$y$dat),
                                                total_comparisons = as.double(nrow(t_dat$y$dat)) * as.double(nrow(t_dat$y$dat)),# = t_dat$total_comparisons,
                                                suffix = suffix,
                                                row_name_nm = c(t_dat$x$row_name_nm, t_dat$y$row_name_nm)
    ))}() |>

    dplyr::mutate(m_prob_prod_lambda = m_prob_prod * priori) |>
    dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-priori)) |>
    dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
    dplyr::select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |> #filter(row_name.x != row_name.y)
    {\(.) if(is.null(min_posterior)) . else dplyr::filter(., posterior > min_posterior)}() |>
    #dplyr::filter(posterior > min_posterior) |>
    #dplyr::arrange(posterior)
    dplyr::arrange(dplyr::desc(posterior))






  #
  #
  #
  # x_2[['priori']] <-
  #   priori_func(x_2,
  #               n_x = nrow(t_dat$x$dat),
  #               n_y = nrow(t_dat$y$dat),
  #               total_comparisons = as.double(nrow(t_dat$y$dat)) * as.double(nrow(t_dat$y$dat)),# = t_dat$total_comparisons,
  #               suffix = suffix,
  #               row_name_nm = c(t_dat$x$row_name_nm, t_dat$y$row_name_nm)
  #   )
  # x_2[['m_prob_prod_lambda']] <-x_2[['m_prob_prod']] * x_2[['priori']]
  # x_2[['u_prob_prod_one_lambda']] <-x_2[['u_prob_prod']] * (1-x_2[['priori']])
  # x_2[['posterior']] <-x_2[['m_prob_prod_lambda']] / (x_2[['m_prob_prod_lambda']] + x_2[['u_prob_prod_one_lambda']])
  #
  #
  # x_2[['u_prob_prod']] <- NULL
  # x_2[['m_prob_prod']] <- NULL
  # x_2[['m_prob_prod_lambda']] <- NULL
  # x_2[['u_prob_prod_one_lambda']] <- NULL
  #
  #
  # all_evidence <-
  #   maybe_do(x_2, ! is.null(min_posterior), dplyr::filter, posterior > min_posterior) |>
  #   dplyr::arrange(dplyr::desc(posterior)) |>
  #   dplyr::as_tibble()

  #
  #
  #
  # min_posterior
  # dplyr::mutate(m_prob_prod_lambda = m_prob_prod * priori) |>
  #   dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-priori)) |>
  #   dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
  #   dplyr::select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |> #filter(row_name.x != row_name.y)
  #   {\(.) if(is.null(min_posterior)) . else dplyr::filter(., posterior > min_posterior)}()
  #
  # all_evidence <-
  #   all_evidence_index_tokens |>
  #   dplyr::group_by_at(c(x_y_indexes, token_join_by)) |>
  #   dplyr::summarise(n_dat_set = dplyr::n(), .groups = 'drop') |> #filter(row_name.x != row_name.y)
  #   dplyr::mutate(evidence_in_favour = n_dat_set == 2) |>#filter(row_name.x != row_name.y)
  #   #filter(row_name.EDGAR == '11991' & row_name.ALB == '2') |>
  #   dplyr::inner_join(t_dat$tokens_all,
  #                     by = token_join_by) |>
  #   dplyr::mutate(m_prob = dplyr::if_else(evidence_in_favour , m_prob, 1-m_prob)) |>
  #   dplyr::mutate(u_prob = dplyr::if_else(evidence_in_favour , u_prob, 1-u_prob)) |>
  #   dplyr::group_by_at(x_y_indexes) |>
  #   dplyr::summarise(u_prob_prod = prod(u_prob),
  #                    m_prob_prod = prod(m_prob),
  #                    #n_tokens = dplyr::n(),
  #                    tokens_in_favour = sum(evidence_in_favour),
  #                    tokens_against = sum(!evidence_in_favour),
  #                    .groups = 'drop'
  #   ) |>
  #   {\(.) dplyr::mutate(., priori = priori_func(.,
  #                                               n_x = nrow(t_dat$x$dat),
  #                                               n_y = nrow(t_dat$y$dat),
  #                                               total_comparisons = as.double(nrow(t_dat$y$dat)) * as.double(nrow(t_dat$y$dat)),# = t_dat$total_comparisons,
  #                                               suffix = suffix,
  #                                               row_name_nm = c(t_dat$x$row_name_nm, t_dat$y$row_name_nm)
  #   ))}() |>
  #
  #   dplyr::mutate(m_prob_prod_lambda = m_prob_prod * priori) |>
  #   dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-priori)) |>
  #   dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
  #   dplyr::select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |> #filter(row_name.x != row_name.y)
  #   {\(.) if(is.null(min_posterior)) . else dplyr::filter(., posterior > min_posterior)}() |>
  #   #dplyr::filter(posterior > min_posterior) |>
  #   #dplyr::arrange(posterior)
  #   dplyr::arrange(dplyr::desc(posterior))



  if (return_all){
    t_dat$posterior_threshold_all_evidence <- min_posterior
    t_dat$all_evidence  <- all_evidence
    return(t_dat)
  }else{
    return(all_evidence)
  }
}



#' Appends dataframes with posteriors and returns it
#'
#' @param t_dat tdat object
#' @param min_posterior_all_evidence passed to find_posterior_all_evidence as min_posterior. Default 0.01
#' @param min_posterior_positive_evidence_only passed to find_posterior_all_evidence as min_posterior. Default 0.3
#' @param ... passed wholsale to both find_posterior_positive_evidence_only, and find_posterior_all_evidence
#'
#' @examples
#' t_dat <-
#'   token_links(
#'      dat_x = readr::read_csv('https://tinyurl.com/2p8etjr6'),
#'      dat_y = readr::read_csv('https://tinyurl.com/2p8ap4ad'),
#'      args_x = list(col_nms = 'coname'),
#'      args_y = list(col_nms = 'companyName'),
#'      token_types = 'company_name',
#'      token_index = '',
#'      suffix = c('ceo', 'alb')
#'   )
#'
#'   t_dat <- t_dat |> find_posterior()
#'
#'
#' @export
find_posterior <- function(t_dat,
                       min_posterior_all_evidence = MIN_POSTERIOR_ALL_EVIDENCE,
                       min_posterior_positive_evidence_only = MIN_POSTERIOR_POSITIVE_EVIDENCE_ONLY,
                       ...){


  t_dat <- find_posterior_positive_evidence_only(t_dat,
                                                 min_posterior = min_posterior_positive_evidence_only,
                                                 return_all  = TRUE,
                                                 ...)
  t_dat <- find_posterior_all_evidence(t_dat,
                                       min_posterior = min_posterior_all_evidence,
                                       return_all  = TRUE,
                                       ...)

  t_dat
}



#' Returns a joined dataframe
#'
#' @param t_dat a t_dat object
#' @param include_row_numbers Boolean Do we include the row names. Default False
#' @param pairs_to_get either 'all_evidence' or 'positive_evidence' or a dataframe. Default 'all_evidence'
#' @param link_col_nms vector of column names to includ from the joining dataframe. Default 'Posterior'
#' @param orig_data_to_include either 'matched' or 'all' and determines what is returned from the x and y dataframes. Default 0.3
#' @param ... Ignored
#'
#'
#' @examples
#'
#' dat_ceo <- readr::read_csv('https://tinyurl.com/2p8etjr6')
#' dat_alb <- readr::read_csv('https://tinyurl.com/2p8ap4ad')
#' token_links(
#' dat_x = dat_ceo,
#' dat_y = dat_alb,
#' args_x = list(col_nms = 'coname'),
#' args_y = list(col_nms = 'companyName'),
#' token_types = 'company_name',
#' token_index = '',
#' suffix = c('ceo', 'alb')
#' ) |> find_posterior_positive_evidence_only() |>
#' find_posterior_all_evidence() |>
#' joined_results()
#'
#'@export
joined_results <- function(t_dat,
                           include_row_numbers = FALSE,
                           pairs_to_get = 'all_evidence',#'positive_evidence'
                           link_col_nms = c('posterior'),
                           orig_data_to_include  = 'matched',#,'all'
                           #min_posterior = 0.3,
                           ...
                           ){


  ##############
  # get evidence to use
  evidence_to_use <-
    if (pairs_to_get == 'all_evidence'){t_dat$all_evidence
    }else if (pairs_to_get == 'positive_evidence'){t_dat$positive_evidence_only
    }else if (is.data.frame(pairs_to_get)){pairs_to_get
    }else{
      warning(glue::glue('Error in function "joined_results", variable pairs_to_get is not an acceptable value'))
      assert(FALSE)
    }




  suffix0 <-paste0(".",c(t_dat$x$suffix, t_dat$y$suffix))
  x_y_indexes <- c(t_dat$x$row_name_nm, t_dat$y$row_name_nm) |> paste0(suffix0)
  x_indexes <- x_y_indexes[[1]]
  y_indexes <- x_y_indexes[[2]]

  if (orig_data_to_include == 'matched'){
    x_cols <- t_dat$x$col_nms
    y_cols <- t_dat$y$col_nms
  } else if (orig_data_to_include == 'all'){
    x_cols <- colnames(t_dat$x$dat)
    y_cols <- colnames(t_dat$y$col_nms)
  } else{
    testit::assert(FALSE)
  }



  evidence_to_use |>
    dplyr::select(dplyr::all_of(c(x_indexes,y_indexes,link_col_nms))) |>
    dplyr::left_join(
      t_dat$x$dat |>
        dplyr::select(dplyr::all_of(x_cols)) |>
        #tibble::rownames_to_column(var = t_dat$x$row_name_nm) |>
        tibble::rowid_to_column(var = t_dat$x$row_name_nm) |>
        dplyr::rename_all(\(x) paste0(x,'.', t_dat$x$suffix)),
      by = x_indexes) |>
    dplyr::left_join(
      t_dat$y$dat |>
        dplyr::select(dplyr::all_of(y_cols )) |>
        #tibble::rownames_to_column(var = t_dat$y$row_name_nm) |>
        tibble::rowid_to_column(var = t_dat$x$row_name_nm) |>
        dplyr::rename_all(\(x) paste0(x,'.', t_dat$y$suffix)),
      by = y_indexes) |>
    {\(.) if (include_row_numbers) . else dplyr::select(., -dplyr::any_of(c(x_y_indexes))   ) }()
}






#'
#' Returns a dataframe with two columns indicating the rows of each Df that are to be paired
#'
#' @param t_dat, a list that is a t_dat object
#' @param x_tokens defaults as t_dat$x$tokens
#' @param y_tokens defaults as t_dat$y$tokens
#' @param tokens_to_keep dataframe indicate which tokens matterin the analysis. typically this is a filtered subset of t_dat$tokens_all. Default is NULL. If value is NULL we filter t_dat$tokens_all by min_token_u_prob and u_prob != 0
#' @param token_join_by defaults to  TOKEN_TOKEN_TYPE_VEC
#' @param suffix defaults to paste0(".",c(t_dat$x$suffix, t_dat$y$suffix))
#' @param min_token_u_prob minimum u_prob of token to use as join
#'
#' @export
get_paired_row_names <- function(t_dat,
                                 x_tokens = t_dat$x$tokens,
                                 y_tokens = t_dat$y$tokens,
                                 tokens_to_keep = NULL,
                                 token_join_by = TOKEN_TOKEN_TYPE_VEC,
                                 suffix  = paste0(".",c(t_dat$x$suffix, t_dat$y$suffix)),
                                 min_token_u_prob = TOKEN_MIN_UPROB_DEFAULT){
  tokens_to_keep <-
    if (is.null(tokens_to_keep)){
      t_dat$tokens_all |>
        dplyr::filter(u_prob != 0) |>
        dplyr::filter(u_prob < min_token_u_prob)
    }else{tokens_to_keep}



  row_name_nm_x <- paste0(t_dat$x$row_name_nm, '.', t_dat$x$suffix)
  row_name_nm_y <- paste0(t_dat$y$row_name_nm, '.', t_dat$y$suffix)

  t_dat$x$tokens |>
    dplyr::inner_join(tokens_to_keep,
                      by = token_join_by,
                      suffix = suffix) |>
    dplyr::inner_join(t_dat$y$tokens,
                     by = token_join_by,
                     suffix = suffix) |>
    dplyr::distinct(!!dplyr::sym(row_name_nm_x), !!dplyr::sym(row_name_nm_y))
}



#'
#'creates a pair blocking based on columns passed into blocking_var, then then it generates more pairs based on token,
#'that are the same with columns col_nms_x and col_nms_y.
#'
#' @param x dataframe
#' @param y dataframe
#' @param blocking_var vector of column names to block on, unlike reclin these columns are joined with 'or'
#' @param token_types vector of token types
#' @param large Passed to reclin::pair_blocking. Default FALSE
#' @param add_xy Boolean Default True. This is passed to reclin::pair_blocking, inside map_dfr
#' @param chunk_size passed to reclin::pair_blocking. Default 1E+07
#' @param col_nms_x passed to token_links
#' @param col_nms_y passed to token_links
#' @param min_token_u_prob passed to get_paired_row_names
#' @param ... passed to token_links
#'
#'@export
reclin_pair_blocking <- function(x, y,
                                 blocking_var,
                                 token_types,
                                 large = FALSE,
                                 add_xy = TRUE,
                                 chunk_size = 1E+07,
                                 col_nms_x = x |> select_if(is_character) |> colnames(),
                                 col_nms_y = y |> select_if(is_character) |> colnames(),
                                 min_token_u_prob = TOKEN_MIN_UPROB_DEFAULT,
                                 ...){


  # col_nms_x = c('company_name', 'address')
  # col_nms_y = c('company_name', 'address')
  # token_types = c('cn', 'add')
  blocked_pairs <- blocking_var |> purrr::map_dfr(~{reclin::pair_blocking(x, y, blocking_var  = .x, large = large, add_xy = add_xy, chunk_size = chunk_size)})

  # blocked_pairs |>
  #   mutate(is_same = (x == y)) |>
  #   filter(is.na(is_same))
  token_blocking <-
    token_links(x, y, col_nms_x = col_nms_x, col_nms_y = col_nms_y, token_types= token_types, ...) |>
    get_paired_row_names(min_token_u_prob = min_token_u_prob) |>
    purrr::set_names(c('x', 'y')) |>
    dplyr::mutate_all(as.integer)


# token_blocking |>
#   mutate(is_same = (x == y)) |>
#   count(is_same)


  dplyr::bind_rows(blocked_pairs, token_blocking) |>
    dplyr::distinct()
}









#' Scales a vector from 1-priori_delta to priori_delta
#'
#'@param x vector of numbers
#'@param priori_delta how much to compress the 0->1 scale range default PRIORI_DELTA
#'
#'@return
#'  returns a vector
#'@examples
#' scale_to_prob(c(10,9,7,5,34,2,1,0,-1), priori_delta = 0)
#'
#'@export
scale_to_prob <- function(x, priori_delta = PRIORI_DELTA){
  assertthat::assert_that(priori_delta >= 0)
  assertthat::assert_that(priori_delta < 0.5)

  #x <- P$x_weight
  x2 <- (x-min(x))/(max(x)-min(x))
  x2 * (1-(2*priori_delta))+priori_delta
}




#'
#' After generating probabilities for a list of pairs this will refine the probabilities
#'
#' @param p should be a dataframe like object with atleast 3 columns 'x', 'y' indicating the row names of the x_dat and y_dat that are being compared
#' @param x_dat dataframe to check
#' @param y_dat dataframe to check
#' @param weights_nm Name of the column in pairs object that contains some kind of score or probability, which will be re scaled to a probability
#' @param priori_delta passed to scale_to_prob. Default = 0.01
#' @param args_x passed to token_links. Default list(col_nms = 'company_name')
#' @param args_y passed to token_links. Default list(col_nms = 'company_name')
#' @param token_types  passed to token_links. Default 'company_name'
#' @param token_index  passed to token_links. Default is a blank string.
#' @param suffix  passed to token_links. Default is TOKEN_SUFFIX_DEFAULT
#'
#'
#'@return
#'  returns a dataframe with x and y columns as well as priori and posterior from the original dataframe 'p' containing pairs with priori
#'

#'
#'@export
refine_posterior <- function(
    p,
    x_dat,# = orig_dat,
    y_dat,# = edited_dat,
    weights_nm = 'x_weight',
    priori_delta = 0.01,
    args_x = list(col_nms = 'company_name'),
    args_y = list(col_nms = 'company_name'),
    token_types = 'company_name',
    token_index = '',
    suffix = TOKEN_SUFFIX_DEFAULT,
    token_join_by = TOKEN_TOKEN_TYPE_VEC
){
  p <-
    p |>
    mutate(priori = scale_to_prob(!!sym(weights_nm), priori_delta = priori_delta))


  t_dat <- token_links(
    dat_x = x_dat,
    dat_y = y_dat,
    args_x = args_x,
    args_y = args_y,
    token_types = token_types,
    token_index = token_index,
    suffix = suffix
  )


  suffix0 <-paste0(".",c(t_dat$x$suffix, t_dat$y$suffix))
  x_y_indexes <- c(t_dat$x$suffix, t_dat$y$suffix)



  dplyr::bind_rows(
    p |>
      select(x,y) |>
      mutate_all(as.integer) |>
      left_join(t_dat$x$tokens |> rename(x:=t_dat$x$row_name_nm), by = 'x') ,
    p |>
      select(x,y) |>
      mutate_all(as.integer) |>
      left_join(t_dat$y$tokens |> rename(y:=t_dat$y$row_name_nm), by = 'y')
  ) |>
    dplyr::group_by_at(c(x_y_indexes, token_join_by)) |>
    dplyr::summarise(n_dat_set = dplyr::n(), .groups = 'drop') |>
    dplyr::mutate(evidence_in_favour = n_dat_set == 2) |>
    dplyr::inner_join(t_dat$tokens_all,
                      by = token_join_by) |>
    dplyr::mutate(m_prob = dplyr::if_else(evidence_in_favour , m_prob, 1-m_prob)) |>
    dplyr::mutate(u_prob = dplyr::if_else(evidence_in_favour , u_prob, 1-u_prob)) |>
    dplyr::group_by_at(x_y_indexes) |>
    dplyr::summarise(u_prob_prod = prod(u_prob),
                     m_prob_prod = prod(m_prob),
                     #n_tokens = dplyr::n(),
                     tokens_in_favour = sum(evidence_in_favour),
                     tokens_against = sum(!evidence_in_favour),
                     .groups = 'drop'
    ) |>
    left_join(p |> select(c(x_y_indexes, 'priori')) |> mutate_at(x_y_indexes, as.integer),
              by = x_y_indexes) |>
    #rename(priori := !!sym(priori_nm)) |>
    dplyr::mutate(m_prob_prod_lambda = m_prob_prod * priori) |>
    dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-priori)) |>
    dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
    dplyr::select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |>
    #dplyr::filter(posterior > min_posterior) |>
    #dplyr::arrange(posterior)
    dplyr::arrange(dplyr::desc(posterior))
}


