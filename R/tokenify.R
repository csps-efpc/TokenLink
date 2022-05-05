




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
read_replacements_token_type <- function(token_type = NULL, data_dir = 'helper'){
  if (is.null(token_type))
    return(NULL)
  fn <- file.path(data_dir, glue::glue('token_replace_{token_type}.csv'))
  if (!file.exists(fn)){
    warning(glue::glue('File not found in read_replacements_token_type, for "{token_type}". Returning NULL. No token replacements will be done. fn = "{fn}" '))
    return(NULL)
  }

  readr::read_csv(file =fn, show_col_types = FALSE) |>
    janitor::clean_names() |>
    mutate_if(is.character, function(x){trimws(tolower(x))})
}


#' will apply func to x if bool is TRUE. Saves us from an ugly function
#'
#'
#'@param x a vector
#'@param bool Boolean. applies function if bool is True
#'@param func function that takes x and ...
#'
#'@examples
#'  maybe_do(c(2, 2,100), TRUE, function(x){x*2})
#'  maybe_do(c(2, 2,100), FALSE, function(x){x*2})
#'
#'@export
maybe_do <- function(x, bool, func, ...){
  if(bool)
    return(func(x, ...))

  return(x)
}


########################
#'
#'Replaces tokens, and cleans a string using regex stuff largely, and by doing search and replace.This is the the default string cleaner used before tokenization
#'It can be overriden  in tokenizer_basic, tokenize_col, tokenize_df, etc by passing a new function as pre_token_clean_str.
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
  #x = c('Z.Y. do things inc', 'z. y. DO things montrèal', 'at&t')
  #x <- dat |> filter(stringr::str_detect(name, pattern = '\\.')) |> sample_n(10000) |> pull(name)
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
#'@param dat dataframe. No default.
#'@param ... passed to both clean_str and  tidytext::unnest_tokens
#'@param col_nm string, name of column to tokenize
#'@param row_name_nm string, name of column to put row_name into
#'@param token_col_nm String, column name of new tokens.
#'@param drop_col Boolean. If True drops the original column, default = TRUE
#'@param token_index String. name of column  that will have index of order of tokens in origional column, Default ""
#'@param pre_clean function. that takes vector of strings and ... cleans the string. will clean the string before tokenization. Default clean_str.
#'@param post_clean function. that takes vector of strings and ... cleans the string. will clean the string before tokenization. Default clean_str_2.
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
    mutate(!!dplyr::sym(col_nm) := pre_clean_str(!!dplyr::sym(col_nm), token_type = token_type,  ...)) |>
    tidytext::unnest_tokens(output = !!rlang::sym(token_col_nm), input = col_nm, ...) |>
    {\(.) if (drop_col) {dplyr::select(., -col_nm)} else {.}}() |>
    {\(.) if (nchar(token_index) > 0) dplyr::group_by_at(., row_name_nm) |> dplyr::mutate(!!rlang::sym(token_index) := dplyr::row_number()) |> dplyr::ungroup() else .}() |>
    mutate(!!dplyr::sym(token_col_nm) := post_token_clean_Str(!!dplyr::sym(token_col_nm), ...))

}



#' turns a column of strings into a tokenized dataframe this returned dataframe will have two or three columns
#'
#' @param dat dataframe
#' @param col_nm column that will be tokenized.
#' @param row_name_nm name of a return column that has the rownames in the original dataframe default row_name
#' @param drop_col boolean. should we drop all other columns default TRUE
#' @param token_col_nm name of column that has tokens in return dataframe. Default 'token'
#' @param token_type name of column that has tokens in return dataframe. Default appends '_type' onto token_col_nm
#' @param tokenizer function that tokenzes the column. Default  tidytext::unnest_tokens
#' @param ... passed to tokenizer
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
                  row_name_nm = 'row_name',
                  #drop_col = TRUE,
                  #token_index = 'token_index',
                  #token_col_nm = 'token', #glue('{col_nm}_{tokens_suffix}'),
                  token_type = glue::glue('{col_nm}'),
                  tokenizer = tokenizer_basic#tidytext::unnest_tokens

){
  testit::assert(col_nm %in% colnames(dat))

  dat |>
    tibble::rownames_to_column(var = row_name_nm) |>
    dplyr::select(row_name_nm, col_nm) |>
    tokenizer(col_nm = col_nm, row_name_nm = row_name_nm, token_type = token_type, ...) |>
    dplyr::distinct() |>
    dplyr::mutate(token_type = token_type )
}




#' Tokenize a dataframe and multiple columns in the dataframe
#'
#'@param dat dataframe
#'@param ... passed to tokenize_col
#'@param col_nms vector of string. These strings are column names in dat to tokenize. Default None
#'@param token_type vector of strings. these are the type of tokens for each token column
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

  #print(names(list(...)))

  purrr::map2_dfr(col_nms, token_types, function(.x, .y){
    #print(glue::glue('.x={.x}, .y={.y}'))
    #print(list(...))
    #dat |> tokenize_col(col_nm = .x, token_type = .y,  row_name_nm = 'row_name', ...)
    dat |> tokenize_col(col_nm = .x, token_type = .y, ...)
  }) # |>
  #dplyr::bind_rows()
}


#' Takes a dataframe with columns from cols and counts the unique ocurrances, returns a dataframe with counts.
#'
#'
#' @param dat_token dataframe with columns in cols.
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
                        cols = c('token', 'token_type'),
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
#' this dataframe must have a column of name n_comparisonsand it will return a vector. which indicate the m_probs
#' This is the default calculator for m prob of tokens it can be overwritten by injecting a diferent function into the
#' m_prob_func parameter into any of tokenize_ations_m_u_prob, or token_links when called.
#' This function must take a dataframe and return a vector. The dataframe passed in will have columns token, token_type, n.x, n.y, n_comparisons, u_prob
#'
#'
#'@param dat_token_info a dataframe with information about the tokens
#'@param min_m_prob minimum value of m_prob returned
#'@param max_m_prob maximum value of m_prob returned
#'@param log_base Number. Base of the log. Default 10
#'@param ... is ignored
#'
#'@examples
#'
#'

t_dat <- token_links(
  dat_x = dat_ceo,
  dat_y = dat_alb,
  args_x = list(col_nms = 'coname'),
  args_y = list(col_nms = 'companyName'),
  token_types = 'company_name',
  token_index = '',
  suffix = c('ceo', 'alb')
)
t_dat$

#'
#'
#'
#'@export
calc_m_prob <- function(dat_token_info,
                        min_m_prob = 0.99,
                        max_m_prob = 0.999,
                        log_base = 10,
                        ...){
  x <-
    dat_token_info |> dplyr::select(n_comparisons) |>
    dplyr::mutate(n_comparisons_log = log(n_comparisons, base = log_base)) |>
    dplyr::mutate(n_comparisons_log = dplyr::if_else(n_comparisons_log == -Inf , 0, n_comparisons_log)) |>
    dplyr::pull(n_comparisons_log)


  #x <-dat_token_info$n_comparisons_log
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
#' t_dat <- token_links(
#'   dat_x = dat_ceo,
#'   dat_y = dat_alb,
#'   args_x = list(col_nms = 'coname'),
#'   args_y = list(col_nms = 'companyName'),
#'   token_types = 'company_name',
#'   token_index = '',
#'   suffix = c('ceo', 'alb')
#' )
#' generate_all_tokens(t_dat$x$token_counts, t_dat$y$token_counts, t_dat$total_comparisons)
#'
#'
#' @export
generate_all_tokens <- function(x_counts,
                               y_counts,
                               total_comparisons,
                               token_count_join = c('token','token_type'),
                               suffix = c('x','y'),
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
    dplyr::mutate(n_comparisons = !!dplyr::sym(n_nms_x)*!!dplyr::sym(n_nms_y)) |>
    dplyr::mutate(u_prob = (n_comparisons) / total_comparisons) |>
    dplyr::arrange(dplyr::desc(u_prob)) |>
    {\(.) dplyr::mutate(., m_prob = m_prob_func(.))}()
}


#' Joins two objects together that come back from the tokenize_ations function, joins the token counts together, and calculates the m and u probs for each token.
#'
#' @param x list returned from tokenize_ations
#' @param y list returned from tokenize_ations
#' @param suffix String vector of length 2, identifies which original dataframe a column in the result comes from. Default c('x', 'y')
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
                                   suffix = c('x', 'y'),
                                   #token_count_join = c("token", "token_type"),
                                   #m_prob_func = calc_m_prob,
                                   ...
                                   ){

  t_dat <- list(x = x, y = y )

  ########################
  # as some simple info to the return object
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


#' returns the required information about the joint probability of the tokens in one object
#'
#'@param dat_x dataframe
#'@param dat_y dataframe
#'@param col_nms_x vector of string column names to tokenize. Default all character columns
#'@param col_nms_y vector of string column names to tokenize. Default all character columns
#'@param args_x list of arguments passed to 'tokenize_ations'
#'@param args_y list of arguments passed to 'tokenize_ations'
#'@param row_name string that is the name of the column to get the rownames. Default 'row_name'
#'@param suffix vector of length two with suffixs to add to column names as needed
#'@param token_count_join vector of column names to join the token count dataframes on. Default c("token", "token_type")
#'@param m_prob_func a function that takes a dataframe with counts of tokens then returns a vector of m_probs
#'@param min_token_u_prob smaller numbers will eliminate more tokes to match on and mean things run quicker
#'@param ... arguments passed to 'tokenize_ations', note ... is  the lowest priority and all other passed first
#'
#'
#'@examples:
#'t_dat <- token_links(
#'  dat_x = readr::read_csv('https://tinyurl.com/2p8etjr6'),
#'  dat_y = readr::read_csv('https://tinyurl.com/2p8ap4ad' ),
#'  args_x = list(col_nms = 'coname'),
#'  args_y = list(col_nms = 'companyName'),
#'  token_types = 'company_name',
#'  token_index = '',
#'  suffix = c('ceo', 'alb')
#')
#'
#'
#'
#'@export
token_links <- function(dat_x, dat_y,
                            col_nms_x = dat_x |>
                              dplyr::select_if(is.character) |>
                              colnames(),
                            col_nms_y = dat_y |>
                              dplyr::select_if(is.character) |>
                              colnames(),
                            args_x = list(),
                            args_y = list(),
                        row_name_nm = 'row_name',
                        suffix = c('x', 'y'),
                        token_count_join = c("token", "token_type"),
                        m_prob_func = calc_m_prob,
                        #min_token_u_prob = 0.0000784,
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
#' @param row_name_nm String. rowname to use for each dataset. Default 'row_name'
#' @param suffix String vector of length 2. Default c('x', 'y')
#'
#' @examples
#' calculate_priori(
#'                  tibble(row_name.x = sample(1:1000, 100, replace=T),
#'                         row_name.y = sample(1:1000000, 100, replace=T)),
#'                  n_x = 100,
#'                  n_y = 1000000
#' )
#'
#' @export
calculate_priori <- function(x_y_rec_checks,
                             n_x,# = nrow(t_dat$x$dat)
                             n_y,# = nrow(t_dat$y$dat)
                             total_comparisons = as.double(n_x) * as.double(n_y),# = t_dat$total_comparisons,
                             row_name_nm = 'row_name',
                             suffix = c('x', 'y'),
                             ...
                             ){
  rep(1/total_comparisons,nrow(x_y_rec_checks))
}





#' Creates a subset of pairs to check in more detail.
#' @param t_dat t_dat object
#' @param min_posterior filter posterior results above this value. Default 0.2.
#' @param token_join_by vector column names that joins the tokens. Default c("token", "token_type")
#' @param tokens_to_keep NULL or dataframe with a list of tokens, and m_prob and u_prob for each token, in the case where it is NULL we use tokenized$tokens_all filtered by min_token_u_prob
#' @param min_token_u_prob a float. This is ignored if tokens_to_keep is not NULL.  Default 0.0000784.
#' @param return_all if TRUE it returns the whole object if FALSE it just returns the dataframe. Default TRUE
#' @param ... not used
#'
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
#') |> find_posterior_positive_evidance_only(return_all = FALSE)
#'
#'@export
find_posterior_positive_evidance_only <- function(t_dat,
                                                  min_posterior = 0.2,
                                                  token_join_by = c("token", "token_type"),
                                                  tokens_to_keep = NULL,
                                                  min_token_u_prob = 0.0000784,
                                                  return_all = TRUE,
                                                  priori_func = calculate_priori,
                                                  ...){

  suffix <-paste0(".",c(t_dat$x$suffix, t_dat$y$suffix))
  x_y_indexes <- c(t_dat$x$row_name_nm, t_dat$y$row_name_nm) |> paste0(suffix)

  print(glue::glue('Checking positive Evidance only. {t_dat$x$suffix}={nrow(t_dat$x$dat)}, {t_dat$y$suffix}={nrow(t_dat$y$dat)}, tokens used {nrow(t_dat$tokens_to_keep)}, expected comparison max = {sum(t_dat$tokens_to_keep$n_comparisons)}.'))
  tic <- Sys.time()


  #####################
  # Only keep useful tokens
  tokens_to_keep <-
    if (is.null(tokens_to_keep)){
        t_dat$tokens_all |>
        dplyr::filter(u_prob != 0) |>
        dplyr::filter(u_prob < min_token_u_prob)
    }else{tokens_to_keep}



  positive_evidance_only <-
    t_dat$x$tokens |>
    dplyr::inner_join(tokens_to_keep,
                      by = token_join_by,
                      suffix = suffix) |>
    dplyr::left_join(t_dat$y$tokens,
                     by = token_join_by,
                     suffix = suffix) |>
    #filter(row_name.x == '101') %>%
    dplyr::group_by_at(x_y_indexes) |>
    dplyr::summarise(u_prob_prod = prod(u_prob),
                     m_prob_prod = prod(m_prob),
                     n_tokens = dplyr::n(),
                     .groups = 'drop'
    ) |>
    {\(.) dplyr::mutate(., priori = priori_func(.,
                                      n_x = t_dat$x$dat,
                                      n_y = t_dat$y$dat,
                                      total_comparisons = as.double(n_x) * as.double(n_y),# = t_dat$total_comparisons,
                                      suffix = suffix,
                                      row_name_nm = c(t_dat$x$row_name_nm, t_dat$y$row_name_nm)
                                      ))}() |>
    dplyr::mutate(m_prob_prod_lambda = m_prob_prod * priori) |>
    #dplyr::mutate(m_prob_prod_lambda = m_prob_prod * t_dat$lambda) |>
    dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-priori)) |>
    dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
    #dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + (1- t_dat$lambda) * u_prob_prod) )  ) |>
    #t_dat$positive_evidance_only |>
    dplyr::select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |>
    dplyr::filter(posterior >= min_posterior) |>
    dplyr::distinct()


  toc <- Sys.time()
  print(glue::glue('found {nrow(t_dat$positive_evidance_only)} records in {round(difftime(toc,tic, units = "mins"),1)} minutes'))




  if (return_all){
    t_dat$posterior_threshold_positive_only <- min_posterior
    t_dat$min_token_u_prob <- min_token_u_prob
    t_dat$positive_evidance_only  <- positive_evidance_only
    return(t_dat)
  }else{
    return(positive_evidance_only)
  }
}







#' t_dat should have been run through find_posterior_positive_evidance_only first to create the list of candidates
#'
#'@param t_dat t_dat object
#'@param min_posterior filter results below this threshold Default = 0.01
#'@param token_join__by  what to join the all_tokens by. Default = c("token", "token_type"),
#'@param positive_evidance_only, NULL or a dataframe with at least two columns indicating the row_names to be checked in eash of the datasets
#'@param return_all if TRUE it returns the whole object if FALSE it just returns the dataframe. Default TRUE
#'@param ... Not used
#'
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
#') |> find_posterior_positive_evidance_only() |>
#' find_posterior_all_evidance(return_all = FALSE)
#'

#'
#'@export
find_posterior_all_evidance <- function(t_dat,
                                        min_posterior = 0.01,
                                        token_join_by = c("token", "token_type"),
                                        positive_evidance_only = NULL,
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


  positive_evidance_only <-
    if (is.null(positive_evidance_only)){
      t_dat$positive_evidance_only
    }else{positive_evidance_only}

  all_evidance <-
    dplyr::bind_rows(
      dplyr::inner_join(
        t_dat$x$tokens |> dplyr::rename(!!rlang::sym(x_indexes) := x_row_nm), # rename_all(~{paste0(.x, suffix0[[1]])}),
        positive_evidance_only |> dplyr::distinct_at(x_y_indexes),
        by = x_indexes
      ),
      dplyr::inner_join(
        t_dat$y$tokens |> dplyr::rename(!!rlang::sym(y_indexes) := y_row_nm), #rename_all(~{paste0(.x, suffix0[[2]])}),
        positive_evidance_only |> dplyr::distinct_at(x_y_indexes),
        by = y_indexes
    )) |>
    dplyr::group_by_at(c(x_y_indexes, token_join_by)) |>
    dplyr::summarise(n_dat_set = dplyr::n(), .groups = 'drop') |>
    dplyr::mutate(evidance_in_favour = n_dat_set == 2) |>
    #filter(row_name.EDGAR == '11991' & row_name.ALB == '2') |>
    dplyr::inner_join(t_dat$tokens_all,
                      by = token_join_by) |>
    dplyr::mutate(m_prob = dplyr::if_else(evidance_in_favour , m_prob, 1-m_prob)) |>
    dplyr::mutate(u_prob = dplyr::if_else(evidance_in_favour , u_prob, 1-u_prob)) |>
    dplyr::group_by_at(x_y_indexes) |>
    dplyr::summarise(u_prob_prod = prod(u_prob),
                     m_prob_prod = prod(m_prob),
                     #n_tokens = dplyr::n(),
                     tokens_in_favour = sum(evidance_in_favour),
                     tokens_against = sum(!evidance_in_favour),
                     .groups = 'drop'
    ) |>
    {\(.) dplyr::mutate(., priori = priori_func(.,
                                                n_x = t_dat$x$dat,
                                                n_y = t_dat$y$dat,
                                                total_comparisons = as.double(n_x) * as.double(n_y),# = t_dat$total_comparisons,
                                                suffix = suffix,
                                                row_name_nm = c(t_dat$x$row_name_nm, t_dat$y$row_name_nm)
    ))}() |>

    dplyr::mutate(m_prob_prod_lambda = m_prob_prod * priori) |>
    dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-priori)) |>
    dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
    dplyr::select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |>
    dplyr::filter(posterior > min_posterior) |>
    #dplyr::arrange(posterior)
    dplyr::arrange(dplyr::desc(posterior))



  if (return_all){
    t_dat$posterior_threshold_all_evidance <- min_posterior
    t_dat$all_evidance  <- all_evidance
    return(t_dat)
  }else{
    return(all_evidance)
  }
}



#' appends dataframes with posteriors and returns it
#'@param t_dat tdat object
#'@param min_posterior_all_evidance passed to find_posterior_all_evidance as min_posterior. Default 0.01
#'@param min_posterior_positive_evidance_only passed to find_posterior_all_evidance as min_posterior. Default 0.3
#'@param ... passed wholsale to both find_posterior_positive_evidance_only, and find_posterior_all_evidance
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
#') |> find_posterior()
#'
#'
#'@export
find_posterior <- function(t_dat,
                       min_posterior_all_evidance = 0.01,
                       min_posterior_positive_evidance_only = 0.3,
                       ...){


  t_dat <- find_posterior_positive_evidance_only(t_dat,
                                                 min_posterior = min_posterior_positive_evidance_only,
                                                 return_all  = TRUE,
                                                 ...)
  t_dat <- find_posterior_all_evidance(t_dat,
                                       min_posterior = min_posterior_all_evidance,
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
#'
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
#' ) |> find_posterior_positive_evidance_only() |>
#' find_posterior_all_evidance() |>
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
  # get evidance to use
  evidance_to_use <-
    if (pairs_to_get == 'all_evidence'){t_dat$all_evidance
    }else if (pairs_to_get == 'positive_evidence'){t_dat$all_evidance
    }else if (is.data.frame(pairs_to_get)){pairs_to_get
    }else{
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



  evidance_to_use |>
    dplyr::select(dplyr::all_of(c(x_indexes,y_indexes,link_col_nms))) |>
    dplyr::left_join(
      t_dat$x$dat |>
        dplyr::select(dplyr::all_of(x_cols)) |>
        tibble::rownames_to_column(var = t_dat$x$row_name_nm) |>
        dplyr::rename_all(\(x) paste0(x,'.', t_dat$x$suffix)),
      by = x_indexes) |>
    dplyr::left_join(
      t_dat$y$dat |>
        dplyr::select(dplyr::all_of(y_cols )) |>
        tibble::rownames_to_column(var = t_dat$y$row_name_nm) |>
        dplyr::rename_all(\(x) paste0(x,'.', t_dat$y$suffix)),
      by = y_indexes) |>
    {\(.) if (include_row_numbers) . else dplyr::select(., -dplyr::any_of(c(x_y_indexes))   ) }()
}



#
# dat_ceo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')
# dat_alb <- readr::read_csv('https://open.alberta.ca/dataset/a2b1fc9b-aac4-4718-8645-b0466ca5ec57/resource/3da9a7f9-bd34-48c0-841f-19c856b551ad/download/foodindustry.csv')
# dat_edgar <- readr::read_csv(file.path('data', 'sec__edgar_company_info.csv')) |> janitor::clean_names()
# getwd()
#
#
#
# t_dat <- token_links(
#   dat_alb,
#   dat_edgar,
#   args_x = list(col_nms = 'companyName'),
#   args_y = list(col_nms = 'company_name'),
#   token_type = 'company_name',#, min_token_u_prob = 0.2
#   token_index = '',
#   suffix = c('ALB', 'EDGAR')
# )
#
# x <- find_posterior_positive_evidance_only(t_dat, return_all = FALSE )
#
# t_dat <- find_posterior(t_dat, min_posterior_positive_evidance_only = 0.5)
# a <- joined_results(t_dat, link_col_nms = c('posterior', 'tokens_in_favour', 'tokens_against'))
#
#
#
# joined_results(t_dat,pairs_to_get = x, link_col_nms = c())
#
# t_dat$all_evidance
#
# q <-
#
# t_dat$x$col_nms
#
#
# row_name_x
# row_name_X
# t_dat$x$dat
# tic = Sys.time()
# toc = Sys.time()
# print(toc-tic)




#
#
# library(sqldf)
#
# possible_match_positive_matches <-
#   keep_all_evidance |>
#   distinct_at(c('row_name.x', 'row_name.y'))  |>
#   filter(!is.na(row_name.x) & !is.na(row_name.y))
#
#
# t_dat$tokens_all
# full_join_dat <-
#   full_join(
#     t_dat$x$tokens,
#     t_dat$y$tokens,
#     by = c("token", "token_type"),
#     suffix = c('_x','_y')
#   )
#
# full_join_data <-
#   full_join(
#   t_dat$x$tokens %>%
#     left_join(t_dat$tokens_all, by = c("token", "token_type")) |>
#     rename(row_name_x = row_name) #|>
#     #filter(row_name_x == '101')
#   ,
#   t_dat$y$tokens |>
#     left_join(t_dat$tokens_all, by = c("token", "token_type"))|>
#     rename(row_name_y = row_name) #|>
#     #filter(row_name_y == '212')
#   ) |>
#   filter(row_name_x == '101') |>
#   filter(row_name_y == '212')
#
#
# t_dat$y$tokens |>
#   left_join(t_dat$tokens_all, by = c("token", "token_type"))|>
#   rename(row_name_y = row_name) |>
#   anti_join(full_join_data)
#
#
#
#
#   bind_rows(  t_dat$y$tokens |>
#                 left_join(t_dat$tokens_all, by = c("token", "token_type"))|>
#                 rename(row_name_y = row_name),
#               t_dat$x$tokens %>%
#                 left_join(t_dat$tokens_all, by = c("token", "token_type")) |>
#                 rename(row_name_x = row_name)
#               ) %>%
#     mutate(evidance_in_favour = !is.na(row_name_x) & ! is.na(row_name_y)) |>
#     mutate(m_prob = dplyr::if_else(evidance_in_favour , m_prob, 1-m_prob)) |>
#     mutate(u_prob = dplyr::if_else(evidance_in_favour , u_prob, 1-u_prob))
#
#
#
# full_tokens <-
# bind_rows(
# t_dat$x$tokens %>%
#   left_join(t_dat$tokens_all, by = c("token", "token_type")) |>
#   rename(row_name_x = row_name),
# t_dat$y$tokens |>
#   left_join(t_dat$tokens_all, by = c("token", "token_type"))|>
#   rename(row_name_y = row_name)
# ) |> distinct()
#
# keep_all_evidance %>%
#   distinct_at(c('row_name.x', 'row_name.y')) |>
#   head(1) |>
#   pmap_dfr(function(row_name.x, row_name.y){
#     full_join_data %>% filter((row_name_x == row_name.x & row_name_y == row_name.y) |
#                               (row_name_x == row_name.x & is.na(row_name_y)) |
#                               (is.na(row_name_x) & row_name_y == row_name.y)) %>%
#       mutate(row_name_x = dplyr::if_else(is.na(row_name_x), row_name.x, row_name_x)) |>
#       mutate(row_name_y = dplyr::if_else(is.na(row_name_y), row_name.y, row_name_y)) |>
#       arrange(desc(evidance_in_favour)) %>%
#       distinct_at(c("token", "token_type"), .keep_all = TRUE)
#   })
#
#
#  full_join_data %>% filter((row_name_x == '101' & row_name_y == '212') |
#                             (row_name_x == '101' & is.na(row_name_y)) |
#                             (is.na(row_name_x) & row_name_y == '212')) %>%
#   mutate(row_name_x = dplyr::if_else(is.na(row_name_x), row_name.x, row_name_x)) |>
#   mutate(row_name_y = dplyr::if_else(is.na(row_name_y), row_name.y, row_name_y)) |>
#   arrange(desc(evidance_in_favour)) %>%
#   distinct_at(c("token", "token_type"), .keep_all = TRUE)
#
# |>
#     dplyr::group_by_at(c('row_name_x','row_name_y')) |>
#     dplyr::summarise(u_prob_prod = prod(u_prob),
#                      m_prob_prod = prod(m_prob),
#                      n = dplyr::n(),
#                      .groups = 'drop'
#     ) |>
#     dplyr::mutate(m_prob_prod_lambda = m_prob_prod * t_dat$lambda) |>
#     dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + (1- t_dat$lambda) * u_prob_prod) )  )
#
# postieriors
#
#
#
#
#   dplyr::filter(posterior > min_posterior_postive_threshold)
#
#
# full_join_dat %>% distinct(row_name.x)
# full_join_dat |>
#   filter((row_name_x == '101' & row_name_y == '212') |
#            (is.na(row_name_x) & row_name_y == '212') |
#            (row_name_x == '101' & is.na(row_name_y))
#   ) |>
#   bind_rows(t_dat$y$tokens |> rename(row_name_y = row_name)) |>
#  #bind_rows(t_dat$x$tokens %>% rename(row_name_x = row_name)) |>
#  #bind_rows(t_dat$y$tokens %>% rename(row_name_y = row_name)) |>
#   #filter(row_name_y == '212' & token == 'green')
#   filter((row_name_x == '101' & row_name_y == '212') |
#          (is.na(row_name_x) & row_name_y == '212') |
#          (row_name_x == '101' & is.na(row_name_y))
#         ) |>
#   distinct() |>
#   left_join(t_dat$tokens_all, by = c("token", "token_type")) |>
#   mutate(m_prob = )
#
#
#   left_join(possible_match_positive_matches, by = c('row_name_x' = 'row_name.x')) |>
#   left_join(possible_match_positive_matches, by = c('row_name_y' = 'row_name.y')) |>
#
#   filter(!is.na(row_name.y))
#   mutate(evidance_in_favour = !is.na(row_name_x) & ! is.na(row_name_y)) |>
#   mutate(row_name_x = dplyr::if_else(is.na(row_name_x), row_name.x, row_name_x)) |>
#   mutate(row_name_y = dplyr::if_else(is.na(row_name_y), row_name.y, row_name_y))
#
#
#
# b
# t_dat$total_comparisons
# t_dat$lambda
# b
# a$tokens_to_keep
# a$tokens_all
# x = list(a=1,b=2)
#
# q <- map2(names(x), x, function(nm, val){print(val)})
# a(b=1)$b
# a()
#
#
#
#
# tokenize_ations(dat, col_nms = 'exec_fullname')
#
# tokenize_col(dat, col_nm = 'exec_fullname', drop_col = TRUE)
#
# tokenize_df(dat , col_nms = c('coname', 'exec_fullname'), token_type = c('a','b')) %>% count(token_type)
# tokenize_df(dat , col_nms = c('coname', 'exec_fullname'))
#
# dat_tokens <- tokenize_df(dat , col_nms = c('coname', 'exec_fullname'), token_type = c('a','b'))
