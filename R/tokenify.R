



library(tidytext)
library(dplyr)
library(tibble)
library(glue)
library(purrr)
library(testthat)
library(testit)
library(janitor)

dat <- read.csv('C:/Users/hswerdfe/Downloads/labour-sponsored-investment-funds.csv')|>  tibble() |>
  janitor::clean_names()
dat <- read.csv('C:/Users/hswerdfe/Downloads/community-small-business-investment-funds_3.csv')|>  tibble() |>
  janitor::clean_names()
dat <- read.csv('C:/Users/hswerdfe/Downloads/community-small-business-investment-funds_4.CSV') |>  tibble() |>
  janitor::clean_names()




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
#' tokenize_col(dat)
#' tokenize_col(dat, col_nm = 'companyName')
tokenize_col <- function(dat,
                  col_nm,
                  row_name_nm,
                  drop_col = TRUE,
                  token_index = 'token_index',
                  token_col_nm = 'token', #glue('{col_nm}_{tokens_suffix}'),
                  token_type = glue('{col_nm}'),
                  tokenizer = tidytext::unnest_tokens,
                  ...
){
  testit::assert(col_nm %in% colnames(dat))




  dat |>
    tibble::rownames_to_column(var = row_name_nm) |>
    dplyr::select(row_name_nm, col_nm) |>
    tokenizer(output = !!dplyr::sym(token_col_nm), input = col_nm, drop = drop_col , ...) |>
    {\(.) if (drop_col) select(., -col_nm) else .}() |>
    {\(.) if (nchar(token_index) > 0) dplyr::group_by_at(., row_name_nm) |> dplyr::mutate(!!sym(token_index) := dplyr::row_number()) else .}() |>
    dplyr::distinct() |>
    dplyr::mutate(token_type = token_type )
}

#' Tokenize a dataframe and multiple columns in the dataframe
#'
#'@param dat dataframe
#'@param col_nms vector of string. These strings are column names in dat to tokenize. Default None
#'@param token_type vector of strings. these are the type of tokens for each token column
#'@param ... passed to tokenize_col
#'
tokenize_df <- function(dat,
                        col_nms,
                        token_types = col_nms,
                        ...
                        ){
  testit::assert(length(col_nms) == length(token_types))
  testit::assert(typeof(col_nms) == typeof(token_types))
  testit::assert(is.character(col_nms))

  purrr::map2(col_nms, token_types, function(.x, .y){
    tokenize_col(dat, col_nm = .x, token_type = .y, ...)
  }) |>
  dplyr::bind_rows()
}


#' takes a dataframe with columns from cols and counts the unique ocurrances
#' @param dat_token dataframe with columns in cols.
#' @param cols vector of column names. Default token, token_type
#' @param .groups passed to summarize. Default 'drop'
#' @param ... not used
token_count <- function(dat_tokens, cols = c('token', 'token_type'), .groups = 'drop', ... ){
  dat_tokens |>
    dplyr::group_by_at(cols) |>
    dplyr::summarise(n = dplyr::n(), .groups = .groups) |>
    dplyr::arrange(dplyr::desc(n))
}

#' takes a dataframe and tokenizes the columns indicated and then counts the tokens, and returns a list of two dataframes
#'
#'@param dat a dataframe
#'@param col_nms vector of column names to be tokenized
#'@param ... passed to tokenize_df
tokenize_ations <- function(dat,
                            col_nms,
                            ...){

  dat_tokens <-
    dat |>
      tokenize_df(col_nms, ...)

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





#' calculates what m_prob should be, takes in a dataframe and returns a vector of same length
#'
#'@param dat_token_info a dataframe with information about the tokens
#'@param min_m_prob minimum value of m_prob returned
#'@param max_m_prob maximum value of m_prob returned
#'@param ... is ignored
#'
calc_m_prob <- function(dat_token_info,
                        min_m_prob = 0.99,
                        max_m_prob = 0.999,
                        log_base = 10,
                        ...){
  x <-
    dat_token_info |>
    dplyr::mutate(n_comparisons_log = log(n_comparisons, base = log_base)) |>
    dplyr::mutate(n_comparisons_log = if_else(n_comparisons_log == -Inf , 0, n_comparisons_log)) |>
    dplyr::pull(n_comparisons_log)


  #x <-dat_token_info$n_comparisons_log
  rng <- max(x)-min(x)

  if (rng <=0)
    return (mean(c(max_m_prob, min_m_prob)))

  (1+(min(x)-x)/rng) * (max_m_prob - min_m_prob) + min_m_prob
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

  if (!'col_nms' %in% names(args_x) )
    args_x$col_nms <- col_nms_x
  if (!'dat' %in% names(args_x) )
    args_x$dat <- dat_x
  if (!'row_name_nm' %in% names(args_x) )
    args_x[['row_name_nm']] <- row_name_nm
  # if (!'suffix' %in% names(args_x) )
  #   args_x[['suffix']] <- row_name_nm




  if (!'col_nms' %in% names(args_y) )
    args_y$col_nms <- col_nms_y
  if (!'dat' %in% names(args_y) )
    args_y$dat <- dat_y
  if (!'row_name_nm' %in% names(args_y) )
    args_y[['row_name_nm']] <- row_name_nm





  common_args <- list(...)
  map2(names(common_args), common_args, function(nm, val){
    if (!nm %in% names(args_x) )
      args_x[[nm]] <<- val
    if (!nm %in% names(args_y) )
      args_y[[nm]] <<- val
  })


  #######################
  # Tokenize the x and y dataframes seperately
  tokenized <-
    list(x = do.call(tokenize_ations, args_x),
         y = do.call(tokenize_ations, args_y)
    )

  ########################
  # as some simple info to the return object
  tokenized$x$suffix = suffix[[1]]
  tokenized$y$suffix = suffix[[2]]
  tokenized$total_comparisons <- nrow(dat_x) * nrow(dat_y)
  tokenized$lambda <- 1 /tokenized$total_comparisons


  n_nms <- paste0('n.', suffix)
  n_nms_x <- n_nms[[1]]
  n_nms_y <- n_nms[[2]]
  #########################
  # calculate the m and u prob
  tokenized$tokens_all <-
    #dat_token_info <-
    dplyr::full_join(x = tokenized$x$token_counts,
                     y = tokenized$y$token_counts,
                     by = token_count_join,
                     suffix = paste0('.' ,suffix)
                    ) |>
    dplyr::mutate(!!sym(n_nms_x) := replace(!!sym(n_nms_x), is.na(!!sym(n_nms_x)), 0)) |>
    dplyr::mutate(!!sym(n_nms_y) := replace(!!sym(n_nms_y), is.na(!!sym(n_nms_y)), 0)) |>
    dplyr::mutate(n_comparisons = !!sym(n_nms_x)*!!sym(n_nms_y)) |>
    # dplyr::mutate(n_comparisons_log = log(n_comparisons, base = 10)) |>
    # dplyr::mutate(n_comparisons_log = if_else(n_comparisons_log == -Inf , 0, n_comparisons_log)) |>
    dplyr::mutate(u_prob = (n_comparisons) / tokenized$total_comparisons) |>
    dplyr::arrange(dplyr::desc(u_prob)) |>
    {\(.) dplyr::mutate(., m_prob = m_prob_func(.))}()


  ####################
  # Only keep useful tokens
  # tokenized$tokens_to_keep <-
  #   tokenized$tokens_all |>
  #   dplyr::filter(u_prob != 0) |>
  #   dplyr::filter(u_prob < min_token_u_prob)


  tokenized
}

#' creates a subset of pairs to check in more detail.
#'@param t_dat t_dat object
#'@param min_posterior filter posterior results above this value. Default 0.2.
#'@param token_join_by vector column names that joins the tokens. Default c("token", "token_type")
#'@param tokens_to_keep NULL or dataframe with a list of tokens, and m_prob and u_prob for each token, in the case where it is NULL we use tokenized$tokens_all filtered by min_token_u_prob
#'@param min_token_u_prob a float. This is ignored if tokens_to_keep is not NULL.  Default 0.0000784.
#'@param return_all if TRUE it returns the whole object if FALSE it just returns the dataframe. Default TRUE
#'@param ... not used
#'
find_posterior_positive_evidance_only <- function(t_dat,
                                                  min_posterior = 0.2,
                                                  token_join_by = c("token", "token_type"),
                                                  tokens_to_keep = NULL,
                                                  min_token_u_prob = 0.0000784,
                                                  return_all = TRUE,
                                                  ...){

  suffix <-paste0(".",c(t_dat$x$suffix, t_dat$y$suffix))
  x_y_indexes <- c(t_dat$x$row_name_nm, t_dat$y$row_name_nm) |> paste0(suffix)

  print(glue::glue('Checking positive Evidance only. {t_dat$x$suffix}={nrow(t_dat$x$dat)}, {t_dat$y$suffix}={nrow(t_dat$y$dat)}, tokens used {nrow(t_dat$tokens_to_keep)}, expected comparison max = {sum(t_dat$tokens_to_keep$n_comparisons)}.'))
  tic <- Sys.time()



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
                     n_tokens = n(),
                     .groups = 'drop'
    ) |>
    dplyr::mutate(m_prob_prod_lambda = m_prob_prod * t_dat$lambda) |>
    dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-t_dat$lambda)) |>
    dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
    #dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + (1- t_dat$lambda) * u_prob_prod) )  ) |>
    #t_dat$positive_evidance_only |>
    select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |>
    dplyr::filter(posterior >= min_posterior) |>
    distinct()


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
find_posterior_all_evidance <- function(t_dat,
                                        min_posterior = 0.01,
                                        token_join_by = c("token", "token_type"),
                                        positive_evidance_only = NULL,
                                        return_all = TRUE,
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
    bind_rows(
      inner_join(
        t_dat$x$tokens |> rename(!!sym(x_indexes) := x_row_nm), # rename_all(~{paste0(.x, suffix0[[1]])}),
        positive_evidance_only |> distinct_at(x_y_indexes),
        by = x_indexes
      ),
      inner_join(
        t_dat$y$tokens |> rename(!!sym(y_indexes) := y_row_nm), #rename_all(~{paste0(.x, suffix0[[2]])}),
        positive_evidance_only |> distinct_at(x_y_indexes),
        by = y_indexes
    )) |>
    group_by_at(c(x_y_indexes, token_join_by)) |>
    summarise(n_dat_set = n(), .groups = 'drop') |>
    mutate(evidance_in_favour = n_dat_set == 2) |>
    #filter(row_name.EDGAR == '11991' & row_name.ALB == '2') |>
    dplyr::inner_join(t_dat$tokens_all,
                      by = token_join_by) |>
    mutate(m_prob = if_else(evidance_in_favour , m_prob, 1-m_prob)) |>
    mutate(u_prob = if_else(evidance_in_favour , u_prob, 1-u_prob)) |>
    dplyr::group_by_at(x_y_indexes) |>
    dplyr::summarise(u_prob_prod = prod(u_prob),
                     m_prob_prod = prod(m_prob),
                     #n_tokens = n(),
                     tokens_in_favour = sum(evidance_in_favour),
                     tokens_against = sum(!evidance_in_favour),
                     .groups = 'drop'
    ) |>

    dplyr::mutate(m_prob_prod_lambda = m_prob_prod * t_dat$lambda) |>
    dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-t_dat$lambda)) |>
    dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
    select(-u_prob_prod, -m_prob_prod, -m_prob_prod_lambda, -u_prob_prod_one_lambda) |>
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



#'
#'
#'
#'
#'
#'
# find_posterior_all_evidance_OLD <- function(t_dat,
#                                         #min_posterior = 0.01,
#                                         token_join_by = c("token", "token_type"),
#                                         sample_pairs = NULL){
#
#   suffix0 <-paste0(".",c(t_dat$x$suffix, t_dat$y$suffix))
#   suffix1 <-paste0("_",c(t_dat$x$suffix, t_dat$y$suffix))
#   x_y_indexes <- c(t_dat$x$row_name_nm, t_dat$y$row_name_nm) |> paste0(suffix0)
#   x_y_indexes1 <- c(t_dat$x$row_name_nm, t_dat$y$row_name_nm) |> paste0(suffix1)
#   x_indexes <- x_y_indexes[[1]]
#   y_indexes <- x_y_indexes[[2]]
#   x_indexes1 <- x_y_indexes1[[1]]
#   y_indexes1 <- x_y_indexes1[[2]]
#
#
#   print(glue::glue('Checking all evidance refining {nrow(t_dat$positive_evidance_only)} possibilities. With {nrow(t_dat$tokens_all)} tokens.'))
#
#   i = 1
#
#   tic <- Sys.time()
#   t_dat$all_evidance <-
#     t_dat$positive_evidance_only |>
#     distinct_at(x_y_indexes) |>
#     #filter(row_name.tt == '170' & row_name.alb == '133') |>
#     {\(.) if (is.null(sample_pairs)) . else sample_n(., sample_pairs)}() |>
#     {\(.) map2_dfr(.x = .[[x_indexes]],
#                    .y = .[[y_indexes]],
#                    ~{
#
#                      cat(glue::glue('{i}, '))
#
#                      i <<- i + 1
#                      #.x = '101' #.x = '1'
#                      #.y = '212  #.y = '100881'
#                      full_join(
#                        t_dat$x$tokens |> filter(!!sym(t_dat$x$row_name_nm) == .x),
#                        t_dat$y$tokens |> filter(!!sym(t_dat$y$row_name_nm) == .y),
#                        by = token_join_by,
#                        suffix = suffix1
#                      ) |>
#                        mutate(evidance_in_favour = !is.na(!!sym(x_indexes1)) & ! is.na(!!sym(y_indexes1))) |>
#                        mutate(!!sym(x_indexes1) := if_else(is.na(!!sym(x_indexes1)), .x, !!sym(x_indexes1))) |>
#                        mutate(!!sym(y_indexes1) := if_else(is.na(!!sym(y_indexes1)), .y, !!sym(y_indexes1)))
#                    })}() |>
#     dplyr::inner_join(t_dat$tokens_all,
#                       by = token_join_by) |>
#     mutate(m_prob = if_else(evidance_in_favour , m_prob, 1-m_prob)) |>
#     mutate(u_prob = if_else(evidance_in_favour , u_prob, 1-u_prob)) |>
#     dplyr::group_by_at(x_y_indexes1) |>
#     dplyr::summarise(u_prob_prod = prod(u_prob),
#                      m_prob_prod = prod(m_prob),
#                      n = n(),
#                      tokens_in_favour = sum(evidance_in_favour),
#                      tokens_against = sum(!evidance_in_favour),
#                      .groups = 'drop'
#     ) |>
#     dplyr::mutate(m_prob_prod_lambda = m_prob_prod * t_dat$lambda) |>
#     dplyr::mutate(u_prob_prod_one_lambda = u_prob_prod * (1-t_dat$lambda)) |>
#     dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + u_prob_prod_one_lambda) )  ) |>
#     dplyr::rename(!!sym(x_indexes) := x_indexes1, !!sym(y_indexes) := y_indexes1) |>
#     dplyr::filter(posterior > min_posterior)
#
#
#   toc <- Sys.time()
#
#
#   t_dat$posterior_all_evidance <- min_posterior
#   print(glue::glue('found {nrow(t_dat$all_evidance)} matches with posterior > {min_posterior} took {round(difftime(toc, tic, units = "mins"),1)} min'))
#   t_dat
# }


#' appends dataframes with posteriors and returns it
#'@param t_dat tdat object
#'@param min_posterior_all_evidance passed to find_posterior_all_evidance as min_posterior. Default 0.01
#'@param min_posterior_positive_evidance_only passed to find_posterior_all_evidance as min_posterior. Default 0.3
#'@param ... passed wholsale to both find_posterior_positive_evidance_only, and find_posterior_all_evidance
#'
#'
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
#'@param t_dat a t_dat object
#'@param include_row_numbers Boolean Do we include the row names. Default False
#'@param pairs_to_get either 'all_evidence' or 'positive_evidence' or a dataframe. Default 'all_evidence'
#'@param link_col_nms vector of column names to includ from the joining dataframe. Default 'Posterior'
#'@param orig_data_to_include either 'matched' or 'all' and determines what is returned from the x and y dataframes. Default 0.3
#'
#'
joined_results <- function(t_dat,
                           include_row_numbers = FALSE,
                           pairs_to_get = 'all_evidence',#'positive_evidence'
                           link_col_nms = c('posterior'),
                           orig_data_to_include  = 'matched',#,'all'
                           #min_posterior = 0.3,
                           ...
                           ){


  ##############
  #
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
    select(dplyr::all_of(c(x_indexes,y_indexes,link_col_nms))) |>
    left_join(
      t_dat$x$dat |>
        select(dplyr::all_of(x_cols)) |>
        tibble::rownames_to_column(var = t_dat$x$row_name_nm) |>
        rename_all(\(x) paste0(x,'.', t_dat$x$suffix)),
      by = x_indexes) |>
    left_join(
      t_dat$y$dat |>
        select(dplyr::all_of(y_cols )) |>
        tibble::rownames_to_column(var = t_dat$y$row_name_nm) |>
        rename_all(\(x) paste0(x,'.', t_dat$y$suffix)),
      by = y_indexes) |>
    {\(.) if (include_row_numbers) . else select(., -any_of(c(x_y_indexes))   ) }()
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






library(sqldf)

possible_match_positive_matches <-
  keep_all_evidance |>
  distinct_at(c('row_name.x', 'row_name.y'))  |>
  filter(!is.na(row_name.x) & !is.na(row_name.y))


t_dat$tokens_all
full_join_dat <-
  full_join(
    t_dat$x$tokens,
    t_dat$y$tokens,
    by = c("token", "token_type"),
    suffix = c('_x','_y')
  )

full_join_data <-
  full_join(
  t_dat$x$tokens %>%
    left_join(t_dat$tokens_all, by = c("token", "token_type")) |>
    rename(row_name_x = row_name) #|>
    #filter(row_name_x == '101')
  ,
  t_dat$y$tokens |>
    left_join(t_dat$tokens_all, by = c("token", "token_type"))|>
    rename(row_name_y = row_name) #|>
    #filter(row_name_y == '212')
  ) |>
  filter(row_name_x == '101') |>
  filter(row_name_y == '212')


t_dat$y$tokens |>
  left_join(t_dat$tokens_all, by = c("token", "token_type"))|>
  rename(row_name_y = row_name) |>
  anti_join(full_join_data)




  bind_rows(  t_dat$y$tokens |>
                left_join(t_dat$tokens_all, by = c("token", "token_type"))|>
                rename(row_name_y = row_name),
              t_dat$x$tokens %>%
                left_join(t_dat$tokens_all, by = c("token", "token_type")) |>
                rename(row_name_x = row_name)
              ) %>%
    mutate(evidance_in_favour = !is.na(row_name_x) & ! is.na(row_name_y)) |>
    mutate(m_prob = if_else(evidance_in_favour , m_prob, 1-m_prob)) |>
    mutate(u_prob = if_else(evidance_in_favour , u_prob, 1-u_prob))



full_tokens <-
bind_rows(
t_dat$x$tokens %>%
  left_join(t_dat$tokens_all, by = c("token", "token_type")) |>
  rename(row_name_x = row_name),
t_dat$y$tokens |>
  left_join(t_dat$tokens_all, by = c("token", "token_type"))|>
  rename(row_name_y = row_name)
) |> distinct()

keep_all_evidance %>%
  distinct_at(c('row_name.x', 'row_name.y')) |>
  head(1) |>
  pmap_dfr(function(row_name.x, row_name.y){
    full_join_data %>% filter((row_name_x == row_name.x & row_name_y == row_name.y) |
                              (row_name_x == row_name.x & is.na(row_name_y)) |
                              (is.na(row_name_x) & row_name_y == row_name.y)) %>%
      mutate(row_name_x = if_else(is.na(row_name_x), row_name.x, row_name_x)) |>
      mutate(row_name_y = if_else(is.na(row_name_y), row_name.y, row_name_y)) |>
      arrange(desc(evidance_in_favour)) %>%
      distinct_at(c("token", "token_type"), .keep_all = TRUE)
  })


 full_join_data %>% filter((row_name_x == '101' & row_name_y == '212') |
                            (row_name_x == '101' & is.na(row_name_y)) |
                            (is.na(row_name_x) & row_name_y == '212')) %>%
  mutate(row_name_x = if_else(is.na(row_name_x), row_name.x, row_name_x)) |>
  mutate(row_name_y = if_else(is.na(row_name_y), row_name.y, row_name_y)) |>
  arrange(desc(evidance_in_favour)) %>%
  distinct_at(c("token", "token_type"), .keep_all = TRUE)

|>
    dplyr::group_by_at(c('row_name_x','row_name_y')) |>
    dplyr::summarise(u_prob_prod = prod(u_prob),
                     m_prob_prod = prod(m_prob),
                     n = n(),
                     .groups = 'drop'
    ) |>
    dplyr::mutate(m_prob_prod_lambda = m_prob_prod * t_dat$lambda) |>
    dplyr::mutate(posterior = (m_prob_prod_lambda / (m_prob_prod_lambda + (1- t_dat$lambda) * u_prob_prod) )  )

postieriors




  dplyr::filter(posterior > min_posterior_postive_threshold)


full_join_dat %>% distinct(row_name.x)
full_join_dat |>
  filter((row_name_x == '101' & row_name_y == '212') |
           (is.na(row_name_x) & row_name_y == '212') |
           (row_name_x == '101' & is.na(row_name_y))
  ) |>
  bind_rows(t_dat$y$tokens |> rename(row_name_y = row_name)) |>
 #bind_rows(t_dat$x$tokens %>% rename(row_name_x = row_name)) |>
 #bind_rows(t_dat$y$tokens %>% rename(row_name_y = row_name)) |>
  #filter(row_name_y == '212' & token == 'green')
  filter((row_name_x == '101' & row_name_y == '212') |
         (is.na(row_name_x) & row_name_y == '212') |
         (row_name_x == '101' & is.na(row_name_y))
        ) |>
  distinct() |>
  left_join(t_dat$tokens_all, by = c("token", "token_type")) |>
  mutate(m_prob = )


  left_join(possible_match_positive_matches, by = c('row_name_x' = 'row_name.x')) |>
  left_join(possible_match_positive_matches, by = c('row_name_y' = 'row_name.y')) |>

  filter(!is.na(row_name.y))
  mutate(evidance_in_favour = !is.na(row_name_x) & ! is.na(row_name_y)) |>
  mutate(row_name_x = if_else(is.na(row_name_x), row_name.x, row_name_x)) |>
  mutate(row_name_y = if_else(is.na(row_name_y), row_name.y, row_name_y))



b
t_dat$total_comparisons
t_dat$lambda
b
a$tokens_to_keep
a$tokens_all
x = list(a=1,b=2)

q <- map2(names(x), x, function(nm, val){print(val)})
a(b=1)$b
a()




tokenize_ations(dat, col_nms = 'exec_fullname')

tokenize_col(dat, col_nm = 'exec_fullname', drop_col = TRUE)

tokenize_df(dat , col_nms = c('coname', 'exec_fullname'), token_type = c('a','b')) %>% count(token_type)
tokenize_df(dat , col_nms = c('coname', 'exec_fullname'))

dat_tokens <- tokenize_df(dat , col_nms = c('coname', 'exec_fullname'), token_type = c('a','b'))
