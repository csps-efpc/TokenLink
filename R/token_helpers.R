
#' @import tidytext
#' @import readr
#' @import tibble
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @import glue
#' @import purrr
#' @import glue



DEFAULT_RANGE_NGRAM_RANGE <- 1:4
DEFAULT_NUM_NGRAMS_RETURN <- 25


#' Returns a dataframe of common ngrams
#'
#' @param .v a vector of strings
#' @param n_range range of ngrames to feed back : Default 1:4
#' @param token passed to tokenizer_basic which passes it to tidytext::unnest_tokens :Default 'ngrams'
#' @param n_ngrams_returns how many of each n to return for most common :Default 12
#' @param tokenizer a function that tokenizes a column of a data frame :Default tokenizer_basic
#' @param ... passed to tokenizer_basic
#'
#' @return a dataframe showing the most common tokens
#' @export
#'
#' @examples
#'   mtcars |> tibble::rownames_to_column() |> dplyr::pull(rowname) |> token_most_common()
#'   library(tokenizers)
#'   mobydick |> stringr::str_split('\\.')   |> magrittr::extract2(1) |> token_most_common()
#'
token_most_common <- function(.v,
                              n_range = DEFAULT_RANGE_NGRAM_RANGE,
                              token = 'ngrams',
                              n_ngrams_returns = DEFAULT_NUM_NGRAMS_RETURN,
                              tokenizer = tokenizer_basic,
                              ...
                              ){
  row_name_nm = 'row_name'
  col_nm = 'txt'
  #token_type = glue::glue('{col_nm}')

  v_prime <- unique(.v)
  v_prime_length = v_prime |> length()
  dat <-
    tibble::tibble(
      txt = v_prime
    )


  all_grams <-
    n_range |>
    purrr::map_dfr(\(.n){
      dat |>
        tibble::rownames_to_column(var = row_name_nm) |>
        dplyr::select(dplyr::all_of(c(row_name_nm, col_nm))) |>
        tokenizer(col_nm = col_nm, row_name_nm = row_name_nm, #token_type = token_type,
                  token = token, n = .n, ...) |>
        dplyr::distinct() |>
        #dplyr::mutate(token_type = token_type ) |>
        dplyr::count(token, sort = TRUE) |>
        dplyr::filter(!is.na(token)) |>
        dplyr::mutate(ngram = .n)
    })

  n_range |>
    purrr::map_dfr(\(.n){
      #.n = 1
      #print(.n)

      curr_grams_to_remove <-
        all_grams |>
        dplyr::filter(ngram == .n) |>
        dplyr::slice_max(order_by = n, n = n_ngrams_returns)

      #print(curr_grams_to_remove |> dplyr::pull(token))


      curr_grams_to_remove |>
        dplyr::pull(token) |>
        purrr::walk(\(.word){
          all_grams <<-
            all_grams |>
            dplyr::filter(!stringr::str_detect(token, glue::glue('\\b{.word}\\b')))
        })
      #print(all_grams |> nrow())
      curr_grams_to_remove
    })  |>
    dplyr::mutate(f = n/v_prime_length)
}




#' returns a vector of replacement tokens
#'
#' @param .v vector of strings
#' @param replace_space_with replace punctuation with: Default '_'
#' @param replace_punc_with  replace space with: Default '_'
#'
#' @return
#' @export
#'
#' @examples
#'   token_replacement_generator(c('One Two Three', 'ehhh bee See'))
token_replacement_generator <- function(.v, replace_space_with = '_', replace_punc_with = '_'){
  #.v <- most_common_2[['token']]
 .v |>
    stringr::str_squish() |>
    stringr::str_replace_all('[:punct:]', replace_punc_with) |>
    stringr::str_replace_all('[:space:]', replace_space_with) |>
    stringr::str_to_lower()
}




#write_token_replacement(common_tokens$token, token_type = 'company_name')

#' Given a vector of strings this will create or append a file of replacement tokens
#'
#' @param .v vector of strings
#' @param token_type a string
#' @param append_mode
#' @param ... passed to read_replacements_token_type_get_fn
#'
#' @return
#' @export
#'
#' @examples
write_token_replacement <- function(.v,
                                    token_type,
                                    append_mode = 'best',
                                    create_usable_or_testing_file = 'testing',
                                    ...
                                    ){

  ##################################
  # Check inputs
  acceptible_append_mode <- c('after','before','replace', 'best')
  if (! append_mode %in% acceptible_append_mode){
    stop(glue::glue('append_mode must given an incorect value of "{append_mode}", try using one of ({paste0(acceptible_append_mode, collapse = ",")})'))
  }

  if (file.exists(fn)){
    file.copy(fn, glue::glue('{fn}.{Sys.Date()}.bak'))
  }



  fn <- read_replacements_token_type_get_fn(token_type, ...)
  orig_replace_file <-
    if (file.exists(fn) & append_mode != 'replace'){
      readr::read_csv(fn, show_col_types = FALSE) |>
        janitor::clean_names() |>
        dplyr::mutate_if(is.character, function(x){trimws(tolower(x))}) |>
        dplyr::mutate(orig = T)
    }else{
      tibble::tibble(token = character(), replacement = character(), word_wrap = logical(), orig = logical())
    }



  added_replace_file <-
    tibble::tibble(token = .v) |>
      dplyr::mutate(replacement = token_replacement_generator(token)) |>
      dplyr::mutate(word_wrap = T) |>
      dplyr::select(token,	replacement, word_wrap) |>
      dplyr::mutate(orig = F)


  fn_out <-
    if (create_usable_or_testing_file == 'testing'){
      glue::glue('{fn}_{Sys.Date()}.csv')
    }else{

    }


  replace_file_final <-
    dplyr::bind_rows(orig_replace_file, added_replace_file ) |>
    dplyr::mutate(n_char = nchar(token)) |>
    dplyr::mutate(n_spaces = stringr::str_count(token, '[:space:]')) |>
      {\(.) if(append_mode == 'after'){
          dplyr::arrange(., desc(orig))
        }else if (append_mode == 'before'){
          dplyr::arrange(., orig)
        } else if(append_mode == 'best'){
          . |>
            #dplyr::mutate(n_spaces = stringr::str_count(token, '[:space:]')) |>
            dplyr::arrange(desc(n_char), n_spaces)
        }
      }() |>
    dplyr::filter(token != replacement) |>
    dplyr::distinct(token, .keep_all = TRUE) |>
    dplyr::select(token, replacement, word_wrap) |>
    dplyr::mutate( word_wrap = dplyr::if_else(word_wrap, 'T', 'F')) |>
    readr::write_csv(fn_out)
}
