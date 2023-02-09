


#' @import base
#' @import purrr
#' @import magrittr

N_CHUNKS = 100






#' Like "find_posterior" but this will always set "return_all = FALSE" and will require a subset of rownames for at least the X data set. but also maybe the y dataset
#'
#' @param t_dat t_dat object
#' @param ... passed to both find_posterior_positive_evidence_only and find_posterior_all_evidence
#' @param x_rows_filter vector of row numbers to filter the x dataset by NO Default
#' @param y_rows_filter vector of row numbers to filter the y dataset Default NULL
#' @param min_posterior_all_evidence min_posterior_all_evidence passed to find_posterior_all_evidence as min_posterior. Default NULL
#' @param min_posterior_positive_evidence_only  passed to find_posterior_positive_evidence_only as min_posterior. Default NULL
#'
#' @return
#' @export
#'
#' @examples
find_posterior_subset <- function(t_dat,
                                  x_rows_filter,
                                  ...,
                                  y_rows_filter = NULL,
                                  min_posterior_all_evidence = NULL,
                                  min_posterior_positive_evidence_only = NULL
){

  positive_evidence_only <-
    t_dat |>
    find_posterior_positive_evidence_only(...,
                                          x_rows_filter = x_rows_filter,
                                          y_rows_filter = y_rows_filter,
                                          return_all = FALSE,
                                          min_posterior = min_posterior_positive_evidence_only,
    )

  all_evidence <-
    t_dat |>
    find_posterior_all_evidence(...,
                                positive_evidence_only = positive_evidence_only,
                                return_all = FALSE,
                                min_posterior = min_posterior_all_evidence
    )
  all_evidence
}










#' Like "find_posterior" but it uses less memory at any one time by iterating over chunks of the x dataset.
#' When this is used the returned t_dat object will not have "positive_evidence_only" dataframe it will have the "all_evidence" dataframe
#'
#' @param t_dat  t_dat object
#' @param ...  passed to find_posterior_subset
#' @param min_posterior_all_evidence  passed to find_posterior_subset. : Default MIN_POSTERIOR_ALL_EVIDENCE
#' @param min_posterior_positive_evidence_only  passed to find_posterior_subset. : Default MIN_POSTERIOR_POSITIVE_EVIDENCE_ONLY
#' @param n_chunks How many chunks to break up the x dataset into : Default 100
#' @param return_all return modified "t_dat" object if TRUE otherwise return the all_evidence dataframe : Default TRUE
#'
#' @return
#' @export
#'
#' @examples
find_posterior_chunked <- function(t_dat,
                                   ...,
                                   min_posterior_all_evidence = MIN_POSTERIOR_ALL_EVIDENCE,
                                   min_posterior_positive_evidence_only = MIN_POSTERIOR_POSITIVE_EVIDENCE_ONLY,
                                   n_chunks = N_CHUNKS,
                                   return_all = TRUE
){
  min_posterior_all_evidence = NULL
  min_posterior_positive_evidence_only = NULL
  x <- 1:(t_dat$x$dat |> nrow())
  lx = length(x)
  all_evidence <-
    base::split(x, rep(1:ceiling(lx/n), each=(lx/n_chunks)+1, length.out=lx)) |>
    purrr::map_dfr(\(.x_rows_filter){
      print('.')
      .x_rows_filter <- base::split(x, rep(1:ceiling(lx/n), each=(lx/n_chunks)+1, length.out=lx)) |> base::sample(1) |> magrittr::extract2(1)
      find_posterior_subset(t_dat = t_dat,
                            x_rows_filter = .x_rows_filter,
                            min_posterior_all_evidence = min_posterior_all_evidence,
                            min_posterior_positive_evidence_only = min_posterior_positive_evidence_only,
                            ...)
    })


  if (return_all){
    t_dat$posterior_threshold_all_evidence <- min_posterior_all_evidence
    t_dat$posterior_threshold_positive_only <- min_posterior_positive_evidence_only
    t_dat$all_evidence  <- all_evidence
    return(t_dat)
  }else{
    return(all_evidence)
  }
}




