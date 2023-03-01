



#' @import purrr
#' @import magrittr
#' @import dplyr
#' @import igraph
source(file.path('R', 'tokenify.R'))







#' comverts a igraph to a dataframe, giving a group name to every subgraph
#'
#' @param g a graph
#' @param group_prefix prefix for group name
#'
#' @return
#' @export
#'
#' @examples
group_df_from_graph <- function(g, group_prefix = 'g_'){

  assertthat::assert_that(class(g) == 'igraph')

  g_s <-
    g |>
    igraph::decompose.graph()


  purrr::map2_dfr(g_s, 1:length(g_s),
                  \(g_i, i){
                    #g_i <- g_s |> sample(1) |> extract2(1)
                    #print(g_i, i)
                    num_nodes_i <- igraph::gorder(g_i)
                    g_i_names <- g_i |> igraph::V() |> names()
                    g_name <- paste0(group_prefix,i)
                    dplyr::tibble(
                      g_name = g_name,
                      num_nodes = num_nodes_i,
                      nodes = g_i_names
                    )
                  })
}




#' given a single dataframe, will de-duplicate that dataframe and return either a dataframe with grouping names or a TokenLink::t_dat object with the data.
#'
#' @param dat_x_y  a data frame to look for duplicates in.
#' @param col_nms_x_y
#' @param args_x_y
#' @param max_total_comparisons
#' @param min_token_u_prob
#' @param min_posterior_all_evidence
#' @param min_posterior_positive_evidence_only
#' @param n_chunks
#' @param min_posterior_for_graph
#' @param group_prefix
#' @param node_prefix
#' @param return_all
#' @param ... passed to token_link function
#'
#' @return
#' @export
#'
#' @examples
group_duplicates <- function(dat_x_y,# = contracts_raw |> select(matches('vendor_name')) |> distinct(),
                             col_nms_x_y = colnames(dplyr::select_if(dat_x_y, is.character)),
                             args_x_y = list(),
                             max_total_comparisons  = TOKEN_MAX_COMPARE_DEFAULT*10,
                             min_token_u_prob = 0.001,
                             min_posterior_all_evidence = 0.001,
                             min_posterior_positive_evidence_only  = 0.01,
                             n_chunks =  TokenLink:::N_CHUNKS,
                             min_posterior_for_graph = 0.93,
                             group_prefix = 'g_',
                             node_prefix = 'N_',
                             return_all = TRUE,
                             ...){
  # dat <-
  #   dat_raw |>
  #   mutate(vendor_name_pre_clean = str_squish(TokenLink::clean_str(vendor_name, token_type = token_type)))
  #

  # t_dat <- NULL
  # token_types = uuid::UUIDgenerate() |> as.character()


  t_dat <- TokenLink::token_links(
    dat_x = dat_x_y,
    dat_y = dat_x_y,
    col_nms_x = col_nms_x_y,
    col_nms_y = col_nms_x_y,
    args_x = args_x_y,
    args_y = args_x_y,
    #token_types = token_types#,
    ...
  )


  dat_x_y <-
    dat_x_y |>
    dplyr::mutate(row_id=dplyr::row_number())


  t_dat <-
    t_dat |>
    find_posterior_chunked(
      max_total_comparisons = max_total_comparisons,
      min_token_u_prob = min_token_u_prob,
      min_posterior_all_evidence = min_posterior_all_evidence,
      min_posterior_positive_evidence_only  = min_posterior_positive_evidence_only,
      n_chunks =  n_chunks
    )

  best_matches <-
    t_dat$all_evidence |>
    dplyr::filter(posterior > min_posterior_for_graph) |>
    dplyr::filter(row_name.x != row_name.y)

  #######################
  # Make Nodes in Graph
  num_nodes <- nrow(dat_x_y)
  node_names <- paste0(node_prefix, 1:num_nodes)
  print(glue('Number of Nodes = {num_nodes}.'))
  #g <- igraph::make_(igraph::empty_graph(n = num_nodes, directed=FALSE),  igraph::with_vertex_(name = node_names))
  g <- make_empty_graph(directed = FALSE)
  g <- g |> add_vertices(num_nodes, name = node_names)
  print(igraph::V(g))


  #############################
  # Add Edges to Graph
  edges_vector <-
    best_matches |>
    dplyr::select(row_name.x, row_name.y) |>
    purrr::pmap(function(row_name.x, row_name.y){
      c(row_name.x, row_name.y)
    }) |>
    unlist()

  print(n)
  print(glue('largest vector node number {max(edges_vector)}'))
  edge_name <- paste0(node_prefix, edges_vector)
  g <- g |> igraph::add_edges(edges = edge_name)
  print(g)




  ###############################
  # Find Individual graph components
  g_dat <- g |>
    group_df_from_graph(group_prefix = group_prefix) |>
    mutate(g_i = as.integer(stringr::str_replace_all(g_name   , group_prefix, ''))) |>
    mutate(node_i = as.integer(stringr::str_replace_all(nodes  , node_prefix, ''))) |>
    left_join(dat_x_y, by = c('node_i' = 'row_id'))



  if (return_all) {
    t_dat$min_posterior_for_graph <- min_posterior_for_graph
    t_dat$best_matches <- best_matches
    t_dat$g_dat <- g_dat
    return(t_dat)
  }
  else {
    return(g_dat)
  }


}

