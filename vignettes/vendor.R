

getwd()

library(tidyverse)
library(arrow)
library(magrittr)
library(stringr)
library(janitor)



ised_corp <-
  file.path('..', 'Federal-Corporations', 'data', 'names.feather') |>
  arrow::read_feather() |>
  dplyr::distinct(stringr::str_to_upper(name), .keep_all = TRUE) |>
  dplyr::select(name)







contract_venders <-
  file.path('..', 'contracts-data', 'data', 'source') |>
  list.files(pattern = '\\d{4}-\\d{2}-\\d{2}-contracts.csv', full.names = TRUE) |>
  sort(decreasing = TRUE) |>
  magrittr::extract2(1) |>
  readr::read_csv() |>
  dplyr::distinct(stringr::str_to_upper(vendor_name), .keep_all = TRUE) |>
  dplyr::select(vendor_name) |>
  dplyr::rename(name := vendor_name)




t_dat <- token_links(
  dat_x = contract_venders,
  dat_y = ised_corp,
  args_x = list(col_nms = 'name'),
  args_y = list(col_nms = 'name'),
  token_types = 'company_name',
  token_index = '',
  suffix = c('vend', 'ised')
)



t_dat <-
  t_dat |>
  find_posterior(max_total_comparisons = 5e+08, min_posterior_positive_evidence_only = 0.1)





t_dat |>
  joined_results(include_row_numbers = TRUE, link_col_nms = c('posterior', 'tokens_in_favour', 'tokens_against')) |>
  dplyr::as_tibble() |>
  dplyr::group_by(row_name.vend) |>
  dplyr::slice_max(order_by = posterior, n = 1)  |>
  ggplot2::ggplot(ggplot2::aes(x = posterior )) + ggplot2::geom_density()



t_dat |>
  joined_results(include_row_numbers = TRUE, link_col_nms = c('posterior', 'tokens_in_favour', 'tokens_against')) |>
  dplyr::as_tibble() |>
  dplyr::group_by(row_name.vend) |>
  dplyr::slice_max(order_by = posterior, n = 1)  |>
  dplyr::filter(stringr::str_to_lower(name.vend) != stringr::str_to_lower(name.ised)) |>
  dplyr::filter(posterior >= 0.80 & posterior <= 0.95) |>
  ungroup() |>
  dplyr::sample_n(20)


t_dat |>
  joined_results(include_row_numbers = TRUE, link_col_nms = c('posterior', 'tokens_in_favour', 'tokens_against')) |>
  dplyr::filter(stringr::str_to_lower(name.vend) != stringr::str_to_lower(name.ised)) |>
  dplyr::filter(posterior >= 0.90 & posterior <= 0.95) |>
  dplyr::sample_n(20)






t_dat$tokens_all |>
  filter(str_detect(token, 'harb')) |>
  mutate(n_max = pmax(n.vend, n.ised)) |>
  View()

x_rows <- t_dat$x$dat |> nrow()
y_rows <- t_dat$y$dat |> nrow()
t_dat$tokens_all |>
  dplyr::filter(u_prob == 0) |>
  mutate(f.vend = n.vend/x_rows, f.ised = n.ised/y_rows) |>
  mutate(n_max = pmax(n.vend, n.ised)) |>
  mutate(f_max = pmax(f.vend, f.ised)) |>
  arrange(desc(f_max)) |>
  head(1000) |>
  View()
  dplyr::filter(u_prob < min_token_u_prob)


t_dat <-
  t_dat |>
  find_posterior()



t_dat$all_evidence |>
  arrange(desc(posterior)) |>
  knitr::kable()


t_dat |>
  joined_results(include_row_numbers = TRUE, link_col_nms = c('posterior', 'tokens_in_favour', 'tokens_against')) |>
  filter(posterior >0.75 & posterior < 0.9) |>
  sample_n(20)





t_dat$tokens_all |>
  filter(n.vend == 1 & n.ised == 1) |>
  arrange(n_comparisons) |> sample_n(1000) |> view()
