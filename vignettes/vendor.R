




getwd()

library(tidyverse)
library(arrow)
library(magrittr)
library(stringr)
library(janitor)
#devtools::load_all()





###########################
#
# Read in a database for ISED Data. This dataframe was made with a seperate project "https://github.com/csps-efpc/federal-corporations"
#
ised_corp <-
  file.path('..', 'Federal-Corporations', 'data', 'names.feather') |>
  arrow::read_feather() |>
  dplyr::distinct(stringr::str_to_upper(name), .keep_all = TRUE) |>
  dplyr::select(name)






###########################
#
# Read in the Federal contract Data data.
#
contract_venders <-
  file.path('..', 'contracts-data', 'data', 'source') |>
  list.files(pattern = '\\d{4}-\\d{2}-\\d{2}-contracts.csv', full.names = TRUE) |>
  sort(decreasing = TRUE) |>
  magrittr::extract2(1) |>
  readr::read_csv() |>
  dplyr::distinct(stringr::str_to_upper(vendor_name), .keep_all = TRUE) |>
  dplyr::select(vendor_name) |>
  dplyr::rename(name := vendor_name)



######################################
#
# Create t_dat Object
#
t_dat <- token_links(
  dat_x = contract_venders,
  dat_y = ised_corp,
  args_x = list(col_nms = 'name'),
  args_y = list(col_nms = 'name'),
  token_types = 'company_name',
  token_index = '',
  suffix = c('vend', 'ised')
)



###########################
#
# use the "chunked" version of the function because the other version gives memmory errors.
#
t_dat <- t_dat |> find_posterior_chunked()


y <- t_dat$all_evidence

#################################
#
# Get ALL the results for ONE row of the vendors
#
id_sampled <- y |> distinct(row_name.vend) |> pull() |>sample(1)
t_dat |> joined_results(include_row_numbers = TRUE, pairs_to_get = y |> filter(row_name.vend == id_sampled) ) |>
    arrange(desc(posterior))


