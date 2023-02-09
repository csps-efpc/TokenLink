library(tidyr)
library(dplyr)
library(readr)
library(magrittr)

# devtools::load_all()
# devtools::load_all()
library(TokenLink)

ceo_url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv'
ceo_url <- 'https://tinyurl.com/2p8etjr6'
alb_url <- 'https://open.alberta.ca/dataset/a2b1fc9b-aac4-4718-8645-b0466ca5ec57/resource/3da9a7f9-bd34-48c0-841f-19c856b551ad/download/foodindustry.csv'
alb_url <- 'https://tinyurl.com/2p8ap4ad'

# Load Data From internet
dat_ceo <- readr::read_csv(ceo_url)
dat_alb <- readr::read_csv(alb_url)



dat_ceo_tokes <-
  dat_ceo |>
  tokenize_ations(col_nms = 'coname', token_types = 'company_name')

dat_ceo_tokes |>
  magrittr::extract2('tokens') |>
  group_by(row_name) |>
  nest() |> ungroup() |>
  sample_n(3) |>
  unnest(cols = data) |>
  knitr::kable(caption = 'Tokens')



nsamp <- 4

dat_ceo_tokes |>
  magrittr::extract2('token_counts') |>
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  arrange(desc(n)) |>
  knitr::kable(caption = 'Token Counts')


nsamp <- 4

dat_ceo_tokes |>
  magrittr::extract2('token_counts') |>
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  arrange(desc(n)) |>
  knitr::kable(caption = 'Token Counts')




t_dat <- token_links(
  dat_x = dat_ceo,
  dat_y = dat_alb,
  args_x = list(col_nms = 'coname'),
  args_y = list(col_nms = 'companyName'),
  token_types = 'company_name',
  token_index = '',
  suffix = c('ceo', 'alb')
)

t_dat |>
  extract2('tokens_all') |>
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  knitr::kable(caption = 'All Tokens')



t_dat <-
  t_dat |>
  find_posterior()

t_dat$all_evidence |>
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  arrange(desc(posterior)) |>
  knitr::kable()



t_dat |>  joined_results(include_row_numbers = TRUE, link_col_nms = c('posterior', 'tokens_in_favour', 'tokens_against')) |>
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  arrange(desc(posterior)) |>
  knitr::kable()

