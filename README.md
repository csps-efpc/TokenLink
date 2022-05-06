TokenLink
================
2022-05-05

# TokenLink

link two dataset using tokens or words in common between them

# Install

``` r
devtools::install_github("csps-efpc/TokenLink")
```

# Example Basic Usage

## Load Libraries and Data from Internet

``` r
library(tidyr)
library(dplyr)
library(readr)
library(magrittr)
library(TokenLink)

ceo_url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv'
ceo_url <- 'https://tinyurl.com/2p8etjr6'
alb_url <- 'https://open.alberta.ca/dataset/a2b1fc9b-aac4-4718-8645-b0466ca5ec57/resource/3da9a7f9-bd34-48c0-841f-19c856b551ad/download/foodindustry.csv'
alb_url <- 'https://tinyurl.com/2p8ap4ad'

# Load Data From internet
dat_ceo <- readr::read_csv(ceo_url)
dat_alb <- readr::read_csv(alb_url)
```

### CEO Resignation data from [Tidy Tuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-27/readme.md)

| coname              | exec_fullname        |
|:--------------------|:---------------------|
| INFOSEEK CORP       | Harry M. Motro       |
| BELL INDUSTRIES INC | John A. Fellows      |
| AMERIGROUP CORP     | James Gordon Carlson |

### Data from [open.alberta.ca](https://open.alberta.ca/dataset/a2b1fc9b-aac4-4718-8645-b0466ca5ec57)

| companyName                           | address        | town       | province |
|:--------------------------------------|:---------------|:-----------|:---------|
| Provost Packers Ltd.                  | Box 570        | Provost    | AB       |
| Alberta Safflower Growers Association | Box 822        | Lethbridge | AB       |
| Normerica Inc.                        | 112 - 30 St. N | Lethbridge | AB       |

# Show Tokenization

``` r
dat_ceo_tokes <- 
  dat_ceo |> 
  tokenize_ations(col_nms = 'coname', token_types = 'company_name') 

dat_ceo_tokes |>
  magrittr::extract2('tokens') |> 
  group_by(row_name) |>
  nest() |> ungroup() |>
  sample_n(3) |>
  unnest() |>
  knitr::kable(caption = 'Tokens')
```

| row_name | token      | token_type   |
|:---------|:-----------|:-------------|
| 9014     | kulicke    | company_name |
| 9014     | and        | company_name |
| 9014     | soffa      | company_name |
| 9014     | industries | company_name |
| 3127     | oakwood    | company_name |
| 3127     | homes      | company_name |
| 3127     | corp       | company_name |
| 7934     | carpenter  | company_name |
| 7934     | technology | company_name |
| 7934     | corp       | company_name |

Tokens

# Count Tokenization

``` r
nsamp <- 4

dat_ceo_tokes |> 
  magrittr::extract2('token_counts') |> 
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  arrange(desc(n)) |>
  knitr::kable(caption = 'Token Counts') 
```

| token        | token_type   |    n |
|:-------------|:-------------|-----:|
| incorporated | company_name | 5064 |
| corp         | company_name | 2436 |
| company      | company_name |  728 |
| group        | company_name |  470 |
| lear         | company_name |    4 |
| conn         | company_name |    4 |
| imports      | company_name |    4 |
| cities       | company_name |    3 |
| zions        | company_name |    1 |
| zoetis       | company_name |    1 |
| zumiez       | company_name |    1 |
| zynex        | company_name |    1 |

Token Counts

# Create a t_dat Object

``` r
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
```

| token        | token_type   | n.ceo | n.alb | n_comparisons |    u_prob |    m_prob |
|:-------------|:-------------|------:|------:|--------------:|----------:|----------:|
| incorporated | company_name |  5064 |   102 |        516528 | 0.1081177 | 0.9900000 |
| company      | company_name |   728 |    37 |         26936 | 0.0056381 | 0.9920208 |
| limited      | company_name |   114 |   197 |         22458 | 0.0047008 | 0.9921452 |
| corp         | company_name |  2436 |     9 |         21924 | 0.0045890 | 0.9921616 |
| bites        | company_name |     0 |     1 |             0 | 0.0000000 | 0.9990000 |
| skin         | company_name |     2 |     0 |             0 | 0.0000000 | 0.9990000 |
| rock         | company_name |     1 |     3 |             3 | 0.0000006 | 0.9982484 |
| amsco        | company_name |     2 |     0 |             0 | 0.0000000 | 0.9990000 |
| yat          | company_name |     0 |     1 |             0 | 0.0000000 | 0.9990000 |
| yeg          | company_name |     0 |     1 |             0 | 0.0000000 | 0.9990000 |
| yogurt       | company_name |     0 |     1 |             0 | 0.0000000 | 0.9990000 |
| zinter       | company_name |     0 |     1 |             0 | 0.0000000 | 0.9990000 |

All Tokens

# Find Posteriors

``` r
t_dat <- 
  t_dat |> 
  find_posterior()

t_dat$all_evidance %>% 
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  arrange(desc(posterior)) |>
  knitr::kable()
```

| row_name.ceo | row_name.alb | tokens_in_favour | tokens_against |  priori | posterior |
|:-------------|:-------------|-----------------:|---------------:|--------:|----------:|
| 3115         | 40           |                3 |              1 | 3.9e-06 | 1.0000000 |
| 59           | 40           |                3 |              1 | 3.9e-06 | 1.0000000 |
| 5962         | 40           |                3 |              1 | 3.9e-06 | 1.0000000 |
| 7464         | 40           |                3 |              1 | 3.9e-06 | 1.0000000 |
| 8465         | 134          |                2 |              3 | 3.9e-06 | 0.0136264 |
| 171          | 134          |                2 |              3 | 3.9e-06 | 0.0136264 |
| 172          | 134          |                2 |              3 | 3.9e-06 | 0.0136264 |
| 3040         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |
| 3041         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |
| 4404         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |
| 6032         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |
| 6247         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |

# Vissual Compare Of Results

``` r
t_dat |>  joined_results(include_row_numbers = TRUE, link_col_nms = c('posterior', 'tokens_in_favour', 'tokens_against')) |>
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  arrange(desc(posterior)) |>
  knitr::kable()
```

| row_name.ceo | row_name.alb | posterior | tokens_in_favour | tokens_against | coname.ceo                | companyName.alb                |
|:-------------|:-------------|----------:|-----------------:|---------------:|:--------------------------|:-------------------------------|
| 3115         | 40           | 1.0000000 |                3 |              1 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 59           | 40           | 1.0000000 |                3 |              1 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 5962         | 40           | 1.0000000 |                3 |              1 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 7464         | 40           | 1.0000000 |                3 |              1 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 7464         | 40           | 1.0000000 |                3 |              1 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 172          | 133          | 0.9999739 |                3 |              1 | COCA-COLA CO              | Coca-Cola Bottling Company     |
| 170          | 134          | 0.0136264 |                2 |              3 | COCA-COLA CO              | Coca-Cola Bottling Ltd.        |
| 3041         | 241          | 0.0103901 |                3 |              4 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 3041         | 241          | 0.0103901 |                3 |              4 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 4404         | 241          | 0.0103901 |                3 |              4 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 6032         | 241          | 0.0103901 |                3 |              4 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 6247         | 241          | 0.0103901 |                3 |              4 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
