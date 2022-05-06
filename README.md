TokenLink
================
2022-05-05

<!-- badges: start -->

[![R-CMD-check](https://github.com/csps-efpc/TokenLink/workflows/R-CMD-check/badge.svg)](https://github.com/csps-efpc/TokenLink/actions)
<!-- badges: end -->

# TokenLink

link two dataset using tokens or words in common between them

# Example Basic Usage

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

``` r
knitr::kable(dplyr::select(dplyr::sample_n(dat_ceo, 3), coname, exec_fullname), caption = "CEO Resignation data from tidy Tuesday", floating.environment="sidewaystable")
```

| coname                     | exec_fullname       |
|:---------------------------|:--------------------|
| CHASE MANHATTAN CORP -OLD  | Thomas G. Labrecque |
| PREMIER BANCORP            | G. Lee Griffin      |
| REMINGTON OIL&GAS CP -CL B | Thomas D. Box       |

CEO Resignation data from tidy Tuesday

``` r
knitr::kable(dplyr::select(dplyr::sample_n(dat_alb, 3), companyName, address, town, province ), caption = "Some Dataset I found on Open Canada", floating.environment="sidewaystable")
```

| companyName            | address         | town       | province |
|:-----------------------|:----------------|:-----------|:---------|
| Lynn Thacker Ag. Corp. | Box 719         | Bow Island | AB       |
| Spragg’s Meat Shop     | Box 323         | Rosemary   | AB       |
| Bassano Growers Ltd.   | 923 - 28 St. NE | Calgary    | AB       |

Some Dataset I found on Open Canada

# Show Tokenization

``` r
dat_ceo_tokes <- 
  dat_ceo |> 
  tokenize_ations(col_nms = 'coname', token_types = 'company_name') 

dat_ceo_tokes |>
  magrittr::extract2('tokens') |> 
  group_by(row_name) |>
  nest() |> ungroup() |>
  sample_n(5) |>
  unnest() |>
  knitr::kable(caption = 'Tokens')
```

| row_name | token           | token_type   |
|:---------|:----------------|:-------------|
| 9263     | arrowhead       | company_name |
| 9263     | pharmaceuticals | company_name |
| 2294     | valassis        | company_name |
| 2294     | communications  | company_name |
| 2294     | incorporated    | company_name |
| 1916     | washington      | company_name |
| 1916     | energy          | company_name |
| 1916     | company         | company_name |
| 351      | hershey         | company_name |
| 351      | company         | company_name |
| 1801     | aqua            | company_name |
| 1801     | america         | company_name |
| 1801     | incorporated    | company_name |

Tokens

# Count Tokenization

``` r
nsamp <- 4

dat_ceo_tokes |> 
  magrittr::extract2('token_counts') |> 
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  knitr::kable(caption = 'Token Counts')
```

| token        | token_type   |    n |
|:-------------|:-------------|-----:|
| incorporated | company_name | 5064 |
| corp         | company_name | 2436 |
| company      | company_name |  728 |
| group        | company_name |  470 |
| legacy       | company_name |    8 |
| galileo      | company_name |    1 |
| standex      | company_name |    4 |
| dhi          | company_name |    3 |
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

| token        | token_type   |  n.x | n.y | n_comparisons |    u_prob |    m_prob |
|:-------------|:-------------|-----:|----:|--------------:|----------:|----------:|
| incorporated | company_name | 5064 | 102 |        516528 | 0.1081177 | 0.9900000 |
| company      | company_name |  728 |  37 |         26936 | 0.0056381 | 0.9920208 |
| limited      | company_name |  114 | 197 |         22458 | 0.0047008 | 0.9921452 |
| corp         | company_name | 2436 |   9 |         21924 | 0.0045890 | 0.9921616 |
| factory      | company_name |    8 |   2 |            16 | 0.0000033 | 0.9971031 |
| kbr          | company_name |    2 |   0 |             0 | 0.0000000 | 0.9990000 |
| assurant     | company_name |    2 |   0 |             0 | 0.0000000 | 0.9990000 |
| compusa      | company_name |    2 |   0 |             0 | 0.0000000 | 0.9990000 |
| yat          | company_name |    0 |   1 |             0 | 0.0000000 | 0.9990000 |
| yeg          | company_name |    0 |   1 |             0 | 0.0000000 | 0.9990000 |
| yogurt       | company_name |    0 |   1 |             0 | 0.0000000 | 0.9990000 |
| zinter       | company_name |    0 |   1 |             0 | 0.0000000 | 0.9990000 |

All Tokens

# Find Posteriors

``` r
t_dat <- 
  t_dat |> 
  find_posterior()

t_dat$all_evidance %>% 
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  arrange(desc(posterior)) |>
  knitr::kable(caption = 'Posterior')
```

| row_name.x | row_name.y | tokens_in_favour | tokens_against |  priori | posterior |
|:-----------|:-----------|-----------------:|---------------:|--------:|----------:|
| 3115       | 40         |                3 |              1 | 3.9e-06 | 1.0000000 |
| 59         | 40         |                3 |              1 | 3.9e-06 | 1.0000000 |
| 5962       | 40         |                3 |              1 | 3.9e-06 | 1.0000000 |
| 7464       | 40         |                3 |              1 | 3.9e-06 | 1.0000000 |
| 5962       | 40         |                3 |              1 | 3.9e-06 | 1.0000000 |
| 6038       | 133        |                3 |              1 | 3.9e-06 | 0.9999739 |
| 170        | 134        |                2 |              3 | 3.9e-06 | 0.0136264 |
| 6032       | 241        |                3 |              4 | 3.9e-06 | 0.0103901 |
| 3041       | 241        |                3 |              4 | 3.9e-06 | 0.0103901 |
| 4404       | 241        |                3 |              4 | 3.9e-06 | 0.0103901 |
| 6032       | 241        |                3 |              4 | 3.9e-06 | 0.0103901 |
| 6247       | 241        |                3 |              4 | 3.9e-06 | 0.0103901 |

Posterior

# Vissual Compare Of Results

``` r
t_dat |> joined_results() |>
  {\(.) bind_rows(head(., nsamp), sample_n(.,nsamp), tail(., nsamp))}() |>
  arrange(desc(posterior)) |>
  knitr::kable(caption = 'Compare Names')
```

| posterior | coname.x                  | companyName.y                  |
|----------:|:--------------------------|:-------------------------------|
| 1.0000000 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 1.0000000 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 1.0000000 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 1.0000000 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 1.0000000 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 0.9999739 | COCA-COLA CO              | Coca-Cola Bottling Company     |
| 0.9999739 | COCA-COLA CO              | Coca-Cola Bottling Company     |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |

Compare Names
