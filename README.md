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
source('R/tokenify.R')
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

| coname              | exec_fullname      |
|:--------------------|:-------------------|
| STERLING BANCORP    | George L. Strayton |
| COMSCORE INC        | Gian M. Fulgoni    |
| CINCINNATI BELL INC | Kevin W. Mooney    |

### Data from [open.alberta.ca](https://open.alberta.ca/dataset/a2b1fc9b-aac4-4718-8645-b0466ca5ec57)

| companyName                      | address             | town              | province |
|:---------------------------------|:--------------------|:------------------|:---------|
| Love’s Custom Meats Inc.         | Box 953             | Vegreville        | AB       |
| Rocky Mountain Flatbread Company | 101, 838 - 10 St.   | Canmore           | AB       |
| Balzac Meat Processing           | 293113 Twp. Rd. 263 | Rocky View County | AB       |

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

| row_name | token        | token_type   |
|:---------|:-------------|:-------------|
| 8934     | corcept      | company_name |
| 8934     | therapeutics | company_name |
| 8934     | incorporated | company_name |
| 3856     | wet          | company_name |
| 3856     | seal         | company_name |
| 3856     | incorporated | company_name |
| 3856     | cl           | company_name |
| 3856     | a            | company_name |
| 1007     | idacorp      | company_name |
| 1007     | incorporated | company_name |

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
| harman       | company_name |   14 |
| jo           | company_name |    2 |
| embarq       | company_name |    2 |
| rykoff       | company_name |    1 |
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
| hibernia     | company_name |     2 |     0 |             0 | 0.0000000 | 0.9990000 |
| twenty       | company_name |     2 |     0 |             0 | 0.0000000 | 0.9990000 |
| lyondell     | company_name |     2 |     0 |             0 | 0.0000000 | 0.9990000 |
| goodmark     | company_name |     1 |     0 |             0 | 0.0000000 | 0.9990000 |
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

t_dat$all_evidence |> 
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
| 3167         | 133          |                3 |              1 | 3.9e-06 | 0.9999739 |
| 171          | 133          |                3 |              1 | 3.9e-06 | 0.9999739 |
| 3167         | 134          |                2 |              3 | 3.9e-06 | 0.0136264 |
| 4404         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |
| 3041         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |
| 4404         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |
| 6032         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |
| 6247         | 241          |                3 |              4 | 3.9e-06 | 0.0103901 |

# Visual Compare Of Results

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
| 59           | 40           | 1.0000000 |                3 |              1 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 3115         | 40           | 1.0000000 |                3 |              1 | ARCHER-DANIELS-MIDLAND CO | Archer Daniels Midland         |
| 6038         | 134          | 0.0136264 |                2 |              3 | COCA-COLA CO              | Coca-Cola Bottling Ltd.        |
| 3167         | 134          | 0.0136264 |                2 |              3 | COCA-COLA CO              | Coca-Cola Bottling Ltd.        |
| 3041         | 241          | 0.0103901 |                3 |              4 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 4404         | 241          | 0.0103901 |                3 |              4 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 6032         | 241          | 0.0103901 |                3 |              4 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 6247         | 241          | 0.0103901 |                3 |              4 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
