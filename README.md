TokenLink
================
2022-05-05

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

| coname                       | exec_fullname                            |
|:-----------------------------|:-----------------------------------------|
| LOCKHEED MARTIN CORP         | Vance D. Coffman                         |
| ADTALEM GLOBAL EDUCATION INC | Daniel M. Hamburger                      |
| STILLWATER MINING CO         | Michael James W. McMullen B.Sc (Geology) |

CEO Resignation data from tidy Tuesday

``` r
knitr::kable(dplyr::select(dplyr::sample_n(dat_alb, 3), companyName, address, town, province ), caption = "Some Dataset I found on Open Canada", floating.environment="sidewaystable")
```

| companyName               | address               | town       | province |
|:--------------------------|:----------------------|:-----------|:---------|
| Dehnamar Inc.             | 16137 - 128A St. NW   | Edmonton   | AB       |
| McLane’s Meats Ltd.       | Box 7084              | Wetaskiwin | AB       |
| Zinter-Brown Taste Treats | 25 Wedgewood Cres. NW | Edmonton   | AB       |

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

| row_name | token        | token_type   |
|:---------|:-------------|:-------------|
| 8486     | citizens     | company_name |
| 8486     | financial    | company_name |
| 8486     | group        | company_name |
| 8486     | incorporated | company_name |
| 2895     | travelers    | company_name |
| 2895     | cos          | company_name |
| 2895     | incorporated | company_name |
| 2899     | engelhard    | company_name |
| 2899     | corp         | company_name |
| 2780     | wellpoint    | company_name |
| 2780     | health       | company_name |
| 2780     | netwrks      | company_name |
| 2780     | incorporated | company_name |
| 6111     | option       | company_name |
| 6111     | care         | company_name |
| 6111     | incorporated | company_name |

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
| adc          | company_name |    3 |
| homes        | company_name |   20 |
| dealertrack  | company_name |    1 |
| clothing     | company_name |    1 |
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
| txu          | company_name |    3 |   0 |             0 | 0.0000000 | 0.9990000 |
| dsc          | company_name |    1 |   0 |             0 | 0.0000000 | 0.9990000 |
| santander    | company_name |    8 |   0 |             0 | 0.0000000 | 0.9990000 |
| kcs          | company_name |    1 |   0 |             0 | 0.0000000 | 0.9990000 |
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
| 3115       | 40         |                3 |              1 | 3.9e-06 | 1.0000000 |
| 172        | 134        |                2 |              3 | 3.9e-06 | 0.0136264 |
| 6247       | 241        |                3 |              4 | 3.9e-06 | 0.0103901 |
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
| 0.9999739 | COCA-COLA CO              | Coca-Cola Bottling Company     |
| 0.0136264 | COCA-COLA CO              | Coca-Cola Bottling Ltd.        |
| 0.0136264 | COCA-COLA CO              | Coca-Cola Bottling Ltd.        |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |
| 0.0103901 | STEWART ENTERPRISES -CL A | J & A Stewart Enterprises Ltd. |

Compare Names
