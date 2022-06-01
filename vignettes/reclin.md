TokenLink
================
2022-05-05

Link two data sets using tokens or words in common between them.

# Install Libraries

``` r
source('R/tokenify.R')
devtools::install_github("csps-efpc/TokenLink")
```

## Load Libraries

``` r
library(reclin)
library(tidyverse)
library(purrr)

data_dir <- 'data'
```

## Load in two datasets

``` r
orig_dat <- 
  data_dir |> 
  file.path('generated_dataset.csv') |>
  read_csv() |>
  replace_na(list(company_name = '', 
                  address = '', 
                  first_name = '', 
                  last_name = '', 
                  age = 0 
                  ))|>
  mutate_all(as.character)

edited_dat <- 
  data_dir |> 
  file.path('generated_dataset_random_edits.csv') |>
  read_csv() |>
  replace_na(list(company_name = '', 
                  address = '', 
                  first_name = '', 
                  last_name = '', 
                  age = 0 
                  )) |>
  mutate_all(as.character)




bind_cols(orig_dat, edited_dat |> rename_all(\(x){paste0(x,"_edited")})) |> 
  {\(.)select(., order(colnames(.)))}() |>
  sample_n(5) |> 
  knitr::kable(caption = 'original and edited data')
```

| address                                      | address_edited                                      | age | age_edited | company_name                       | company_name_edited                       | first_name | first_name_edited | last_name | last_name_edited  |
|:---------------------------------------------|:----------------------------------------------------|:----|:-----------|:-----------------------------------|:------------------------------------------|:-----------|:------------------|:----------|:------------------|
| Viã Oak Fort States States United            | Viã Fort bubalis States bubalis United              | 33  | 33         | Payments Electronics Oy Financial  | Electronics Oy Financial                  | David      | David dichromaiso | Lauren    | Laure             |
| Ankara Vegas United                          | Ankara Vegas United                                 | 5   | 5          | Espiritualidade Africa O           | Espiritualidade O pentacles               | Samantha   | Samanth           | Ranno     | Ranno             |
| Oregon                                       |                                                     | 84  | 84         | Gancedoat A Incorporated Limited   | Gancedoat A Incorporated Limited defender | Dylan      | Dylan             | Tullison  | Tullison peltless |
| States                                       |                                                     | 78  | 78         | Upcycle Stefan Isd Services        | Isd Services superscribe                  | Isaac      | Isaac             | Goldstein | Gvoldstein        |
| Comer Reading Greater Illinois States United | Comer Reading rottes Greater Illinois States United | 81  | 81         | Rmores Dass Earth South Industries | Rmores Dass Earth South                   | Matthew    | Matthew           | Kleese    | Kleese            |

original and edited data

## Block Pairs by First Name or Last Name is the same, as well any company_name or address fields that have a word in common that is not to common.

``` r
blocked_pairs <- reclin_pair_blocking(x = orig_dat,
                                      y = edited_dat, 
                                      blocking_var = c('first_name', 'last_name'), #Block on Any of these Columns
                                      token_types =  c('company_name', 'address'), #Block on Any of these tokens
                                      col_nms_x = c('company_name', 'address'),    # Column Names
                                      col_nms_y =  c('company_name', 'address'),   # Column Names
                                      min_token_u_prob = 0.0000784)               # min u_prob to consider blocking on

blocked_pairs |>
  as_tibble() 
```

    ## # A tibble: 22 x 2
    ##        x     y
    ##    <int> <int>
    ##  1     1     1
    ##  2     2     2
    ##  3     2    17
    ##  4     3     3
    ##  5     4    13
    ##  6     5     5
    ##  7     6     6
    ##  8     7     7
    ##  9     8     8
    ## 10    13    13
    ## # ... with 12 more rows

## Generate Reclin EM Scores

``` r
# Compare pairs in Reclin using First and last name
p <- reclin::compare_pairs(blocked_pairs, 
                   by = c('first_name', 'last_name'),
                    default_comparator = jaro_winkler(0.9))

m <- problink_em(p)
p <- score_simsum(p, var = "sim_sum")
p <- score_problink(p, model = m, var = "scores", type  = 'all')

p |>
  sample_n(5) |>
  knitr::kable(caption = 'Show scores cenerated from Reclin')
```

|   x |   y | first_name | last_name |  sim_sum | scores_mprob | scores_uprob | scores_mpost | scores_upost | scores_weight |
|----:|----:|-----------:|----------:|---------:|-------------:|-------------:|-------------:|-------------:|--------------:|
|  11 |  11 |  0.8095238 | 1.0000000 | 1.809524 |    0.6619053 |    0.3691278 |    0.7196689 |    0.2803311 |     0.5839794 |
|   3 |   3 |  1.0000000 | 0.9333333 | 1.933333 |    0.7130048 |    0.4618474 |    0.6884952 |    0.3115048 |     0.4342535 |
|   4 |   4 |  0.7777778 | 1.0000000 | 1.777778 |    0.6449610 |    0.3546526 |    0.7224990 |    0.2775010 |     0.5980511 |
|   6 |   6 |  1.0000000 | 0.8235294 | 1.823529 |    0.6297190 |    0.4715125 |    0.6565976 |    0.3434024 |     0.2893280 |
|   7 |   7 |  1.0000000 | 1.0000000 | 2.000000 |    0.7635711 |    0.4559793 |    0.7056602 |    0.2943398 |     0.5155588 |

Show scores cenerated from Reclin

## Refine the priori from reclin as a posterior taking into acount aditional information

``` r
refined_p <- 
  refine_posterior(p = p, 
                   x_dat = orig_dat, 
                   y_dat = edited_dat, 
                   weights_nm = 'scores_weight',
                   args_x = list(col_nms = c('company_name', 'address')),
                   args_y = list(col_nms = c('company_name', 'address')),
                   token_types = c('company_name', 'address')
                   )

refined_p |> 
  mutate(is_same = (x == y)) |>
  mutate(delta_belief = posterior - priori) |> 
  ggplot(aes(y = delta_belief, x = priori,  color = is_same )) + 
  geom_jitter(alpha = 0.05, width = 0.05, height = 0.05) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.25) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'How much do the token columns change our belief in the Match?',
       y = 'Change in Belief', 
       color = 'Actually the same')
```

![](reclin_files/figure-gfm/refine-1.png)<!-- -->
