# TokenLink
link two dataset using tokens or words in common between them

# Example Basic Usage
```
ceo_url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv'

alb_url <- 'https://open.alberta.ca/dataset/a2b1fc9b-aac4-4718-8645-b0466ca5ec57/resource/3da9a7f9-bd34-48c0-841f-19c856b551ad/download/foodindustry.csv'

# Load Data From internet
dat_ceo <- readr::read_csv(ceo_url)
dat_alb <- readr::read_csv(alb_url )

# source the functions
source('R/tokenify.R')

# create a t_dat object
t_dat <- token_links(
   dat_x = dat_ceo,
   dat_y = dat_alb,
  args_x = list(col_nms = 'coname'),
  args_y = list(col_nms = 'companyName'),
  token_types = 'company_name',
  token_index = '',
  suffix = c('ceo', 'alb')
)

# find posteriors
t_dat <- t_dat |> find_posterior()

# get Dataframe showing compared fields
t_dat |> joined_results()

```


