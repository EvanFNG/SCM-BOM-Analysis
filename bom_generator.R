library(tidyverse)

set.seed(123)

# Creating a list of random components to select from.
parts_list <- as.character(sample(1e5:9e5, 200))

# List of random product SKUs
product_list <- as.character(sample(1e4:9e4, 15))

# Creating a table of finished product SKUs with every row containing a list
# of the components going into them and the quantity per build.
tibble(
  sku = product_list,
  item_code = list(component = sample(parts_list, 40))) |>
  unnest(item_code) |>
  mutate(
    qty = sample(1:3, n(), prob = c(.8, .15, .05), replace = TRUE),
    src = sample(c('M', 'P'), n(), prob = c(.3, .7), replace = TRUE)) |>
  nest(bom = c(item_code, qty, src)) ->
  bom_list

# Writing each imaginary BOM to a csv.
# You will need a folder in your working directory called rand_boms for this to work
map2(
  bom_list$bom,
  bom_list$sku,
  ~ write_csv(.x, file = paste0('rand_boms/', .y, '.csv'))
)

