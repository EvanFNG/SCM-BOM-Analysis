library(tidyverse)

set.seed(123)

# Creating a list of random components to select from.
# Might want to add SRC to this so it doesn't vary.
parts_list <- as.character(sample(1e5:9e5, 200))

# List of random product SKUs
product_list <- as.character(sample(1e4:9e4, 15))

# Creating a table of finished product SKUs with every row containing a list
# of the components going into them and the quantity per build.
tibble(
  sku = product_list,
  row_rep = list(rep(1, 40))) |>
  unnest(row_rep) |>
  mutate(
    item_code = sample(parts_list, n(), replace = TRUE),
    qty = sample(1:3, n(), prob = c(.8, .15, .05), replace = TRUE),
    src = sample(c('M', 'P'), n(), prob = c(.3, .7), replace = TRUE)) |>
  nest(bom = c(item_code, qty, src)) |>
  select(-row_rep) ->
  bom_list

rm(parts_list, product_list)

# Writing each imaginary BOM to a csv.
if(!dir.exists('rand_boms')) {

  dir.create('rand_boms')

  map2(
    bom_list$bom,
    bom_list$sku,
    ~ write_csv(.x, file = paste0('rand_boms/', .y, '.csv'))
  )
}

rm(bom_list)


