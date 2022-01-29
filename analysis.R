# Required packages will be installed if they are not already.
pkg <- c("tidyverse", "lpSolve")
invisible(lapply(pkg, function(x) if(!x %in% installed.packages()) install.packages(pkg)))

library(tidyverse)
library(lpSolve)
source('bom_generator.R')

# List of BOM files
files <- fs::dir_ls('rand_boms', regexp = r"(.csv$)")

# Reading and appending them all into a single tibble
files |>
  map_dfr(~ read_csv(.x, col_types = c('c', 'd', 'f')), .id = 'source') |>
  mutate(sku = str_match(source, r"((\d{5})\.csv$)")[,2]) |>
  select(sku, item_code, qty, src) ->
  bom_list

# Creating a randomized component inventory list
components <- bom_list |>
  distinct(item_code) |>
  mutate(
    qty_on_hand = sample(200:500, n())
  )

# Creating a randomized order book
order_book <- bom_list |>
  distinct(sku) |>
  mutate(
    demand = sample(500:1500, n())
  )

# Bringing the demand into the BOM list and summarising by component demand
bom_list |>
  left_join(
    order_book,
    by = 'sku') |>
  group_by(item_code) |>
  summarise(
    total_required = sum(demand * qty),
    used_in = list(sku)) ->
  requirements

# Reshaping the BOM list to create the constraint matrix for optimization.
bom_list |>
  select(sku:qty) |>
  pivot_wider(
    names_from = sku,
    values_from = qty,
    names_sort = TRUE,
    values_fill = 0,
    values_fn = sum) |>
  left_join(
    components,
    by = 'item_code') ->
  pivot

# Preparing the constraint matrix. Consists of components and quantity per build
# for each finished product SKU.
con_mat <- pivot |>
  select(matches(r"(\d{5})")) |>
  data.matrix()

# The objective function is to produce one finished product per build.
f_obj <- rep(1, ncol(con_mat))

# We need to add the constraint to not produce any more than demand
# to our constraint matrix. Adding a diagonal matrix that has the same number of columns
# as the number of finished products allows us to add this systematically.
f_con <- con_mat |>
  rbind(diag(1, ncol(con_mat)))

# This is the direction sign. We can't produce more than what we have parts for,
# and we don't want to produce more than what we have demand for.
f_dir <- rep("<=", nrow(f_con))

# This completes adding the demand as a constraint to our matrix.
# This is why it's important that pivot and order_book are sorted identically.
f_rhs <- c(pivot$qty_on_hand, order_book$demand)


# We can now maximize with lpSolve::lp:
lp_obj <- lp(
  'max',
  f_obj,
  f_con,
  f_dir,
  f_rhs,
  all.int = TRUE
)

# Amount to make of each product
tibble(
  order_book,
  solution = lp_obj$solution
)

# Total amount able to be produced
lp_obj$objval