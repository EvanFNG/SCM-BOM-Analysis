Analysing BOMs and Maximizing Production
================
Evan Smith
1/27/2022

### Intro

Manufacturers with less-than-stellar ERP systems still have a need to
analyse inventory levels and create build plans that are achievable and
optimized. With sufficient access to the bill of material (BOM) for each
desired completed product to be built, it is possible to create analyses
that summarise needs and optimize plans based on current and future
inventory levels.

### Requirements

To create analyses of this format, the following are required:

-   List of demand of completed products with their respective SKUs,
    either true demand or a theoretical.

-   BOM for each completed product of interest. A product’s BOM should
    have a list of every component that goes into building the completed
    product.

-   Master lists of on-hand and on-order inventory levels for
    components.

### Methodology

At a high level, we will be combining BOMs, aggregating the components
and summarising by demand for completed products. The packages
`tidyverse` and `lpSolve` will be used. I have also created a file
called `bom_generator.R` that creates a 15 imaginary BOMs of finished
products, each having 40 components with quantities per build assigned.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lpSolve)
source('bom_generator.R')
```

The way that BOMs are outputted from an ERP system / data source may
vary significantly. The BOM examples I’ve created are very simplistic -
component code, quantity required per build of the finished product, and
whether the component is made or purchased. Your BOMs may have other
important variables to include.

In this example, let’s say you are able to retrieve one BOM at a time
from your data source as a .csv file. The file name is the finished
product SKU. For our analysis, we will need to read all the BOMs for the
products of interest into a single table. If the BOMs are contained in a
folder called `rand_boms` that is a sub-folder of our working directory,
this can be accomplished by the following:

``` r
files <- fs::dir_ls("rand_boms")

files |> 
  map_dfr(~ read_csv(.x, col_types = cols('c', 'd', 'f')), .id = "source") |> 
  slice_head(n = 5)
```

    ## # A tibble: 5 x 4
    ##   source              item_code   qty src  
    ##   <chr>               <chr>     <dbl> <fct>
    ## 1 rand_boms/11203.csv 327182        1 P    
    ## 2 rand_boms/11203.csv 300421        1 P    
    ## 3 rand_boms/11203.csv 470481        1 P    
    ## 4 rand_boms/11203.csv 237694        2 P    
    ## 5 rand_boms/11203.csv 248440        1 M

In the above code chunk, we’ve read all the BOM .csv files in
`rand_boms` and appended them into a single table by use of the
`purrr::map_dfr` function. Notice that the `source` column includes the
full file path. We will want to have the SKU as its own variable by
doing the following:

``` r
files |> 
  map_dfr(~ read_csv(.x, col_types = cols('c', 'd', 'f')), .id = "source") |>
  mutate(sku = str_extract(source, r"(\d{5})")) |> 
  select(sku, item_code, qty, src) ->
  clean_boms

clean_boms |> 
  slice_head(n = 5)
```

    ## # A tibble: 5 x 4
    ##   sku   item_code   qty src  
    ##   <chr> <chr>     <dbl> <fct>
    ## 1 11203 327182        1 P    
    ## 2 11203 300421        1 P    
    ## 3 11203 470481        1 P    
    ## 4 11203 237694        2 P    
    ## 5 11203 248440        1 M

We now need to add some more information: demand for finished products,
and component inventory.

``` r
components <- clean_boms |> 
  distinct(item_code) |> 
  mutate(
    qty_on_hand = sample(50:300, n(), replace = TRUE)
  )

order_book <- clean_boms |> 
  distinct(sku) |> 
  mutate(
    demand = sample(500:2000, n())
  )
```

We now join the order book into the BOM list to find the aggregate
demand of the parts:

``` r
clean_boms |> 
  left_join(
    order_book,
    by = "sku") |> 
  group_by(item_code) |> 
  summarise(
    total_required = sum(demand * qty),
    products_used_in = list(sku)) ->
  aggregate_demand

aggregate_demand |> 
  slice_head(n = 5)
```

    ## # A tibble: 5 x 3
    ##   item_code total_required products_used_in
    ##   <chr>              <dbl> <list>          
    ## 1 104619              3429 <chr [2]>       
    ## 2 119391              4601 <chr [3]>       
    ## 3 120192              4283 <chr [3]>       
    ## 4 125558              7709 <chr [4]>       
    ## 5 131516              8312 <chr [5]>

The above returns the total demand for all components that go into our
15 product SKUs. Additionally, `products_used_in` is a list column
detailing which finished products the components goes into. This detail
can be viewed by opening the data set in the global environment, or by
`tidyr::unnest`.

### Optimizing Production

By adding in the component inventory and reshaping the data, we can
maximize production output.

``` r
clean_boms |> 
  select(sku:qty) |> 
  pivot_wider(
    names_from = sku,
    values_from = qty,
    names_sort = TRUE,
    values_fill = 0,
    values_fn = sum) |> 
  left_join(
    components,
    by = "item_code") ->
  pivot

con_mat <- data.matrix(select(pivot, matches(r"(\d{5})")))

f_obj <- rep(1, ncol(con_mat))

f_con <- rbind(con_mat, diag(1, ncol(con_mat)))

f_dir <- rep("<=", nrow(f_con))

# Order book must be sorted by SKU ascending, just as with the other lists.
f_rhs <- c(pivot$qty_on_hand, order_book$demand)
```

Optimize with `lpSolve::lp`:

``` r
lp_obj <- lp(
  'max',
  f_obj,
  f_con,
  f_dir,
  f_rhs,
  all.int = TRUE)

lp_obj$objval
```

    ## [1] 151

``` r
solution <- tibble(
  order_book,
  solution = lp_obj$solution
)

solution
```

    ## # A tibble: 15 x 3
    ##    sku   demand solution
    ##    <chr>  <int>    <dbl>
    ##  1 11203   1585       19
    ##  2 11672   1135       10
    ##  3 12449   1877        0
    ##  4 16006   1107       24
    ##  5 29850    692        5
    ##  6 54499   1507        0
    ##  7 56467   1406        7
    ##  8 58433   1552       19
    ##  9 63271    974        0
    ## 10 65991    971       14
    ## 11 70905    504        0
    ## 12 76595   1461       12
    ## 13 86791    924        5
    ## 14 88714   1807       20
    ## 15 88966   1314       16
