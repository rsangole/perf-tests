# <A> Setup ----

# Utilities
library(tictoc)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(fs)

# Packages to Compare
library(qs)
library(data.table)
library(arrow)

# <B> Functions ----
# Data :

# Execution :


# <C> Execute ----

dat <- tibble(.desired_nrows = c(1e4, 1e5),
              .nthreads = 10) %>%
  mutate(datasets = purrr::map(
    .x = .desired_nrows,
    .f = ~ make_data(
      .desired_nrows = .x,
      .cat_A_levels = 20,
      .cat_B_levels = 5
    )
  )) |>
  mutate(results = purrr::map2(datasets, 
                               .nthreads,
                               ~ perform_one_read_write_test(.x, .y)))
dat

results_dat <- dat |> 
  select(-datasets) |> 
  tidyr::unnest(results)
results_dat