# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(ggplot2)

# Set target options:
tar_option_set(
  packages = c("tibble",
               "tictoc",
               "dplyr",
               "ggplot2",
               "ggrepel",
               "fs",
               "qs",
               "duckdb",
               "DBI",
               "data.table",
               "microbenchmark",
               "vroom",
               "feather",
               "fst",
               "arrow"),
  format = "rds"
)

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

# Replace the target list below with your own:
list(
  tar_target(
    name = desired_nrows,
    command = c(1e5, 1e6, 5e6)
  ),
  tar_target(
    name = data,
    command = make_data(desired_nrows,
                        .cat_A_levels = 20,
                        .cat_B_levels = 5),
    pattern = map(desired_nrows),
    format = "qs"
  ),
  tar_target(
    name = "datatable_classic",
    command = execute_datatable(data,  yaml = FALSE),
    pattern = map(data)
  ),
  tar_target(
    name = "datatable_yaml",
    command = execute_datatable(data, yaml = TRUE),
    pattern = map(data)
  ),
  tar_target(
    name = "qs",
    command = execute_qs(data),
    pattern = map(data)
  ),
  tar_target(
    name = "arrow_parquet",
    command = execute_arrow(data, format = "parquet"),
    pattern = map(data)
  ),
  tar_target(
    name = "arrow_csv",
    command = execute_arrow(data, format = "csv"),
    pattern = map(data)
  ),
  tar_target(
    name = "rds",
    command = execute_rds(data),
    pattern = map(data)
  ),
  tar_target(
    name = "vroom",
    command = execute_vroom(data),
    pattern = map(data)
  ),
  tar_target(
    name = "feather",
    command = execute_feather(data),
    pattern = map(data)
  ),
  tar_target(
    name = "fst",
    command = execute_fst(data),
    pattern = map(data)
  ),
  tar_target(
    name = "duckdb",
    command = execute_duck(data),
    pattern = map(data)
  ),
  tar_target(
    name = "results",
    command = rbind(
      datatable_classic,
      datatable_yaml,
      qs,
      arrow_parquet,
      arrow_csv,
      vroom,
      rds,
      feather,
      fst,
      duckdb
    ) |> 
      mutate(
        read = read / 1e9,
        write = write / 1e9
      )
  ),
  tar_target(
    name = plot_comparison,
    command = plot_method_compare(results)
  ),
  tar_target(
    name = plot_comparison_scaled,
    command = plot_method_compare(results, scale = TRUE)
  ),
  tar_target(
    name = plot_sizes,
    command = plot_method_sizes(results)
  ),
  tar_target(
    name = export,
    command = export_data(plot_comparison,
                          plot_comparison_scaled,
                          plot_sizes),
    format = "file"
  )
)