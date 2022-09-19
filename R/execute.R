execute_datatable <- function(dat,
                              yaml = FALSE,
                              file = "temp/data.csv",
                              .nthreads = 10,
                              mb_times = 10) {
  
  method = ifelse(yaml, "datatable_classic", "datatable_yaml")
  
  write_time <- microbenchmark(fwrite(
    x = dat,
    file = file,
    yaml = yaml,
    nThread = .nthreads
  ),
  times = mb_times) |>
    as_tibble()
  
  read_time <- microbenchmark({
    x = fread(file = file,
              yaml = yaml,
              nThread = .nthreads)
  },
  times = mb_times) |>
    as_tibble()
  
  tibble(
    method = method,
    nrow = nrow(dat),
    read = read_time$time,
    write = write_time$time,
    size_mb = file.size(file) / 1024 ^ 2
  )
}

execute_qs <- function(dat,
                       file = "temp/data.qs",
                       .nthreads = 10,
                       mb_times = 10) {
  write_time <- microbenchmark(qsave(
    x = dat,
    file = file,
    nthreads = .nthreads
  ),
  times = mb_times) |>
    as_tibble()
  
  read_time <- microbenchmark({
    x = qread(file = file, nthreads = .nthreads)
  },
  times = mb_times) |>
    as_tibble()
  
  tibble(
    method = "qs",
    nrow = nrow(dat),
    read = read_time$time,
    write = write_time$time,
    size_mb = file.size(file) / 1024 ^ 2
  )
}

execute_arrow <- function(dat,
                          format = c("parquet", "csv"),
                          path = "temp/arrow_",
                          mb_times = 10) {
  format = match.arg(format)
  path = paste0(path, format)
  method = paste0("arrow_", format)
  
  write_time <- microbenchmark(write_dataset(dataset = dat,
                                             path = path,
                                             format = format),
                               times = mb_times) |>
    as_tibble()
  
  read_time <- microbenchmark({
    x = open_dataset(sources = path,
                     format = format) |>
      collect()
  },
  times = mb_times) |>
    as_tibble()
  
  tibble(
    method = method,
    nrow = nrow(dat),
    read = read_time$time,
    write = write_time$time,
    size_mb = fs::dir_ls(path, recurse = T) |>
      file_size() / 1024 ^ 2 |>
      sum() |>
      as.numeric()
  )
}

execute_rds <- function(dat,
                        file = "temp/dat.rds",
                        mb_times = 10) {
  write_time <- microbenchmark(saveRDS(object = dat, file = file),
                               times = mb_times) |>
    as_tibble()
  
  read_time <- microbenchmark({
    x = readRDS(file = file)
  },
  times = mb_times) |>
    as_tibble()
  
  tibble(
    method = "rds",
    nrow = nrow(dat),
    read = read_time$time,
    write = write_time$time,
    size_mb = file.size(file) / 1024 ^ 2
  )
}


execute_vroom <- function(dat,
                          file = "temp/dat.csv",
                          mb_times = 10) {
  write_time <- microbenchmark(vroom_write(dat, 
                                           file = file, 
                                           delim = ","),
                               times = mb_times) |>
    as_tibble()
  
  read_time <- microbenchmark({
    x = vroom(file = file, delim = ",")
  },
  times = mb_times) |>
    as_tibble()
  
  tibble(
    method = "vroom",
    nrow = nrow(dat),
    read = read_time$time,
    write = write_time$time,
    size_mb = file.size(file) / 1024 ^ 2
  )
}
