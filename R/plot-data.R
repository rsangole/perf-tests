plot_method_compare <- function(results, 
                                scale = FALSE) {
  
  ylab <- ifelse(scale,
                 "Speed Comparison",
                 "log(time)")
  suff <- ifelse(scale, "x", " s")
  tran <- ifelse(scale, "log10", "log10")
  
  apply_scale <- function(dat, scale){
    if(scale){
      dat <- dat |> 
        group_by(nrow) |> 
        mutate(
          read_lower = read_lower / min(read),
          read_upper = read_upper / min(read),
          write_lower = write_lower / min(write),
          write_upper = write_upper / min(write),
          read = read / min(read),
          write = write / min(write)
        )
    }
    dat
  }
  
  results |>
    dplyr::mutate(nrow = factor(
      nrow,
      labels = scales::label_comma()(unique(nrow))),
      method = stringr::str_replace(method, "_", " ")) |>
    group_by(nrow, method) |>
    summarise(
      read_lower = min(read),
      read_upper = max(read),
      write_lower = min(write),
      write_upper = max(write),
      read = median(read),
      write = median(write),
    ) |>
    apply_scale(scale) |> 
    group_by(nrow) |> 
    mutate(
      read_winner = read == min(read),
      write_winner = write == min(write)
    ) |> 
    ggplot(aes(x = stringr::str_wrap(method, width = 1))) +
    geom_pointrange(aes(y = read,
                        ymin = read_lower,
                        ymax = read_upper,
                        color = "Read"),
                    position = position_nudge(x = -0.1))+
    geom_pointrange(aes(y = write,
                        ymin = write_lower,
                        ymax = write_upper,
                        color = "Write"),
                    position = position_nudge(x = 0.1))+
    geom_point(aes(y = ifelse(read_winner == TRUE, read, NA)),
               pch = 21,
               size = 4,
               position = position_nudge(x = -0.1))+
    geom_point(aes(y = ifelse(write_winner == TRUE, write, NA)),
               pch = 21,
               size = 4,
               position = position_nudge(x = 0.1))+
    labs(x = NULL,
         y = ylab,
         color = NULL,
         title = "Performance Comparison",
         caption = sprintf("Circles highlight the winners\nEach read & write experiment consists of %d runs", results |> group_by(method, nrow) |> count() |> pull(n) |> unique())) +
    theme_bw() +
    theme(plot.title.position = "plot",
          legend.position = "top",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(size = 0.3)) +
    facet_wrap( ~ nrow, scales = "free", ncol = 1) +
    scale_y_continuous(labels = scales::label_number(suffix = suff),
                       trans = tran)
}

plot_method_sizes <- function(dat) {
  
  dat <- dat |>
    dplyr::select(method, nrow, size_mb) |>
    dplyr::distinct() |> 
    dplyr::mutate(
      method = stringr::str_replace(method, "_", " ") |> 
        stringr::str_wrap(width = 1)
    )
  
  method_sorted <- dat |>
    filter(nrow == max(nrow)) |>
    distinct() |> 
    arrange(size_mb) |>
    pull(method)
  
  dat |>
    dplyr::mutate(
      method = factor(method, levels = method_sorted),
      nrow = factor(nrow, labels = scales::label_comma()(unique(nrow)))) |>  
    ggplot(aes(x = method, 
               y = size_mb)) +
    geom_point(aes(color = nrow), size = 5) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("Mb"))) +
    theme_bw() +
    theme(legend.position = "top") +
    labs(
      x = NULL,
      y = "File Size",
      color = NULL
    )
}