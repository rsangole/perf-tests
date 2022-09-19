make_data <- function(.desired_nrows,
                      .cat_A_levels,
                      .cat_B_levels) {
  data.table(
    cat_A = sample(datasets::state.name[1:.cat_A_levels], .desired_nrows, replace = TRUE),
    cat_B = sample(LETTERS[1:.cat_B_levels], .desired_nrows, replace = TRUE),
    date = sample(
      seq(Sys.Date(), Sys.Date() + 364, by = "1 day"),
      .desired_nrows,
      replace = TRUE
    ),
    num_1 = runif(n = .desired_nrows),
    num_2 = runif(n = .desired_nrows),
    num_3 = runif(n = .desired_nrows),
    num_4 = runif(n = .desired_nrows),
    chr_1 = sample(letters, .desired_nrows, replace = TRUE),
    chr_2 = sample(letters, .desired_nrows, replace = TRUE),
    chr_3 = sample(letters, .desired_nrows, replace = TRUE),
    chr_4 = sample(letters, .desired_nrows, replace = TRUE)
  )
}