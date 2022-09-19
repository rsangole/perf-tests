export_data <- function(plot_comparison,
                        plot_comparison_scaled,
                        plot_sizes,
                        outfile = "exports/plots.qs") {
  
  qsave(list(plot_comparison,
             plot_comparison_scaled,
             plot_sizes),
        outfile)
  
  outfile
}