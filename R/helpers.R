#' Save Figures
#'
#' Compiled PDF and HTML documents require different graphics devices.
#' To facilitate `Makefile` automation, save both *.pdf* and *.png* files
#' from input ggplot. Non-standard evaluation with `rlang` is used for naming
#' files from the input `ggplot` object name.
#'
#' @param gg_plot Input plot to save ggplot graphic *.png* and *.pdf* figures.
#' @export
#'
save_plot <- function(gg_plot) {
  gg_plot <- rlang::enquo(gg_plot)
  ggplot2::ggsave(
    filename = fs::path("Figs", rlang::as_label(gg_plot), ext = "png"),
    plot = rlang::eval_tidy(gg_plot), device = "png"
  )
  ggplot2::ggsave(
    filename = fs::path("Figs", rlang::as_label(gg_plot), ext = "pdf"),
    plot = rlang::eval_tidy(gg_plot), device = "pdf"
  )
}

