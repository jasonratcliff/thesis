#' Save Figures
#'
#' Compiled PDF and HTML documents require different graphics devices.
#' To facilitate `Makefile` automation, save both *.pdf* and *.png* files
#' from input ggplot. Non-standard evaluation with `rlang` is used for naming
#' files from the input `ggplot` object name.
#'
#' @param gg_plot Input plot to save ggplot graphic *.png* and *.pdf* figures.
#' @param ... Forwarding arguments to [ggplot2::ggsave()].
#' @export
#'
#' @keywords internal
save_plot <- function(gg_plot, ...) {
  gg_plot <- rlang::enquo(gg_plot)
  ggplot2::ggsave(
    filename = fs::path("Figs", rlang::as_label(gg_plot), ext = "png"),
    plot = rlang::eval_tidy(gg_plot), device = "png", ...
  )
  ggplot2::ggsave(
    filename = fs::path("Figs", rlang::as_label(gg_plot), ext = "pdf"),
    plot = rlang::eval_tidy(gg_plot), device = "pdf", ...
  )
}

#' Capitalize String Character
#'
#' Given an input character vector, capitalize the first element character.
#'
#' @param character_vector Character vector to capitalize.
#' @export
#' @family text
#'
#' @examples
#' traits <- c("oblong", "elliptic", "linear")
#' capitalize(character_vector = traits)
#'
#' @keywords internal
capitalize <- function(character_vector) {
  purrr::map_chr(.x = character_vector, function(string_element) {
    split_string <- strsplit(x = string_element, split = "") %>% unlist()
    paste0(c(toupper(split_string[1]), split_string[2:length(split_string)]),
      collapse = ""
    )
  })
}

#' LaTeX Italicization
#'
#' Wrapper to italicize vector based on `knitr` chunk type.
#'
#' @param unitalicized Character vector to italicize.
#' @inheritParams html_caption
#' @export
#' @family text
#'
#' @examples
#' italicize(unitalicized = "Physaria floribunda", chunk_type = "latex")
#'
#' @keywords internal
italicize <- function(unitalicized, chunk_type) {
  if (chunk_type == "latex") {
    paste0("\\textit{", unitalicized, "}")
  } else {
    paste0("*", unitalicized, "*")
  }
}

#' Replace LaTeX Italics
#'
#' Convert LaTeX italics to HTML markdown for firgure captions.
#'
#' @param chunk_type Knitr chunk type returned by
#'   `opts_knit$get("rmarkdown.pandoc.to")`
#' @param caption Character scalar passed to `knitr::kable()` `caption` arg.
#'
#' @return Caption formatted by knitr pandoc conversion type.
#'
#' @export
#'
#' @keywords internal
html_caption <- function(chunk_type, caption) {
  if (chunk_type == "html") {
    caption <- gsub("\\\\textit\\{", "*", caption)
    caption <- gsub("\\}", "*", caption)
  }
  return(caption)
}

#' Table 2 Bookdown
#'
#' Methods table reference for DNA specimens.
#'
#' @inheritParams html_caption
#' @return Table 2 reference based on type of pandoc conversion.
#'
#' @export
#'
#' @keywords internal
knitr_table_dna <- function(chunk_type) {
  if (chunk_type == "latex") {
    paste0("\\@ref(tab:TableDnaSpecimensLatex)")
  } else if (chunk_type == "html") {
    paste0("\\@ref(tab:TableDnaSpecimensHtml)")
  }
}
