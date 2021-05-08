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
html_caption <- function(chunk_type, caption) {
  if (chunk_type == "html") {
    caption <- gsub("\\\\textit\\{", "*", caption)
    caption <- gsub("\\}", "*", caption)
  }
  return(caption)
}

#' Knitr Section Writer
#'
#' Function to return text to stdout based on the knit option of markdown to
#' pandoc conversion for either "html" or "latex" output.  For composing
#' Bookdown documents in HTML or LaTeX output.
#'
#' @param knitr_title Character vector of length one matching the section title.
#' @param knitr_type Character vector of length one matching the section type.
#' @inheritParams html_caption
#'
#' @return Text to stdout with section formatting based on pandoc conversion.
#'
#' @export
#'
knitr_section <- function(knitr_title, knitr_type, chunk_type) {

  # Check `knitr_type` against list of possible section names.
  knitr_chunk_list <- list(section = "##", subsection = "###")
  if (!TRUE %in% (knitr_type %in% names(knitr_chunk_list))) {
    message("\nArgument `knitr_chunk_list` must match one of:\n\n")
    message(paste(names(knitr_chunk_list), collapse = "\n"), "\n")
    stop()
  }

  # Get knit conversion type and output respective section header.
  if (chunk_type == "latex") {
    # Check regular expression pattern in `knitr_title` string for italics.
    if (grepl(" ?\\*[^\\*].+[^\\*]\\* ", knitr_title) &&
        grepl("\\*{2}", knitr_title) == FALSE) {
      split_title <- unlist(strsplit(knitr_title, ""))
      split_index <- grep("\\*", split_title)
      split_fix <- as.logical(seq_along(split_index) %% 2)
      split_title[split_index][split_fix] <- "\\textit{"
      split_title[split_index][which(split_fix == FALSE)] <- "}"
      knitr_title <- paste(split_title, collapse = "")
    }
    cat(paste("\\", knitr_type, "{", knitr_title, "}", sep = ""))
  } else {
    cat(paste(knitr_chunk_list[knitr_type], knitr_title))
  }

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
knitr_table_dna <- function(chunk_type) {
  if (chunk_type == "latex") {
    paste0("\\@ref(tab:TableDnaSpecimensLatex)")
  } else if (chunk_type == "html") {
    paste0("\\@ref(tab:TableDnaSpecimensHtml)")
  }
}

