# Function to replace LaTeX italics with HTML markdown syntax.
# Use for figure captions replacing specific epithet italicization.
knitr_caption <- function(caption) {
  chunk_type <- opts_knit$get("rmarkdown.pandoc.to")
  if (chunk_type == "html") {
    caption <- gsub("\\\\textit\\{", "*", caption)
    caption <- gsub("\\}", "*", caption)
  }
  return(caption)
}

# Function to output chunk label for HTML rendering.
knitr_chunk_html <- function(html_chunk) {
  chunk_type <- opts_knit$get("rmarkdown.pandoc.to")
  if (chunk_type == "html") {
    cat(html_chunk)
  }
}

#' Knitr section writer
#'
#' Function to return text to stdout based on the knit option of markdown to
#' pandoc conversion for either "html" or "latex" output.  For composing
#' Bookdown documents in HTML or LaTeX output.
#'
#' @param knitr_title Character vector of length one matching the section title.
#' @param knitr_type Character vector of length one matching the section type.
#'
knitr_section <- function(knitr_title, knitr_type) {

  # Check `knitr_type` against list of possible section names.
  knitr_chunk_list <- list(section = "##", subsection = "###")
  if (!TRUE %in% (knitr_type %in% names(knitr_chunk_list))) {
    message("\nArgument `knitr_chunk_list` must match one of:\n\n")
    message(paste(names(knitr_chunk_list), collapse = "\n"), "\n")
    stop()
  }

  # Get knit conversion type and output respective section header.
  knitr_chunk_type <- opts_knit$get("rmarkdown.pandoc.to")
  if (knitr_chunk_type == "html") {
    cat(paste(knitr_chunk_list[knitr_type], knitr_title))
  } else if (knitr_chunk_type == "latex") {
    cat(paste("\\", knitr_type, "{", knitr_title, "}", sep = ""))
  }
}

