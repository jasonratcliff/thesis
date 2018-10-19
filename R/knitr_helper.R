# Function to replace LaTeX italics with HTML markdown syntax.
# Use for figure captions replacing specific epithet italicization.
knitr_caption <- function(latex_caption) {
  chunk_type <- opts_knit$get("rmarkdown.pandoc.to")
  if (chunk_type == "latex") {
    html_cap <- gsub("\\\\textit\\{", "*", latex_caption)
    html_cap <- gsub("\\}", "*", html_cap)
  }
  return(html_cap)
}

# Function to output chunk label for HTML rendering.
knitr_chunk_html <- function(html_chunk) {
  chunk_type <- opts_knit$get("rmarkdown.pandoc.to")
  if (chunk_type == "html") {
    cat(html_chunk)
  }
}
