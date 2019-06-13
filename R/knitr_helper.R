#' Knitr caption writer
#'
#' Function to replace LaTeX italics with HTML markdown for firgure captions.
#'
knitr_caption <- function(caption) {
  chunk_type <- opts_knit$get("rmarkdown.pandoc.to")
  if (chunk_type == "html") {
    caption <- gsub("\\\\textit\\{", "*", caption)
    caption <- gsub("\\}", "*", caption)
  }
  return(caption)
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
  }
}

#' Table 2 Bookdown
#'
#' Methods table reference for DNA specimens.
knitr_table2 <- function() {
  if (knitr_chunk == "latex") {
    paste0("\\@ref(tab:methodsTable2DnaSpecimens)")
  } else if (knitr_chunk == "html") {
    paste0("\\@ref(tab:methodsTable2DnaSpecimensHtml)")
  }
}

