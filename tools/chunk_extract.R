library(dplyr)
library(fs)
library(purrr)
library(readr)

rmd_chunks <- list.files(
  path = "inst/manuscript",
  pattern = "0[1-6](\\.[12])?-[A-Z]+.Rmd",
  full.names = TRUE
) %>%
  purrr::map_dfr(.x = ., function(filepath) {
    chunks <- readr::read_lines(file = filepath) %>%
      purrr::keep(.x = ., 
        ~ grepl(
          pattern = "(```\\{r,?)(.*?)",
          x = .x
        )
      ) %>%
      gsub("```\\{r,?|\\}", "", x = .)
    dplyr::bind_rows(chunk = chunks)
  }) %>%
  
  dplyr::filter(grepl(pattern = "fig.s?cap", x = .data$chunk)) %>%

  dplyr::mutate(
    name = stringr::str_extract(
      string =  .data$chunk,
      pattern = "[:alnum:]+(?=[, ]?)"
    ),
    fig_scap = stringr::str_extract(
      string = .data$chunk,
      pattern = 'fig\\.scap=\\"(.*?)\\"'
    ),
    fig_cap = stringr::str_extract(
      string = .data$chunk,
      pattern = 'fig\\.cap=(.*?)(?=[, ])'
    )
  ) %>%

  dplyr::select(name, fig_scap, fig_cap, chunk) %>%
  
  dplyr::arrange(.data$name) %>%

  readr::write_excel_csv(
    x = .,
    file = fs::path("inst/issues/issue-28", ext = "csv")
  )

