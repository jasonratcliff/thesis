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

#' Count Specimens
#'
#' Tabulate the total number of distinct specimen collections by collector,
#' colleciton number and date while accounting for herbarium duplicates.
#'
#' @param spp_tibble Specimen tibble to count unique specimen vouchers.
#' @export
#'
#' @return Numeric scalar with the number of distinct specimen vouchers.
#'
#' @examples
#' count_specimens(spp_tibble = herbarium_specimens)
#'
count_specimens <- function(spp_tibble) {

  # Calculate total number of specimens accounting for duplicated records.
  specimen_count <- spp_tibble %>%
    dplyr::select("Collector", "Collection_Number", "Date", "Herbarium") %>%
    dplyr::mutate(
      row_id = 1:nrow(.),
      Herbarium = stringr::str_remove_all(
        string = .data$Herbarium,
        pattern = "\\[|\\]"
      )  %>%
        stringr::str_split(string = ., pattern = ", +") %>%
        purrr::map_chr(., function(herbarium) {
          herbarium_split <- unlist(herbarium) %>% sort() %>%
            stringr::str_c(., collapse = " ")
          ifelse(length(herbarium_split) == 1, herbarium_split, "NA")
          })
      ) %>%
    dplyr::distinct(., .data$Collector, .data$Collection_Number,
                    .data$Date, .data$Herbarium, .keep_all = TRUE) %>% nrow()
  return(specimen_count)
}

