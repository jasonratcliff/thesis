# Call expression wrapper over ggrepel geoms

#' Layer [ggrepel] label geoms
#'
#' Leverage [rlang] to build call expressions, mapping a [tibble] with
#' x- and y-axis nugdes for each point layered onto an initial [ggplot].
#' Points represent specimen sampling, with labels identifying individual
#' records of interest.
#'
#' @param label_nudges Inline [tibble::tribble()] with variables to set
#'   `nudge_x`, `nudge_y`, `segment.curvature`, and variable to join by:
#'       - For maps: `Key`
#'       - For trees: `node`
#'   TODO consider abstraction (... implementation?) of calls to `ggrepel`
#' @param map_labels A [tibble] input for `ggplot` with `x`, `y`, and `label`
#'   aesthetics passed to [ggrepel::geom_label_repel()]. 
#' @param initial_ggplot Base [ggplot2::ggplot()] for [purrr::map()] and [reduce()]
#'   into [rlang::call2()] for building R call expressions.
#' @export
#'
#' @return List of R calls to build layered label repel calls.
#'
#' @examples
#' extent <-
#'   tibble::tribble(
#'     ~"Longitude", ~"Latitude",
#'     -108, 39.75,
#'     -105, 39.75,
#'     -108, 41.75,
#'     -105, 41.75
#'   )
#'
#' specimens <- Thesis::herbarium_specimens %>%
#'   subset_coords(
#'     specimen_tbl = .,
#'     Longitude = c(-108, -105),
#'     Latitude = c(39.75, 41.75)
#'      )
#'
#' jackson <-
#'   Thesis::find_spp(
#'     specimen_tbl = Thesis::herbarium_specimens,
#'     collector = "Kastning|Nelson",
#'     collection = "1462|1725|49286|49478"
#'   ) %>%
#'   dplyr::select(Collector, Collection_Number, Longitude, Latitude) %>%
#'   dplyr::mutate(
#'     Label = stringr::str_remove_all(
#'       string = Collector,
#'       pattern = "[A-Z]\\. ?"
#'     ) %>%
#'       gsub("with|and", "&", x = .) %>%
#'       paste(., Collection_Number, sep = "\n"),
#'     Collection_Number = as.numeric(.data$Collection_Number),
#'     Key = stringr::str_replace_all(
#'       string = .data$Label,
#'       pattern = "[^A-z0-9]+",
#'       replacement = "_"
#'     )
#'   )
#'
#' map <- ggplot() +
#'  # layer_borders(spl_extent = extent,sf_county_color = "black") +
#'   layer_specimens(
#'     specimen_tbl = specimens,
#'     id_column = "Taxon_a_posteriori"
#'   ) +
#'   geom_point(
#'     data = jackson,
#'     mapping = aes(x = Longitude, y = Latitude),
#'     shape = 5, color = "black", size = 4
#'   ) +
#'   coord_sf(
#'     xlim = range(extent$Longitude),
#'     ylim = range(extent$Latitude)
#'   ) +
#'   theme_classic()
#'
#' tibble::tribble(
#'   ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"Key",
#'     0.4, -0.05, 0.1,  "Nelson_49286",
#'     -1, -0.25, 0.1, "Kastning_Kastning_1462",
#'     -1, -0.25, 0.1, "Kastning_Culp_1725",
#'     -0.4, -0.1, -0.1, "Nelson_49478",
#'   ) %>%
#'   repel_map_labels(
#'     label_nudges = .,
#'     map_labels = jackson,
#'     initial_ggplot = map
#'   ) %>%
#'   rlang::eval_tidy(expr = .)
#'
repel_map_labels <- function(label_nudges, map_labels, initial_ggplot) {
  stopifnot(identical(class(initial_ggplot), c("gg", "ggplot")))
  repelled_ggplot <- dplyr::left_join(
    x = label_nudges,
    y = map_labels,
    by = "Key"
  ) %>%
    purrr::pmap(
      .l = .,
      .f = function(
        Longitude, Latitude, Label,
        nudge_x, nudge_y, segment.curvature, ...
      ) {
        rlang::call2(
          .fn = ggrepel::geom_label_repel,
          data = tibble::tibble(
            Longitude = Longitude,
            Latitude = Latitude,
            Label = Label
          ),
          mapping = ggplot2::aes(
            x = Longitude,
            y = Latitude,
            label = Label
          ),
          nudge_x = nudge_x,
          nudge_y = nudge_y,
          segment.curvature = segment.curvature,
          box.padding = 0.5,
          alpha = 0.66,
          segment.color = "white"
        )
      }) %>%
    purrr::reduce(
      .x = .,
      .f = ~ rlang::expr(!!.x + !!.y),
      .init = initial_ggplot
    )
}


haplotype_labels <- function(haplotypes) {

  grouped_haplotypes <- haplotypes %>%

    # Filter to nodes with multiple taxa, then count instances of reviewed IDs.
    dplyr::group_by(node) %>%
    dplyr::mutate(nodes = dplyr::n()) %>%
    dplyr::filter(.data$nodes > 1 & !is.na(.data$label)) %>%
    dplyr::group_by(node, Taxon_a_posteriori) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select("node", "x", "y", "Taxon_a_posteriori", "n", node_group) %>%
    dplyr::distinct(.keep_all = TRUE) %>%

    # Create labels with plotmath expressions to parse expressions into text.
    dplyr::mutate(
      Label = paste(
        .data$Taxon_a_posteriori,
        ifelse(
          # Add new line for taxa with subspecific designations.
          test = grepl(pattern = "subsp.", x = .data$Taxon_a_posteriori),
          yes = "", no = "\n"
        ),
        paste0("(n==", .data$n, ")")
      ) %>%
        purrr::map_chr(.x = ., .f = function(label) {
          epithet <-
            stringr::str_extract(
              string = label,
              pattern = "(?<=Physaria )[a-z]+"
            )
          subsp <-
            ifelse(
              test = grepl(pattern = "subsp\\.", x = label),
              yes = stringr::str_extract(
                string = label,
                pattern = ("(?<=subsp. )[a-z]+")
              ),
              no = ""
            )
          plotmath <-
            paste(
              "italic(P.",
              ifelse(
                test = grepl(pattern = "didymocarpa", x = epithet),
                yes = paste0("d.) subsp. italic(", subsp, ")"),
                no = paste0(epithet, ")")
              )
            ) %>%
              paste(.,
                stringr::str_extract(
                  string = label,
                  pattern = "\\(n ?==.+$"
                )
              ) %>%
              gsub(
                pattern = " +",
                replacement = "~",
                x = .
              )
          return(plotmath)
          })
    ) %>%
    dplyr::arrange(.data$node)
  return(grouped_haplotypes)
}


repel_haplotype_labels <- function(label_nudges, grouped_haplotypes,
                                   initial_ggtree, label_size = 2) {
  # Check tree object class and join variables as expected.
  stopifnot(identical(class(initial_ggtree), c("ggtree", "gg", "ggplot")))
  stopifnot(
    unique(
      c("node", "Taxon_a_posteriori") %in%
        names(label_nudges)
    ) == TRUE
  )

  # Build call for `ggrepel::geom_label_repel()` layer expression.
  repelled_ggplot <-
    dplyr::left_join(
      x = label_nudges,
      y = grouped_haplotypes,
      by = c("node", "Taxon_a_posteriori")
    ) %>%
      purrr::pmap(
        .l = .,
        .f = function(
          x, y, Label, Taxon_a_posteriori, color,
          nudge_x, nudge_y, segment.curvature, ...
        ) {
          rlang::call2(
           .fn = ggrepel::geom_label_repel,
            data = tibble::tibble(
              x = x,
              y = y,
              Label = Label
           ),
           mapping = ggplot2::aes(
             x = x,
             y = y,
             label = Label,
             fill = Taxon_a_posteriori
           ),
           xlim = c(-Inf, Inf),
           ylim = c(-Inf, Inf),
           nudge_x = nudge_x,
           nudge_y = nudge_y,
           alpha = 1,
           box.padding = 0.65,
           color = color,
           parse = TRUE,
           segment.color = "black",
           segment.curvature = segment.curvature,
           segment.shape = 0.5,
           show.legend = FALSE,
           size = label_size
          )
       }
      ) %>%
    purrr::reduce(
      .x = .,
      .f = ~ rlang::expr(!!.x + !!.y),
      .init = initial_ggtree
    )
  return(repelled_ggplot)
}

