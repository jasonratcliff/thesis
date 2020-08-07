# Themes | Scales ----

#' Generate HTML vector for
#' \link[ggtext:element_markdown]{element_markdown()}
#'
#' Requires installation of \link{ggtext} as follows:
#'   remotes::install_github("clauswilke/ggtext")
#'
#' @inheritParams layer_specimens
#' @export
#'
#' @return Character vector of html markup named by specimen identification.
#'
#' @examples
#' spl_labels(specimen_tbl = spp_co_front_range,
#'            id_column = "prior_id")
#'
spl_labels <- function(specimen_tbl, id_column) {

  label_markdown <- function(label_vector) {
    purrr::map_chr(.x = label_vector, .f = function(label) {
      split_label <- unlist(strsplit(label, " "))
      if (length(split_label) %in% c(1, 2)) {
        # Genus with or without specific epithet.
        parsed_label <- paste0("*", label, "*")
      } else {
        if (grepl("ssp\\.", x = label)) {
          # Add html formatting to split ssp. onto second line.
          parsed_label <-
            paste0("*", paste0(split_label[1:2], collapse = " "),
                   "*", collapse = "") %>%
            paste0(., gsub(pattern = "s?sp\\.|var\\.", x = split_label[3],
              replacement = "<br><span>&nbsp;&nbsp;&nbsp;</span>  ssp\\. *"),
              split_label[4:length(split_label)], "*")
        } else {  # Any other cases
          parsed_label <- paste0("*", label, "*")
        }
      }
      return(parsed_label)
    })
  }

  # Add italics, html line break and non-breaking space characters.
  labels_vector <- specimen_tbl %>%
    dplyr::select(., !!id_column) %>% dplyr::pull() %>% unique()
  labels_html <- label_markdown(labels_vector)
  names(labels_html) <- labels_vector

  # Return named vector of final identifications and markdown expression.
  return(labels_html)
}

#' Add map ggplot scales and themes
#'
#' Add manual values to specimen identification discrete scales in map ggplots
#' set by \link{spp_color} and \link{spp_shape} vectors. Modify plot themes
#' and add markdown italics for specific epithets with \link{spl_labels}.
#'
#' @param legend_title Character vector of length one to set the ggplot
#'   legend title.
#' @inheritParams layer_specimens
#' @export
#'
#' @return List of scale layers (colour, shape), themes, and axis labels.
#'
#' @examples
#' # Add theme specifications and markdown legend.
#' ggplot2::ggplot() +
#'   layer_borders(spl_extent = spl_bbox(spp_co_front_range),
#'                 sf_county_color = "black") +
#'   layer_specimens(specimen_tbl = spp_co_front_range, shape_aes = TRUE,
#'                   id_column = "Taxon_a_posteriori") +
#'   layer_themes(specimen_tbl = spp_co_front_range,
#'                id_column = "Taxon_a_posteriori",
#'                legend_title = "Reviewed Annotations")
#'
layer_themes <- function(specimen_tbl, id_column, legend_title) {

  # Assign HTML markdown label vector.
  markdown_labels <- ThesisPackage::spl_labels(specimen_tbl = specimen_tbl,
                                               id_column = id_column)

  # Build ggplot scale and theme layers.
  theme_layer <- list(
    ggplot2::scale_color_manual(name = legend_title, labels = markdown_labels,
      values = ThesisPackage::spp_color, na.value = "black"),
    ggplot2::scale_shape_manual(name = legend_title, labels = markdown_labels,
      values = ThesisPackage::spp_shape, na.value = 17),

    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color =  "black"),
      legend.text.align = 0, legend.title.align = 0.5,
      legend.direction = "vertical",
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "grey90",
                                                color =  "black"),
      legend.text = ggtext::element_markdown()),

    ggplot2::labs(x = "Longitude", y = "Latitude")
    )
  return(theme_layer)
}

# Legend ----

#' Extract specimen layer legend
#'
#' Build a minimal specimen layer ggplot with themes and extract the legend.
#'
#' @inheritParams layer_specimens
#' @inheritParams layer_themes
#' @export
#'
#' @examples
#' cowplot::plot_grid(
#'   spl_legend(specimen_tbl = spp_co_front_range,
#'              id_column = "Taxon_a_posteriori",
#'              legend_title = "Review Annotations",
#'              shape_aes = TRUE, geom_size = 3)
#' )
#'
spl_legend <- function(specimen_tbl, id_column, legend_title,
                       shape_aes, geom_size) {
  gg_legend <-
    cowplot::get_legend(
      plot = ggplot2::ggplot() +
        layer_specimens(specimen_tbl = specimen_tbl, id_column = id_column,
                        shape_aes = shape_aes, geom_size = geom_size) +
        layer_themes(specimen_tbl = specimen_tbl, id_column = id_column,
                     legend_title = legend_title) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
    )
  return(gg_legend)
}

