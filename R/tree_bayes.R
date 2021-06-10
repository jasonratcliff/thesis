# MrBayes ----

#' Build `ggtree` Plot
#'
#' Given output of [read_tree()], build a [ggtree::ggtree()] plot with
#' single- and multi-taxa tip labels.
#'
#' @param joined_ggtree Tibble of `ggtree` nodes with labels and scaled geoms
#'   from DNA specimens output by [join_bayes()].
#' @param ggtree_layout `layout` argument passed to [ggtree::ggtree()].
#' @inheritParams node_geoms
#' @importFrom ggplot2 ggplot geom_point aes_
#' @export
#'
#' @return `ggtree` object built from [read_tree()] and [join_bayes()] outputs.
#'
#' @examples
#' joined_ggtree <-
#'   list.files(system.file("extdata/MrBayes", package = "Thesis"),
#'              full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
#'   read_tree(tree_file = .) %>%
#'   join_bayes(tree_data = ., id_column = "prior_id")
#'
#' # Build ggtre object from MrBayes data import.
#' bayes_ggtree(
#'   joined_ggtree = joined_ggtree,
#'   id_column = "prior_id"
#' )
#'
bayes_ggtree <- function(joined_ggtree, id_column,
                         ggtree_layout = "slanted") {
  nudge_xlim <- -(max(range(joined_ggtree$x)) * 0.05)
  tip_sizes <- joined_ggtree$geom_size %>% purrr::keep(.x = ., ~ !is.na(.x))
  bayes_plot <- ggtree::ggtree(joined_ggtree, layout = ggtree_layout) +
    geom_point(
      data = joined_ggtree %>%
        dplyr::filter(!is.na(.data$label)),
      mapping = aes_(color = as.name(id_column), shape = as.name(id_column)),
      size = tip_sizes
    ) +
    ggplot2::geom_label(
      data = joined_ggtree %>%
        dplyr::filter(.data$prob != 1 & !is.na(.data$prob)),
      nudge_x = nudge_xlim,
      ggplot2::aes(
        label = sprintf("%0.3f", as.numeric(.data$prob), digits = 3)),
      fontface = "bold", fill = "lightgoldenrod", size = 4, alpha = 0.35
    ) +
    ggplot2::geom_text(
      data = joined_ggtree %>% dplyr::filter(!is.na(.data$node_group)) %>%
        dplyr::group_by(.data$node_group) %>% dplyr::slice(1),
      ggplot2::aes(label = .data$node_group),
      hjust = 0, vjust = 0.25, nudge_x = abs(nudge_xlim),
      fontface = "bold", size = 3
    ) +
    ggplot2::geom_text(
      data = joined_ggtree %>% dplyr::filter(is.na(.data$node_group)),
      ggplot2::aes(label = .data$single_label),
      hjust = 0, vjust = 0.25, nudge_x = abs(nudge_xlim),
      fontface = "bold", size = 2.5, na.rm = TRUE
    )
  return(bayes_plot)
}

#' MrBayes `ggtree` Theme Layer
#'
#' Build list of `ggplot` theme options for MrBayes `ggtree` build.
#'
#' @inheritParams bayes_ggtree
#' @inheritParams node_geoms
#' @export
#'
#' @return List of `ggplot` theme specifications for `ggtree` build.
#'
#' @examples
#' joined_ggtree <-
#'   list.files(system.file("extdata/MrBayes", package = "Thesis"),
#'              full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
#'   read_tree(tree_file = .) %>%
#'   join_bayes(tree_data = ., id_column = "prior_id",
#'              scale_vector = c(4, 12))
#'
#' # Add theme layers to ggtree plot.
#' bayes_ggtree(joined_ggtree, id_column = "prior_id") +
#'   bayes_themes(joined_ggtree, id_column = "prior_id")
#'
bayes_themes <- function(joined_ggtree, id_column) {

  # Assign HTML markdown label vector.
  markdown_labels <- Thesis::spl_labels(
    specimen_tbl = joined_ggtree,
    id_column = id_column
  )

  theme_layer <- list(
    ggplot2::scale_color_manual(
      values = Thesis::spp_color, labels = markdown_labels
    ),
    ggplot2::scale_shape_manual(
      values = Thesis::spp_shape, labels = markdown_labels
    ),
    ggplot2::theme(
      legend.text = ggtext::element_markdown(size = 8),
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(
        hjust = 0.5, size = 14, face = "bold")
      ),
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(size = 3),
        ncol = 2, byrow = TRUE, keyheight = 0.15, default.unit = "inch")
    ),
    ggtree::geom_treescale(offset = -1.5)
  )
  return(theme_layer)
}

#' Build ggtree Plot Grid
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Wrapper function to plot legend inset onto ggtree object.
#'
#' @inheritParams bayes_ggtree
#' @inheritParams node_geoms
#' @param scale_name Character scalar to set color and shape legend name.
#' @param x_expand Numeric scalar passed to [ggplot2::expand_limits()].
#' @param plot_x Numeric scalar for plot x position.
#' @param plot_y Numeric scalar for plot y position.
#' @param plot_width Numeric scalar for plot width.
#' @param plot_height Numeric scale for plot height.
#' @param ... Forwarding arguments to [cowplot::draw_plot()].
#' @export
#'
#' @return [ggplot2::ggplot()] with [ggtree::ggtree()] and legend inset.
#'
#' @examples
#' joined_ggtree <-
#'   list.files(system.file("extdata/MrBayes", package = "Thesis"),
#'              full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
#'   read_tree(tree_file = .) %>%
#'   join_bayes(tree_data = ., id_column = "prior_id",
#'              scale_vector = c(4, 12))
#'
#' # Build ggtree plot grid.
#' bayes_plot(joined_ggtree, x_expand = 0.025, plot_y = 0.45,
#'            id_column = "prior_id", scale_name = "Priors")
#'
bayes_plot <- function(joined_ggtree, id_column, scale_name,
                       x_expand = 0.02, plot_x = 0.1, plot_y = 0.5,
                       plot_width = 0.25, plot_height = 0.5, ...) {
  bayes_plot <-
    bayes_ggtree(joined_ggtree = joined_ggtree, id_column = id_column,
                 ggtree_layout = "rectangular") +
    bayes_themes(joined_ggtree = joined_ggtree, id_column = id_column) +
    ggplot2::expand_limits(x = x_expand) +
    ggplot2::labs(colour = scale_name, shape = scale_name)

  # Extract legend and build a plot grid with inset legend.
  bayes_build <- bayes_plot +
    ggtree::theme_tree(legend.position = "none")

  ggtree_legend <- cowplot::get_legend(bayes_plot)

  ggtree_grid <- cowplot::ggdraw(bayes_build) +
    cowplot::draw_plot(plot = ggtree_legend, x = plot_x, y = plot_y,
                       width = plot_width, height = plot_height, ...)
  return(ggtree_grid)
}
