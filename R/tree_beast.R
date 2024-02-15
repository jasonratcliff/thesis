# BEAST ----

#' BEAST Probability Tree Aesthetic
#'
#' Build [ggtree::geom_tree()] layer with posterior probabilities from
#' BEAST results.
#'
#' @return [ggtree::ggtree()] aesthetic layers for posterior probabilities.
#' @export
#'
#' @examples
#' list.files(
#'   path = system.file("extdata/BEAST", package = "thesis"),
#'   pattern = "multi-locus.combined.mcc", full.names = TRUE
#' ) %>%
#'   read_tree(tree_file = .) %>%
#'   ggtree::ggtree(tr = .) +
#'   beast_posterior()
#'
#' @keywords internal
beast_posterior <- function() {
  posterior_list <-
    list(
      ggtree::geom_tree(ggplot2::aes(color = .data$posterior)),
      ggplot2::scale_color_gradientn(
        name = "Posterior\nProbablility",
        colors = c("red", "orange", "green", "cyan", "blue"),
        guide = ggplot2::guide_colourbar(order = 1)
      )
    )
  return(posterior_list)
}

#' BEAST Specimen Labels
#'
#' Build specimen tip and node labels with posterior probability > 0.5.
#' Includes both prior and reviewed annotations.
#'
#' @inheritParams node_labels
#' @export
#'
#' @return [ggtree::ggtree()] aesthetic layers for specimen labels.
#'
#' @examples
#' tree_data <-
#'   list.files(
#'     path = system.file("extdata/BEAST", package = "thesis"),
#'     pattern = "multi-locus.combined.mcc", full.names = TRUE
#'   ) %>%
#'   read_tree(tree_file = .)
#'
#' ggtree::ggtree(tr = tree_data) +
#'   beast_labels(tree_data = tree_data)
#'
#' @keywords internal
beast_labels <- function(tree_data) {
  label_list <-
    list(
      ggtree::geom_tiplab(offset = 0.0005, align = TRUE, size = 3),
      ggnewscale::new_scale_color(),
      ggplot2::geom_point(
        data = dplyr::filter(tree_data, !is.na(.data$prior_id)),
        ggplot2::aes(
          color = .data$prior_id,
          shape = .data$prior_id
        ), size = 5, alpha = 0.5
      ),
      ggplot2::geom_point(
        data = dplyr::filter(tree_data, !is.na(.data$Taxon_a_posteriori)),
        ggplot2::aes(
          color = .data$Taxon_a_posteriori,
          shape = .data$Taxon_a_posteriori
        ), size = 3
      ),
      ggtree::geom_label2(
        data = tree_data, alpha = 0.4, size = 3,
        ggplot2::aes(
          subset = .data$posterior > 0.5,
          label = round(.data$posterior, 2)
        )
      )
    )
  return(label_list)
}

#' BEAST `ggtree` Theme
#'
#' Build tree layers to specify manual color and shape values and set legend
#' theme. Legend includes all values from prior and reviewed annotations in
#' `tree_data`.
#'
#' @inheritParams node_labels
#' @export
#'
#' @return [ggtree::ggtree()] aesthetic layers for tree themes.
#'
#' @examples
#' tree_data <-
#'   list.files(
#'     path = system.file("extdata/BEAST", package = "thesis"),
#'     pattern = "multi-locus.combined.mcc", full.names = TRUE
#'   ) %>%
#'   read_tree(tree_file = .)
#'
#' ggtree::ggtree(tr = tree_data) +
#'   beast_labels(tree_data = tree_data) +
#'   beast_theme(tree_data = tree_data)
#'
#' @keywords internal
beast_theme <- function(tree_data) {
  # Combine prior and reviewed annotations then keep unique values.
  id_labels <- c(
    spl_labels(specimen_tbl = tree_data, id_column = "prior_id"),
    spl_labels(specimen_tbl = tree_data, id_column = "Taxon_a_posteriori")
  ) %>%
    purrr::keep(.x = ., ~ !grepl("\\*NA\\*", x = .x))
  id_labels <- id_labels[!duplicated(id_labels)]

  theme_list <-
    list(
      ggplot2::scale_color_manual(
        "Annotations",
        values = thesis::spp_color, labels = id_labels
      ),
      ggplot2::scale_shape_manual(
        "Annotations",
        values = thesis::spp_shape, labels = id_labels
      ),
      ggplot2::theme(
        legend.text = ggtext::element_markdown(),
        legend.title = ggplot2::element_text(hjust = 0.5)
      )
    )
  return(theme_list)
}

# Plot Functions ----

#' BEAST Plot Grid
#'
#' Build a grid plot by [cowplot::plot_grid()] from BEAST tree data.
#' Includes a tree aesthetic with posterior probabilities and color
#' aesthetic for layering prior and reviewed annotations.
#'
#' @inheritParams node_labels
#' @inheritParams bayes_ggtree
#' @export
#'
#' @return [cowplot::plot_grid()] of BEAST results with posterior probabilities,
#' specimen labels, and theme-layered [ggtree::ggtree()] object.
#'
#' @examples
#' tree_data <-
#'   list.files(
#'     path = system.file("extdata/BEAST", package = "thesis"),
#'     pattern = "multi-locus.combined.mcc", full.names = TRUE
#'   ) %>%
#'   read_tree(tree_file = .)
#'
#' beast_plot(tree_data = tree_data)
#'
#' @keywords internal
beast_plot <- function(tree_data, ggtree_layout = "circular") {
  beast_ggtree <-
    ggtree::ggtree(tree_data, layout = ggtree_layout) +
    thesis::beast_posterior() +
    thesis::beast_labels(tree_data = tree_data) +
    thesis::beast_theme(tree_data = tree_data) +
    ggplot2::theme(
      legend.position = "none"
    )
  return(beast_ggtree)
}

#' Extract Color Legend
#'
#' @inheritParams node_labels
#' @param ncol Number of columns for annotation legend guide.
#' @export
#'
#' @return `gtable` object with specimen label legend.
#'
#' @keywords internal
beast_legend_color <- function(tree_data, ncol = 2) {
  color_legend <-
    cowplot::get_legend(
      ggtree::ggtree(tree_data) +
        beast_labels(tree_data = tree_data) +
        beast_theme(tree_data = tree_data) +
        ggplot2::guides(col = ggplot2::guide_legend(ncol = ncol))
    )
  return(color_legend)
}

#' Extract Probability Legend
#'
#' @inheritParams node_labels
#' @export
#'
#' @return `gtable` object with posterior probability legend.
#'
#' @keywords internal
beast_legend_probability <- function(tree_data) {
  posterior_legend <-
    cowplot::get_legend(
      ggtree::ggtree(tree_data) +
        beast_posterior()
    )
  return(posterior_legend)
}

# *BEAST Species Tree ----

#' Plot *BEAST Species Tree
#'
#' Build [ggtree] plot from *BEAST maximum credibility clade (_.mcc_) tree file.
#' Species tip labels are parsed to abbreviate genus and italicize using
#' base expressions.
#'
#' On label expressions:
#'   - https://guangchuangyu.github.io/2018/04/rename-phylogeny-tip-labels-in-treeio/
#'
#' @param label_size Numeric vector of length one for label size.
#' @inheritParams parse_taxa
#' @export
#'
#' @return `ggtree` built from *BEAST species tree
#'
#' @examples
#' treeio::read.beast(
#'   file = system.file("extdata/BEAST/spp-hypothesis-1.mcc",
#'     package = "thesis"
#'   )
#' ) %>%
#'   ggtree::fortify() %>%
#'   dplyr::mutate(
#'     label = gsub("medicinae", "'medicinae'", x = .data$label)
#'   ) %>%
#'   species_plot(tree_tibble = .)
#'
#' @keywords internal
species_plot <- function(tree_tibble, label_size = 2) {
  # Parse tip label expressions for species italicization.
  labeled_tree <- tree_tibble %>%
    dplyr::mutate(
      label = thesis::parse_taxa(tree_tibble = ., id_column = "label")
    )

  # Build ggtree from species tree.
  spp_ggtree <-
    ggtree::ggtree(tr = labeled_tree, ggplot2::aes(color = .data$posterior)) +
    ggtree::geom_tiplab(size = label_size, parse = T) +
    ggrepel::geom_label_repel(
      data = dplyr::filter(tree_tibble, !is.na(.data$posterior)),
      mapping = ggplot2::aes(
        x = .data$x, y = .data$y,
        label = round(x = .data$posterior, 2)
      ),
      nudge_x = -0.00001,
      size = 2
    ) +
    ggplot2::scale_color_gradientn(
      colors = c("red", "orange", "green", "cyan", "blue")
    ) +
    ggtree::xlim_expand(xlim = 0.0013, panel = "Tree") +
    ggtree::geom_treescale() +
    ggtree::theme_tree()
  return(spp_ggtree)
}
