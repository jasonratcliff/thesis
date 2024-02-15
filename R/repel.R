# Call expression wrapper over ggrepel geoms

#' Repel Map Labels
#'
#' @description
#' Leverage [rlang::call2()] to build call expressions, mapping
#' a [tibble::tribble()] with x- and y-axis nudges for layered
#' labels via [ggrepel::geom_label_repel()]. For expression evaluation,
#' pipe output into [rlang::eval_tidy()].
#'
#' @details
#' For each call layered onto a [ggplot2::ggplot()], labels represent
#' specimen sampling identifying individual records of interest.
#'
#' @param map_nudges Inline [tibble::tribble()] with variables to set
#'   `nudge_x`, `nudge_y`, `segment.curvature`, and variable to join by:
#'
#'       - `Key`: e.g., collector(s) and collection separated by "_".
#' @param map_labels A [tibble::tibble()] input for `ggplot` with `x`, `y`, and
#'   `label` aesthetics passed to [ggrepel::geom_label_repel()].
#' @param initial_ggplot Base layer [ggplot2::ggplot()] for [purrr::map()] and
#'   [purrr::reduce()] into [rlang::call2()] to build R call expressions.
#' @family labels
#' @export
#'
#' @return List of R call expressions to layer repelled labels with coordinate
#'   nudges onto [ggplot2::ggplot()] object.
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
#' specimens <- thesis::herbarium_specimens %>%
#'   subset_coords(
#'     specimen_tbl = .,
#'     Longitude = c(-108, -105),
#'     Latitude = c(39.75, 41.75)
#'   )
#'
#' jackson <-
#'   thesis::find_spp(
#'     specimen_tbl = thesis::herbarium_specimens,
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
#' map <- ggplot2::ggplot() +
#'   thesis::layer_specimens(
#'     specimen_tbl = specimens,
#'     id_column = "Taxon_a_posteriori"
#'   ) +
#'   ggplot2::geom_point(
#'     data = jackson,
#'     mapping = ggplot2::aes(x = Longitude, y = Latitude),
#'     shape = 5, color = "black", size = 4
#'   ) +
#'   ggplot2::coord_sf(
#'     xlim = range(extent$Longitude),
#'     ylim = range(extent$Latitude)
#'   ) +
#'   ggplot2::theme_classic()
#'
#' tibble::tribble(
#'   ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"Key",
#'   0.4, -0.05, 0.1, "Nelson_49286",
#'   -1, -0.25, 0.1, "Kastning_Kastning_1462",
#'   -1, -0.25, 0.1, "Kastning_Culp_1725",
#'   -0.4, -0.1, -0.1, "Nelson_49478",
#' ) %>%
#'   thesis::repel_map_labels(
#'     map_nudges = .,
#'     map_labels = jackson,
#'     initial_ggplot = map
#'   ) %>%
#'   rlang::eval_tidy(expr = .)
#'
#' @keywords internal
repel_map_labels <- function(map_nudges, map_labels, initial_ggplot) {
  stopifnot(identical(class(initial_ggplot), c("gg", "ggplot")))
  repelled_ggplot <- dplyr::left_join(
    x = map_nudges,
    y = map_labels,
    by = "Key"
  ) %>%
    purrr::pmap(
      .l = .,
      .f = function(Longitude, Latitude, Label,
                    nudge_x, nudge_y, segment.curvature, ...) {
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
      }
    ) %>%
    purrr::reduce(
      .x = .,
      .f = ~ rlang::expr(!!.x + !!.y),
      .init = initial_ggplot
    )
}

#' Repel Tree Labels
#'
#' @inherit repel_map_labels description
#'
#' @details
#' For nodes with multiple taxa in the base layer [ggtree::ggtree()], repelled
#' labels denote specimen annotations with counts tabulated by
#' [haplotype_labels()].
#'
#' @param tree_nudges Inline [tibble::tribble()] with variables to set
#'   `nudge_x`, `nudge_y`, `segment.curvature`, `color` (label text),
#'   and variables to join by:
#'
#'       - `node`: Numeric scalar denoting phylogenetic tree node.
#'       - `Taxon_a_posteriori`: Character scalar of reviewed identification.
#' @param tree_labels Tibble of labels output by [haplotype_labels()] with
#'   with [ggplot] aesthetics for `x`, `y`, `label`, and `fill` aesthetics.
#' @param initial_ggtree Base layer [ggtree::ggtree()] for [purrr::map()] and
#'   [purrr::reduce()] into [rlang::call2()] to build R call expressions.
#' @param label_size Numeric scalar to indicate `size` parameter for
#'   the repelled geom layer.
#' @family labels
#' @export
#' @seealso haplotype_labels
#'
#' @return List of R call expressions to layer repelled labels with coordinate
#'   nudges onto [ggtree::ggtree()] object.
#'
#' @examples
#' bayes_joined <-
#'   list.files(
#'     path = system.file("extdata/MrBayes", package = "thesis"),
#'     pattern = "rps-infile.nex.con.tre",
#'     full.names = TRUE
#'   ) %>%
#'   thesis::read_tree(tree_file = .) %>%
#'   thesis::join_bayes(
#'     tree_data = .,
#'     id_column = "Taxon_a_posteriori",
#'     scale_vector = c(5, 10)
#'   )
#'
#' bayes_haplotypes <-
#'   thesis::haplotype_labels(
#'     haplotypes = bayes_joined,
#'     id_column = "Taxon_a_posteriori"
#'   )
#'
#' bayes_ggtree <-
#'   ggtree::ggtree(tr = bayes_joined, layout = "circular") +
#'   ggplot2::geom_point(mapping = ggplot2::aes(color = prior_id), size = 3)
#'
#' tibble::tribble(
#'   ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"node", ~"Taxon_a_posteriori", ~"color",
#'   0.01, -1.5, 0.1, 5, "Physaria 'medicinae'", "white",
#'   0.0075, -1.5, 0.1, 5, "Physaria didymocarpa subsp. didymocarpa", "white",
#'   0.005, -1.5, 0.1, 5, "Physaria eburniflora", "white",
#'   0.0025, -1.5, 0.1, 5, "Physaria acutifolia", "black"
#' ) %>%
#'   thesis::repel_haplotype_labels(
#'     tree_nudges = .,
#'     tree_labels = bayes_haplotypes,
#'     initial_ggtree = bayes_ggtree,
#'     label_size = 3
#'   ) %>%
#'   rlang::eval_tidy(expr = .)
#'
#' @keywords internal
repel_haplotype_labels <- function(tree_nudges, tree_labels,
                                   initial_ggtree, label_size = 2) {
  # Check tree object class and join variables as expected.
  stopifnot(identical(class(initial_ggtree), c("ggtree", "gg", "ggplot")))
  stopifnot(
    unique(
      c("node", "Taxon_a_posteriori") %in%
        names(tree_nudges)
    ) == TRUE
  )

  # Build call for `ggrepel::geom_label_repel()` layer expression.
  repelled_ggplot <-
    dplyr::left_join(
      x = tree_nudges,
      y = tree_labels,
      by = c("node", "Taxon_a_posteriori")
    ) %>%
    purrr::pmap(
      .l = .,
      .f = function(x, y, Label, Taxon_a_posteriori, color,
                    nudge_x, nudge_y, segment.curvature, ...) {
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

#' Repel Node Probabilities
#'
#' @details
#' [ggrepel::geom_label_repel()] `LayerInstance` for repelled node posterior
#' probabilities. Excludes nodes with missing values. Probabilites are rounded
#' to 3 digits.
#'
#' @param node_nudges Inline [tibble::tribble()] with variables to set
#'   `nudge_x`, `nudge_y`, `segment.curvature`
#'   and variable to join by:
#'
#'       - `node`: Numeric scalar denoting phylogenetic tree node.
#' @param node_labels Tibble subset from labels read in by [join_bayes()] to
#'   nodes with posterior probabilities.
#' @inherit repel_map_labels description
#' @inheritParams repel_haplotype_labels
#' @family labels
#' @export
#'
#' @return List with [ggplot2::ggplot2()] `LayerInstance` for node repelled
#'   posterior probabilities by tree node.
#'
#' @examples
#' haplotypes <-
#'   list.files(
#'     path = system.file("extdata/MrBayes", package = "thesis"),
#'     pattern = "rps-infile.nex.con.tre",
#'     full.names = TRUE
#'   ) %>%
#'   thesis::read_tree(tree_file = .) %>%
#'   thesis::join_bayes(
#'     tree_data = .,
#'     id_column = "Taxon_a_posteriori"
#'   )
#'
#' probabilities <- haplotypes %>%
#'   dplyr::filter(.data$prob != 1) # Exclude MrBayes polytomies
#'
#' bayes_ggtree <-
#'   ggtree::ggtree(tr = haplotypes, layout = "circular") +
#'   ggplot2::geom_point(mapping = ggplot2::aes(color = prior_id), size = 3)
#'
#' tibble::tribble(
#'   ~"nudge_x", ~"nudge_y", ~"segment.curvature", ~"node",
#'   -0.001, 0, 0.01, 31,
#'   -0.002, 0, 0.01, 32,
#'   -0.003, 0, 0.01, 33,
#'   -0.004, 0, 0.01, 34,
#'   -0.005, 0, 0.01, 35
#' ) %>%
#'   thesis::repel_node_labels(
#'     node_nudges = .,
#'     node_labels = probabilities,
#'     initial_ggtree = bayes_ggtree,
#'     label_size = 4
#'   ) %>%
#'   rlang::eval_tidy(expr = .)
#'
#' @keywords internal
repel_node_labels <- function(node_nudges, node_labels,
                              initial_ggtree, label_size = 3) {
  stopifnot(identical(class(initial_ggtree), c("ggtree", "gg", "ggplot")))

  # Build call for `ggrepel::geom_label_repel()` layer expression.
  repelled_ggplot <-
    dplyr::left_join(
      x = node_nudges,
      y = node_labels,
      by = c("node")
    ) %>%
    purrr::pmap(
      .l = .,
      .f = function(x, y, prob, segment.curvature,
                    nudge_x, nudge_y, ...) {
        rlang::call2(
          .fn = ggrepel::geom_label_repel,
          data = tibble::tibble(
            x = x,
            y = y,
            prob = prob
          ),
          mapping = ggplot2::aes(
            x = x,
            y = y,
            label = sprintf(fmt = "%0.3f", as.numeric(prob), digits = 3)
          ),
          xlim = c(-Inf, Inf),
          ylim = c(-Inf, Inf),
          nudge_x = nudge_x,
          nudge_y = nudge_y,
          fontface = "bold",
          size = label_size,
          segment.linetype = 2,
          segment.curvature = segment.curvature
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

#' Group Haplotype Taxon Labels
#'
#' Create labels of multiple taxa tip positions grouped by count.
#'
#' @param haplotypes Joined haplotypes with labels read in by [join_bayes()]
#' @inheritParams parse_taxa
#' @export
#'
#' @return [tibble::tibble()] of multi-taxa node labels with `x`, `y`, `label`,
#'   and `fill` [ggplot2::ggplot()] aesthetics.
#'
#' @examples
#' list.files(
#'   path = system.file("extdata/MrBayes", package = "thesis"),
#'   pattern = "rps-infile.nex.con.tre",
#'   full.names = TRUE
#' ) %>%
#'   thesis::read_tree(tree_file = .) %>%
#'   thesis::join_bayes(
#'     tree_data = .,
#'     id_column = "Taxon_a_posteriori",
#'     scale_vector = c(5, 10)
#'   ) %>%
#'   haplotype_labels(haplotypes = ., id_column = "Taxon_a_posteriori")
#'
#' @keywords internal
haplotype_labels <- function(haplotypes, id_column) {
  # Filter to nodes with multiple taxa, then count instances of reviewed IDs.
  labelled_haplotypes <- haplotypes %>%
    dplyr::select(
      .data$node, .data$x, .data$y,
      .data[[id_column]], .data$label
    ) %>%
    dplyr::add_count(.data$node, name = "n_nodes") %>%
    dplyr::filter(.data$n_nodes > 1 & !is.na(.data$label)) %>%
    dplyr::add_count(.data$node, .data[[id_column]], name = "n_taxa") %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    dplyr::arrange(.data$node) %>%
    dplyr::mutate(
      Label = thesis::parse_taxa(
        tree_tibble = .,
        id_column = {{ id_column }}
      ) %>%
        paste0(., "~(n==", .data$n_taxa, ")")
    )
  return(labelled_haplotypes)
}
