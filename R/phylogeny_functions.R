# Phylogenetics Functions ----

#' Format Mr. Bayes Results into Tibble
#'
#' Given an input Nexus file with results from a Mr. Bayes run, split rows of
#' nodes with identical sample genotypes and calculate a scaled geom size
#' for plotting phylogenies with the `ggtree` package.
#'
#' @param bayes_file Nexus file of MrBayes results to read in by `treeio`.
#' @param dna_specimens_tbl Tibble of DNA specimens assigned in script
#'   `herbarium_specimens.R`
#' @param id_column Character scalar of identification column to group geoms by.
#' @param geom_size_scale Character vector of length two with min / max values
#'   to rescale the geom size aesthetic for multi-taxa tips.
#' @return Tibble formatted for `ggtree` plotting with geom size scaled by ID.
#'
#' @examples
#' \dontrun{
#' bayes_tbl <-
#'   bayes_tibble(bayes_file = "~/Thesis/data/8.bayes/rITS-infile.nex.con.tre",
#'                dna_specimens_tbl = dna_specimens, id_column = "prior_id")
#' }
#'
#' @export
#'
bayes_tibble <- function(bayes_file, dna_specimens_tbl,
                         id_column, geom_size_scale = c(4, 7)) {

  # Enquosure for tidy evaluation.
  id_quo <- rlang::enquo(id_column)

  # Read in conensus tree file .nexus file as tibble object.
  bayes_tbl <- treeio::read.beast(bayes_file) %>% ggtree::fortify()

  # Map data frame rows with split labels and node key for relational data.
  # Labels are split by RegEx of number-underscore-letter.
  tbl_tree_long <-
    purrr::pmap_dfr(bayes_tbl, function(label, node, ...) {

      # Split labels by preceding numeric and following letter look-arounds.
      phylo_labels <-
        purrr::map(label, function(label) {
          stringr::str_split(string = label,
                             pattern = "(?<=[0-9])_(?=[A-Z])")
        }) %>% unlist()

      # Bind node ID key for joining data to split labels.
      tibble::tibble(node = rep(node, each = length(phylo_labels)),
                     label = phylo_labels)
    }) %>% dplyr::right_join(., bayes_tbl, by = "node") %>%
    dplyr::left_join(., dna_specimens_tbl, by = c("label.x" = "label"))


  # Group data by node and identification column to calculate scaled geom sizes.
  tbl_tree_merged <- tbl_tree_long %>%
    dplyr::group_by_at(dplyr::vars(.data$node, !!id_quo)) %>%
    dplyr::count(name = "geom_size", .drop = FALSE) %>%
    dplyr::left_join(., tbl_tree_long, by = c("node", id_column)) %>%
    dplyr::ungroup()

  # Scale geoms by numeric vector, bind columns, and arrange rows by geom size.
  dplyr::bind_cols(geom_scale = scales::rescale(tbl_tree_merged$geom_size,
    to = geom_size_scale), tbl_tree_merged) %>%
    dplyr::select(.data$node:.data$geom_size,
                  .data$geom_scale, .data$label.x:.data$Chromosomes) %>%
    dplyr::arrange(.data$node, dplyr::desc(.data$geom_scale))

}

# ggtree Plotting ----

#' Plot MrBayes ggtree
#'
#' Given the output of `bayes_tibble()`, build a ggplot with color / shape
#' aesthetics defined by identification column.
#'
#' @param bayes_tbl Parsed ggtree tibble returned by `bayes_tibble()` with
#'   split sample observations and scaled geom size aesthetics.
#' @param scale_name Character scalar with name for scale title.
#' @param x_expand Numeric scalar for x-axis expansion of ggtree plot.
#' @inheritParams bayes_tibble
#'
#' @return ggtree object with tips aesthetics determined by `id_column` and
#' posterior probabilities on resolved (i.e. non-polytomy) nodes.
#'
#' @export
#'
bayes_ggtree <- function(bayes_tbl, id_column, scale_name,
                         x_expand = 0.02) {

  # Re-assign Bayes tibble to prevent function scoping issue.
  bayes_tbl_obj <- bayes_tbl
  id_quo <- rlang::ensym(id_column)
  id_expr <- rlang::expr(!is.na(!!id_quo))

  # Filter out polytomy nodes for labels.
  bayes_labels <- bayes_tbl %>%
    dplyr::filter(.data$prob != 1 & !is.na(.data$prob))
  nudge_xlim <- -(max(range(bayes_labels$x)) * 0.05)

  # Build ggtree plot from Bayes results.
  ggtree_plot <- ggtree::ggtree(bayes_tbl, layout = "rectangular") +
    geom_point(data = dplyr::filter(bayes_tbl, !!id_expr),
               aes_string(color = id_column, shape = id_column,
                          size = "geom_scale"), na.rm = TRUE) +

    # Add labels with rounded posterior probabilities to resolved nodes.
    geom_label(data = bayes_labels, nudge_x = nudge_xlim,
      aes(x = .data$x, y = .data$y,
          label = sprintf("%0.3f", as.numeric(.data$prob), digits = 3)),
      fontface = "bold", fill = "lightgoldenrod", size = 4, alpha = 0.35) +

    # Adjust color and shape scales with spp_labels() markdown ggtext elements.
    scale_color_manual(scale_name, values = ThesisPackage::spp_color,
      labels = spp_labels(specimen_tibble = bayes_tbl, id_column = id_column)) +
    scale_shape_manual(scale_name, values = ThesisPackage::spp_shape,
      labels = spp_labels(specimen_tibble = bayes_tbl, id_column = id_column)) +
    theme(legend.text = ggtext::element_markdown(size = 8),
          legend.background = element_blank(),
          legend.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
    expand_limits(x = x_expand) +

    # Modify scale of size aesthetic and spacing of color / shape guide.
    scale_size_continuous(range = c(3, 9), guide = "none") +
    guides(color = guide_legend(override.aes = list(size = 3),
                                ncol = 2, byrow = TRUE,
                                keyheight = 0.15, default.unit = "inch"))

  # Return ggtree plot with labels
  bayes_tip_labels(bayes_ggtree_obj = ggtree_plot,
                   bayes_tbl = bayes_tbl_obj) +
    ggtree::geom_treescale(offset = -1.5)

}

#' Add ggtree Plot Tip Labels
#'
#' Adds tip labels to ggtree plot for single-taxa tips with unique genotypes.
#' Multi-taxa tips returned by the `multi_taxa_nodes` are marked by grouped
#' genotype numbered by node order ranking.
#'
#' @param bayes_ggtree_obj ggtree object returned by `bayes_ggtree()` function.
#' @inheritParams bayes_ggtree
#'
#' @return ggtree object with text geoms identifying taxa tip labels.
#'
#' @export
#'
bayes_tip_labels <- function(bayes_ggtree_obj, bayes_tbl) {

  # Assign tibbles for multi-taxa and single-taxon tip labels.
  multi_taxa_tbl <- multi_taxa_nodes(bayes_tbl)
  single_taxa_tbl <- bayes_tbl %>%
    dplyr::group_by(.data$node) %>% dplyr::count(vars = "node") %>%
    dplyr::ungroup() %>% dplyr::right_join(., bayes_tbl, by = "node") %>%
    dplyr::filter(.data$n == 1) %>%
    dplyr::select(.data$x, .data$y, .data$label.x)

  # Add tip labels for single- and multi-taxa tip labels.
  xnudge_multi <- max(range(multi_taxa_tbl$x)) * 0.03
  xnudge_single <- max(range(single_taxa_tbl$x)) * 0.025

  bayes_ggtree_obj +
    geom_text(data = multi_taxa_tbl,
              aes(x = .data$x, y = .data$y, label = .data$node_group),
              nudge_x = xnudge_multi,
              size = 2.5, hjust = 0, vjust = 0.25, fontface = "bold") +
    geom_text(data = single_taxa_tbl,
              aes(x = .data$x, y = .data$y, label = .data$label.x),
              na.rm = TRUE, nudge_x = xnudge_single, size = 2.5, hjust = 0)
}

#' Multi-taxa Tips
#'
#' Filter samples from output of `bayes_tibble()` function to nodes with
#' multiple taxa grouped by identical genotypes.
#'
#' @inheritParams bayes_ggtree
#'
#' @return Tibble of filtered multi-taxa tips grouped by node.
#'
#' @export
#'
multi_taxa_nodes <- function(bayes_tbl) {

  # Select tibble variables for filtering multi-taxa tips.
  multi_taxa_nodes <- bayes_tbl %>%
    dplyr::select(.data$node, .data$label.x, .data$x, .data$y, .data$label.y)

  # Filter multi-taxa nodes and create group index for combined genotype.
  multi_taxa_nodes %>%
    dplyr::group_by(.data$node) %>% dplyr::count(vars = "node") %>%
    dplyr::right_join(., multi_taxa_nodes, by = "node") %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::arrange(.data$node) %>% dplyr::ungroup() %>%
    dplyr::mutate(node_group = dplyr::group_indices(., .data$node, .data$n) %>%
                    paste0("genotype ", .))

}

#' Build ggtree Plot Grid
#'
#' Wrapper function to plot legend inset onto ggtree object.
#'
#' @inheritParams bayes_tip_labels
#' @param plot_x Numeric scalar for plot x position.
#' @param plot_y Numeric scalar for plot y position.
#' @param plot_width Numeric scalar for plot width.
#' @param plot_height Numeric scale for plot height.
#' @inheritDotParams cowplot::draw_plot
#'
#' @return Ggplot with ggtree and legend inset.
#'
#' @export
#'
ggtree_plot <- function(bayes_ggtree_obj, plot_x = 0.1, plot_y = 0.5,
                        plot_width = 0.25, plot_height = 0.5, ...) {

  # Remove legend from base ggtree plot.
  ggtree_plot_build <- bayes_ggtree_obj +
    ggtree::theme_tree(legend.position = "none")

    # Extract legend and build a plot grid with inset legend.
    ggtree_legend <- cowplot::get_legend(bayes_ggtree_obj)
    cowplot::ggdraw(ggtree_plot_build) +
      cowplot::draw_plot(plot = ggtree_legend, x = plot_x, y = plot_y,
                         width = plot_width, height = plot_height, ...)
}

