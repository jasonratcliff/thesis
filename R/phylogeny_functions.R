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

#' Multi-Node Tibble
#'
#' Given the Bayes results joined to DNA speciemens returned by
#' `bayes_tibble()`, group by node and filter to specimens with identical nodes
#' (i.e. with identical genotypes).
#'
#' @param bayes_tbl Parsed ggtree tibble returned by `bayes_tibble()` with
#'   split sample observations and scaled geom size aesthetics.
#' @inheritParams bayes_tibble
#'
#' @return Tibble of specimens from multi-taxa nodes.
#'
#' @export
#'
bayes_tibble_multi <- function(bayes_tbl, id_column) {

  bayes_tbl %>% # Select columns and filter to distinct observations.
    dplyr::select(!!id_column, "State", "Collector", "Collection_Number",
                  "node") %>% dplyr::distinct() %>%

    # Group by node and filter to multi-taxa specimens.
    dplyr::group_by(.data$node) %>% dplyr::mutate(Count = dplyr::n()) %>%
    dplyr::filter(.data$Count > 1) %>% dplyr::arrange(.data$node) %>%

    # Add unique genotype ID and select rows.
    dplyr::mutate(Genotype = dplyr::group_indices()) %>% dplyr::ungroup() %>%
    dplyr::select("Genotype", !!id_column, "State",
                  "Collector", "Collection_Number") %>%

    # Text substitution to standardize collector names.
    dplyr::mutate(Collector = purrr::map_chr(.data$Collector,
      function(collector) {
        gsub("[A-Z]\\. ?", "", collector) %>% gsub("&|with", "and", x = .)
        })) %>%

    # Rename column with tidy eval.
    dplyr::rename(Species = rlang::sym(!!id_column),
                  `Collection Number` = .data$Collection_Number)
}

# ggtree Plotting ----

#' Plot MrBayes ggtree
#'
#' Given the output of `bayes_tibble()`, build a ggplot with color / shape
#' aesthetics defined by identification column.
#'
#' @param scale_name Character scalar with name for scale title.
#' @param x_expand Numeric scalar for x-axis expansion of ggtree plot.
#' @inheritParams bayes_tibble
#' @inheritParams bayes_tibble_multi
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
    ggplot2::geom_point(data = dplyr::filter(bayes_tbl, !!id_expr),
      ggplot2::aes_string(color = id_column, shape = id_column,
                          size = "geom_scale"), na.rm = TRUE) +

    # Add labels with rounded posterior probabilities to resolved nodes.
    ggplot2::geom_label(data = bayes_labels, nudge_x = nudge_xlim,
      ggplot2::aes(x = .data$x, y = .data$y,
        label = sprintf("%0.3f", as.numeric(.data$prob), digits = 3)),
      fontface = "bold", fill = "lightgoldenrod", size = 4, alpha = 0.35) +

    # Adjust color and shape scales with spp_labels() markdown ggtext elements.
    ggplot2::scale_color_manual(scale_name, values = ThesisPackage::spp_color,
      labels = spp_labels(specimen_tibble = bayes_tbl, id_column = id_column)) +
    ggplot2::scale_shape_manual(scale_name, values = ThesisPackage::spp_shape,
      labels = spp_labels(specimen_tibble = bayes_tbl, id_column = id_column)) +
    ggplot2::theme(legend.text = ggtext::element_markdown(size = 8),
                   legend.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_text(hjust = 0.5, size = 14,
                                                        face = "bold")) +
    ggplot2::expand_limits(x = x_expand) +

    # Modify scale of size aesthetic and spacing of color / shape guide.
    ggplot2::scale_size_continuous(range = c(3, 9), guide = "none") +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3),
      ncol = 2, byrow = TRUE, keyheight = 0.15, default.unit = "inch"))

  # Return ggtree plot with labels
  ThesisPackage::bayes_tip_labels(bayes_ggtree_obj = ggtree_plot,
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
#' @inheritParams bayes_tibble_multi
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
    ggplot2::geom_text(data = multi_taxa_tbl, fontface = "bold",
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$node_group),
      nudge_x = xnudge_multi, size = 2.5, hjust = 0, vjust = 0.25) +
    ggplot2::geom_text(data = single_taxa_tbl,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label.x),
      na.rm = TRUE, nudge_x = xnudge_single, size = 2.5, hjust = 0)
}

#' Multi-taxa Tips
#'
#' Filter samples from output of `bayes_tibble()` function to nodes with
#' multiple taxa grouped by identical genotypes.
#'
#' @inheritParams bayes_ggtree
#' @inheritParams bayes_tibble_multi
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

# BEAST Functions ----

#' Build BEAST ggtree
#'
#' Read in nexus file of consenus tree exported from fig tree.
#' Consensus of .mcc file built from BEAST .trees output using tree annotator:
#'  ~/Applications/BEASTv1.10.4/bin/treeannotator
#'
#' @param beast_file Filepath to .mcc concensus tree file.
#' @inheritParams bayes_tibble
#' @inheritParams bayes_ggtree
#' @inheritParams ggtree_plot
#'
#' @return BEAST ggtree object with posterior probabilities and spp ID.
#'
#' @export
#'
beast_ggtree <- function(beast_file, id_column, scale_name,
                         plot_x = 0.1, plot_y = 0.5,
                         plot_width = 0.25, plot_height = 0.5) {

  # Join DNA information to BEAST results.
  tree_data <- treeio::read.beast(file = beast_file) %>% ggtree::fortify() %>%
    dplyr::left_join(., ThesisPackage::dna_specimens, by = "label")

  # Re-assign Bayes tibble to prevent function scoping issue.
  id_quo <- rlang::ensym(id_column)
  id_expr <- rlang::expr(!is.na(!!id_quo))

  # beast_ggtree <- beast_ggtree +
  beast_ggtree <-
    ggtree::ggtree(tree_data, ggplot2::aes(color = .data$posterior)) +
      ggplot2::scale_color_gradientn(name = "Posterior\nProbablility",
        colors = c("red", "orange", "green", "cyan", "blue"),
        guide = ggplot2::guide_colourbar(order = 1)) +

    # Add new color scale to plot IDs on tips.
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(data = dplyr::filter(tree_data, !!id_expr),
                        size = 3, na.rm = TRUE,
                        ggplot2::aes_string(color = id_column,
                                            shape = id_column)) +
    ggplot2::scale_color_manual(scale_name, values = ThesisPackage::spp_color,
      labels = ThesisPackage::spp_labels(specimen_tibble = tree_data,
                                         id_column = id_column)) +
    ggplot2::scale_shape_manual(scale_name, values = ThesisPackage::spp_shape,
      labels = ThesisPackage::spp_labels(specimen_tibble = tree_data,
                                         id_column = id_column)) +

    # Add tip labels and posterior probabilities node labels.
    ggtree::geom_tiplab(offset = 0.00015, align = TRUE) +
    ggtree::geom_label2(data = tree_data, alpha = 0.4,
                        nudge_x = (max(tree_data$x) * -0.035),
                        nudge_y = (max(tree_data$y) * 0.012),
                        ggplot2::aes(subset = .data$posterior > 0.5,
                                     label = round(.data$posterior, 2))) +
    ggtree::xlim_expand(xlim = c(0, 0.005), panel = "Tree") +
    ggplot2::guides(colour = FALSE, shape = FALSE) # Overwrite ID guides

  # Build cowplot grid with inset legend extracted with `beast_legend()`.
  spp_legend <-
    ThesisPackage::beast_legend(tree_data = tree_data,
                                id_column = id_column, scale_name = scale_name)

  cowplot::plot_grid(beast_ggtree) +
    cowplot::draw_plot(plot = spp_legend, x = plot_x, y = plot_y,
                       width = plot_width, height = plot_height)
}

#' Extract BEAST Legend
#'
#' Use `cowplot::get_legend()` to extract legend of specimen
#' identifications from ggtree object.
#'
#' @param tree_data Tibble of merged BEAST .mcc data with DNA specimens.
#' @inheritParams bayes_tibble
#' @inheritParams bayes_ggtree
#' @inheritDotParams beast_ggtree
#'
#' @return gtable object of legened extracted from BEAST ggtree build.
#'
#' @export
#'
beast_legend <- function(tree_data, id_column, scale_name, ...) {

  # Assign symbol / expression objects for filtering .
  id_quo <- rlang::ensym(id_column)
  id_expr <- rlang::expr(!is.na(!!id_quo))

  # Build ggtree with specimen identifications indicated by `id_column`.
  beast_ggtree <-
    ggtree::ggtree(tree_data) +
    ggplot2::geom_point(data = dplyr::filter(tree_data, !!id_expr),
      ggplot2::aes_string(color = id_column, shape = id_column),
      size = 3, na.rm = TRUE) +
    ggplot2::scale_color_manual(scale_name, values = ThesisPackage::spp_color,
      labels = ThesisPackage::spp_labels(specimen_tibble = tree_data,
        id_column = id_column), na.translate = FALSE) +
    ggplot2::scale_shape_manual(scale_name, values = ThesisPackage::spp_shape,
      labels = ThesisPackage::spp_labels(specimen_tibble = tree_data,
        id_column = id_column),  na.translate = FALSE) +
    ggplot2::theme(legend.text = ggtext::element_markdown(size = 6),
          legend.title = ggplot2::element_text(hjust = 0.5, size = 11,
                                               face = "bold")) +

    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3),
      ncol = 2, byrow = TRUE, keyheight = 0.1, default.unit = "inch"))

  # Extract legend with specimen ID key.
  return(cowplot::get_legend(plot = beast_ggtree))
}

# Phylogenetics Kable ----

#' Kable build for multi-specimen nodes.
#'
#' @param tbl_multi_node Tibble of multi-taxa nodes returned by
#'   `bayes_tibble_multi()`.
#' @param kable_caption Character vector for caption passed to
#'   `knitr::kable()` argument `caption`.
#' @param knitr_chunk Character vector passed to `knitr::kable()` argument
#'  `format` - one of "latex" or "html".  Defined by
#'  `opts_knit$get("rmarkdown.pandoc.to")` in `index.Rmd` of Thesis Bookedown.
#'
#' @return `kable` object contained multi-taxa nodes with locality and specimen
#'   information.
#'
#' @export
#'
phylo_kable <- function(tbl_multi_node, kable_caption, knitr_chunk) {
  knitr::kable(tbl_multi_node, caption = kable_caption, format = knitr_chunk,
               escape = F, row.names = FALSE,
               align=c("c", "c", "l", "l", "l")) %>%
    kableExtra::kable_styling(full_width = FALSE, font_size = 10,
                              latex_options= "hold_position") %>%
    kableExtra::row_spec(row = 0, bold = TRUE, font_size = 10) %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(2, width = "3.7cm") %>%
    kableExtra::column_spec(5, border_right = TRUE, width = "2cm") %>%
    kableExtra::collapse_rows(columns = 1:3)
}

