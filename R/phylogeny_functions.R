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
#' bayes_test <-
#'   bayes_tibble(bayes_file = "data/8.bayes/rITS-infile.nex.con.tre",
#'                dna_specimens_tbl = dna_specimens, id_column = "prior_id")
#' }
#'
#' bayes_file <- "data/8.bayes/rITS-infile.nex.con.tre"
#' dna_specimens_tbl <- dna_specimens
#' id_column <- "prior_id"

bayes_tibble <- function(bayes_file, dna_specimens_tbl,
                         id_column, geom_size_scale = c(4, 7)) {

  # Enquosure for tidy evaluation.
  id_quo <- rlang::enquo(id_column)

  # Read in conensus tree file .nexus file as tibble object.
  bayes_tbl <- treeio::read.beast(bayes_file) %>% ggplot2::fortify()

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
    group_by_at(vars(node, !!id_quo)) %>%
    dplyr::count(name = "geom_size", .drop = FALSE) %>%
    dplyr::left_join(., tbl_tree_long, by = c("node", id_column)) %>%
    dplyr::ungroup()

  # Scale geoms by numeric vector, bind columns, and arrange rows by geom size.
  dplyr::bind_cols(geom_scale = rescale(tbl_tree_merged$geom_size,
    to = geom_size_scale), tbl_tree_merged) %>%
    dplyr::select(node:geom_size, geom_scale, label.x:`Chromosome #`) %>%
    dplyr::arrange(node, desc(geom_scale))

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
#' @inheritParams bayes_tibble
#' @return ggtree object with tips aesthetics determined by `id_column` and
#' posterior probabilities on resolved (i.e. non-polytomy) nodes.
#'
#' @examples
#' id_column <- "prior_id"
#' scale_name <- "Review Annotations"
bayes_ggtree <- function(bayes_tbl, id_column, scale_name) {

  # Filter out polytomy nodes for labels.
  bayes_labels <- bayes_test %>% dplyr::filter(prob != 1 & !is.na(prob))

  # Build ggtree plot from Bayes results.
  ggtree(bayes_test, layout = "rectangular") +
    geom_point(aes_string(color = id_column, shape = id_column,
                          size = "geom_scale"), na.rm = TRUE) +

    # Add labels with rounded posterior probabilities to resolved nodes.
    geom_label(data = bayes_labels,
      aes(x = x, y = y, label = sprintf("%0.3f", as.numeric(prob), digits = 3)),
      nudge_x = (-range(bayes_labels$x[2] * 0.05)),
      fontface = "bold", fill = "lightgoldenrod", size = 4.5, alpha = 0.5) +

    # Adjust color and shape scales with spp_labels() markdown ggtext elements.
    scale_color_manual(scale_name, values = spp_color,
      labels = spp_labels(specimen_tibble = bayes_test, id_column = id_column)) +
    scale_shape_manual(scale_name, values = spp_shape,
      labels = spp_labels(specimen_tibble = bayes_test, id_column = id_column)) +
    theme(legend.text = ggtext::element_markdown()) +

    # Modify scale of size aesthetic and spacing of color / shape guide.
    scale_size_continuous(range = c(3, 9), guide = "none") +
    guides(color = guide_legend(override.aes = list(size = 4),
                                keyheight = 0.35, default.unit = "inch"))
}

#' Add ggtree Plot Tip Labels
#'
#' Adds tip labels to ggtree plot for single-taxa tips with unique genotypes.
#' Multi-taxa tips returned by the `multi_taxa_nodes` are marked by grouped
#' genotype numbered by node order ranking.
#'
#' @param bayes_ggtree_obj ggtree object returned by `bayes_ggtree()` function.
#' @inheritParams bayes_ggtree
#' @return ggtree object with text geoms identifying taxa tip labels.
#' 
bayes_tip_labels <- function(bayes_ggtree_obj, bayes_tbl) {
  
  # Assign tibbles for multi-taxa and single-taxon tip labels.
  multi_taxa_tbl <- multi_taxa_nodes(bayes_tbl)
  single_taxa_tbl <- bayes_test %>%
    dplyr::group_by(node) %>% dplyr::count(vars = "node") %>% 
    dplyr::ungroup() %>% dplyr::right_join(., bayes_test, by = "node") %>%
    dplyr::filter(n == 1) %>% dplyr::select(x, y, label.x)
  
  # Add tip labels for single- and multi-taxa tip labels.
  bayes_ggtree_obj +
    geom_text(data = multi_taxa_tbl,
              aes(x = x, y = y, label = node_group),
              nudge_x = (range(multi_taxa_tbl$x)[2] * 0.03),
              size = 3, hjust = 0, vjust = 0.25, fontface = "bold") +
    geom_text(data = single_taxa_tbl,
              aes(x = x, y = y, label = label.x), na.rm = TRUE,
              nudge_x = (range(single_taxa_tbl$x)[2] * 0.025),
              size = 3, hjust = 0)
}

#' Multi-taxa Tips
#'
#' Filter samples from output of `bayes_tibble()` function to nodes with
#' multiple taxa grouped by identical genotypes.
#'
#' @inheritParams bayes_tibble
#' @return Tibble of filtered multi-taxa tips grouped by node.
#'
multi_taxa_nodes <- function(bayes_tbl) {
  
  # Select tibble variables for filtering multi-taxa tips.
  multi_taxa_nodes <- bayes_test %>%
    dplyr::select(node, label.x, x, y, label.y)
  
  # Filter multi-taxa nodes and create group index for combined genotype.
  multi_taxa_nodes %>%
    dplyr::group_by(node) %>% dplyr::count(vars = "node") %>%
    dplyr::right_join(., multi_taxa_nodes, by = "node") %>%
    dplyr::filter(n > 1) %>% dplyr::arrange(node) %>% dplyr::ungroup() %>%
    dplyr::mutate(node_group = group_indices(., node, n) %>%
                    paste0("genotype ", .))

}

