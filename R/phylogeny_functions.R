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
    dplyr::group_by_at(vars(.data$node, !!id_quo)) %>%
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

