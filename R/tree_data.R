# Tree Data ----

#' Read in MrBayes Consensus Tree
#'
#' Given an input Nexus file with results from a Mr. Bayes run, split rows of
#' nodes with identical sample genotypes and join in data from
#' [Thesis::dna_specimens].
#'
#' @param tree_file Nexus file of MrBayes results to read in by `treeio`.
#' @export
#'
#' @return Tibble for [ggtree::ggtree()] plotting with joined DNA specimen data.
#'
#' @examples
#' list.files(system.file("extdata/MrBayes", package = "Thesis"),
#'            full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
#'   read_tree(tree_file = .) %>%
#'   dplyr::select(node, single_label, label)
#'
read_tree <- function(tree_file) {
  ggtree_import <- treeio::read.beast(tree_file) %>% ggtree::fortify()
  tree_data <- ggtree_import %>%
    dplyr::select("label", "node") %>%
    purrr::pmap_dfr(.l = ., function(label, node) {
      dplyr::bind_rows(
        node = rep(node),
        single_label = stringr::str_split(
          string = label,
          pattern = "(?<=[0-9])_(?=[A-Z])"
        ) %>% unlist()
      )
    }) %>%
    dplyr::right_join(x = ., y = ggtree_import, by = "node") %>%
    dplyr::left_join(x = .,
      y = Thesis::dna_specimens,
      by = c("single_label" = "label")
    )
  return(tree_data)
}

#' Label Multi-taxa Nodes
#'
#' For tips (i.e. terminal nodes) with multiple taxa split into
#' `single_label` by [Thesis::read_tree], create a new variable
#' `node_group` to distinguish multi-sample genotype labels.
#'
#' @param tree_data Tibble output by [read_tree()] with ggtree node and
#'   label variables.
#' @export
#'
#' @return Tibble with `node`, `single_label`, `label`, and `node_group`
#'   variables where `node_group` represents multi-sample terminal nodes.
#'
#' @examples
#' list.files(system.file("extdata/MrBayes", package = "Thesis"),
#'            full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
#'   read_tree(tree_file = .) %>%
#'   node_labels(tree_data = .)
#'
node_labels <- function(tree_data) {

  ggtree_labels <- tree_data %>%
    dplyr::select("node", "single_label", "label")

  multi_taxa_nodes <- ggtree_labels %>%
    dplyr::add_count(.data$node) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::group_by(.data = ., dplyr::desc(.data$n), .data$node) %>%
    dplyr::mutate(
      node_group = dplyr::cur_group_id() %>%
        paste0("Genotype ", ., " (n=", .data$n, ")")
    ) %>%
    dplyr::ungroup()

  ggtree_labels <- multi_taxa_nodes %>%
    dplyr::select("node", "single_label", "label", "node_group") %>%
    dplyr::right_join(x = ., y = ggtree_labels,
                      by = c("node", "single_label", "label")) %>%
    dplyr::arrange(.data$node)

  return(ggtree_labels)
}

#' Scale Node Geoms
#'
#' Group [ggtree::ggtree()] samples by `node` and `id_column` to
#' scale geom point sizes for tree tips.
#'
#' @param scale_vector Numeric vector of length two passed to
#'   [scales::rescale()] `to` argument.
#' @inheritParams node_labels
#' @inheritParams layer_specimens
#' @export
#'
#' @return Tibble with `node`, `id_column`, and `geom_size` variables
#'   where `geom_size` represents geom sizes scaled by `node` and `id_column`.
#'
#' @examples
#' list.files(system.file("extdata/MrBayes", package = "Thesis"),
#'            full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
#'   read_tree(tree_file = .) %>%
#'   node_geoms(tree_data = ., id_column = "prior_id")
#'
node_geoms <- function(tree_data, id_column, scale_vector = c(4, 12)) {
  ggtree_geoms <- tree_data %>%
    dplyr::select("node", "single_label", "label", !!id_column) %>%
    dplyr::group_by(.data$node, !!rlang::sym(id_column)) %>%
    dplyr::count(name = "geom_size") %>% dplyr::ungroup() %>%
    dplyr::mutate(
      geom_size = scales::rescale(.data$geom_size, to = scale_vector)
    )
  return(ggtree_geoms)
}

#' Join MrBayes Import with Labels and Geoms
#'
#' Combine condensed node labels for multi-sample tips from [node_labels()]
#' with scaled geom sizes from [node_geoms()].
#'
#' @param ... Forwarding arguments to [node_geoms()] (e.g., `scale_vector`).
#' @inheritParams node_geoms
#' @export
#'
#' @return Tibble of multi-sample split node labels and geom sizes joined to
#'   MrBayes import by [read_tree()] and [Thesis::dna_specimens] data.
#'
#' @examples
#' joined_ggtree <-
#'   list.files(system.file("extdata/MrBayes", package = "Thesis"),
#'              full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
#'   read_tree(tree_file = .) %>%
#'   join_bayes(tree_data = ., id_column = "prior_id")
#'
join_bayes <- function(tree_data, id_column, ...) {
  joined_ggtree <- tree_data %>%
    dplyr::left_join(x = .,
      y = Thesis::node_labels(tree_data = tree_data),
      by = c("node", "single_label", "label")
    ) %>%
      dplyr::left_join(x = .,
      y = Thesis::node_geoms(
        tree_data = tree_data, id_column = id_column, ...
      ),
      by = c("node", id_column)) %>%
    dplyr::mutate(
      geom_size = purrr::map2_dbl(
        .x = .data$single_label, .y = .data$geom_size,
        function(label, size) {
          ifelse(!is.na(label), size, NA)
        })
    ) %>%
    dplyr::select("node", "node_group", "geom_size", "single_label",
                  dplyr::everything()) %>%
    dplyr::group_by(.data$node) %>%
    dplyr::arrange(dplyr::desc(.data$geom_size)) %>% dplyr::ungroup()
  return(joined_ggtree)
}

# Kable Wrappers ----

#' Multi-taxa Node Specimens
#'
#' Identify specimens with conserved DNA sequences yielding identical nodes
#' (i.e., terminal tips) for phylogenetic inference.
#'
#' @param id_name Name to replace `id_column` variable in kable output.
#' @inheritParams read_tree
#' @inheritParams layer_specimens
#' @export
#'
#' @return Tibble of specimens comprising multi-taxa nodes with identical
#'   DNA sequences for MrBayes analysis.
#'
#' @examples
#' list.files(system.file("extdata/MrBayes", package = "Thesis"),
#'            full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
#'   conserved_vouchers(tree_file = .,
#'                      id_column = "prior_id", id_name = "Species")
#'
conserved_vouchers <- function(tree_file, id_column, id_name) {
  tree_data <- Thesis::read_tree(tree_file = tree_file)
  conserved_specimens <- Thesis::node_labels(tree_data) %>%
    dplyr::filter(!is.na(.data$node_group)) %>%
    dplyr::left_join(x = ., y = Thesis::dna_specimens,
                     by = c("single_label" = "label")) %>%
    dplyr::select("node_group", !!id_column, "State",
                  "Collector", "Collection_Number") %>%
    dplyr::mutate(
      Collector = purrr::map_chr(.x = .data$Collector, function(collector) {
        gsub("[A-Z]\\. ?", "", collector) %>%
          gsub("&|with", "and", x = .) %>%
          gsub(" +", " ", x = .) %>% stringr::str_squish(string = .)
      })
    ) %>%
    dplyr::rename(
      `Node Group` = .data$node_group,
      !!id_name := !!id_column,
      `Collection Number` = .data$Collection_Number
    )
  return(conserved_specimens)
}

#' Kable build for multi-specimen nodes.
#'
#' For specimens with multi-taxa nodes (i.e., terminal tips), build
#' `kable` object with `kableExtra` formatting.
#'
#' @param conserved_specimens Voucher tibble output by [conserved_vouchers()]
#' @param kable_caption Character vector for caption passed to
#'   [knitr::kable()] argument `caption`.
#' @param knitr_chunk Character vector passed to [knitr::kable()] argument
#'  `format` - one of "latex" or "html".  Defined by
#'  `opts_knit$get("rmarkdown.pandoc.to")` in `index.Rmd` of Thesis Bookdown.
#' @export
#'
#' @return `kable` object with `kableExtra` formatting determined by
#'   `knitr::opts_knit$get("rmarkdown.pandoc.to")`
#'
#' @examples
#' list.files(system.file("extdata/MrBayes", package = "Thesis"),
#'            full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
#'   conserved_vouchers(tree_file = .,
#'                      id_column = "prior_id", id_name = "Species") %>%
#'   bayes_kable(conserved_specimens = .,
#'               kable_caption = "ggtree Specimens", knitr_chunk = "html")
#'
bayes_kable <- function(conserved_specimens, kable_caption, knitr_chunk) {
  kable_build <-
    knitr::kable(x = conserved_specimens,
      caption = kable_caption, format = knitr_chunk, escape = FALSE,
      row.names = FALSE, align=c("c", "c", "l", "l", "l")) %>%
    kableExtra::kable_styling(
      full_width = FALSE, font_size = 10,  latex_options= "hold_position"
    ) %>%
    kableExtra::row_spec(row = 0, bold = TRUE, font_size = 10) %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(5, border_right = TRUE, width = "1.8cm") %>%
    kableExtra::collapse_rows(columns = 1:3)
  return(kable_build)
}
