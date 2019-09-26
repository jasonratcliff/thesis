# Workflow for phylogenetic tree annotation with ggtree R package

# Herbarium tree tibble ----

#' Merge herbarium specimen records with consenus tree data.
#'
#' Read in bayesian inference data from MrBayes consensus tree file as ggtree
#' formal class treedata and combine with herbarium specimen record data.
#'
#' @param bayes_files Character vector of length one with path to MrBayes output
#'   consensus tree `.nexus`` file.
#' @param specimen_records Data frame `total_physaria` containing specimen
#'   record data sourced by the `R/specimen_data.R` script.
#' @param dna_spp_file DNA specimen .csv file containing FASTA header labels and
#'   specimen record collection data from `data/seq-consensus` subdirectory.
#'
#' @examples
#' \dontrun{
#' tbl_phylo_ml <-
#'   phylo_tbl(bayes_file = paste0("data/seq-consensus/",
#'                                 "ml-1.1_infile.nex.con.tre"),
#'             specimen_records = total_physaria)
#' }
#'
phylo_tbl <- function(bayes_file, specimen_records,
                      dna_spp_file = "data/1.specimens/dna_specimens.csv") {

  # Read in conensus tree file .nexus file as tibble object.
  bayes_tbl <- ggtree::read.beast(bayes_file) %>% ggplot2::fortify()

  # Read in specimen record data frame from .csv file as data frame.
  dna_spp <- readr::read_csv(file = dna_spp_file)

  # Establish log file to write which taxa labels don't match DNA_META indices.
  label_resolve_log <-
    file(description = gsub("infile.nex.con.tre", "label_resolve.log",
                            bayes_file), open = "w")

  # Define function to match specimen record by FASTA label to herbarium data.
  spp_match <- function(phylo_label, df_dna_spp, df_specimen_records, ...) {
    df_dna_index <- grep(phylo_label, df_dna_spp$taxa_label)
    indiv_dna_spp <- df_dna_spp[df_dna_index, ]
    indiv_rec_spp <-
      spp_find(taxa_frame = df_specimen_records,
               collector = indiv_dna_spp$Collector,
               collection_number = indiv_dna_spp$Collection_Number,
               priors = TRUE, locale = TRUE, geom = TRUE)[1, ]
    df_spp_match <-
      bind_cols(taxa_label = df_dna_spp[df_dna_index, "taxa_label"],
                as_tibble(indiv_rec_spp))
    return(df_spp_match)
  }

  # Use purrr:: map function to merge beast and herbarium record tibble.
  # https://purrr.tidyverse.org/reference/map2.html
  # https://github.com/tidyverse/purrr/blob/master/R/map2-pmap.R
  bayes_tbl_merge <-
    purrr::pmap_dfr(bayes_tbl, function(node, label, ...) {
      # Merge specimen data for tip labels representing unique sequence sample.
      if (length(which(grepl(label, dna_spp$taxa_label) == TRUE)) == 1) {
        dplyr::bind_cols(node = node, label = label,
                         list(spp_match(phylo_label = label,
                                        df_dna_spp = dna_spp,
                                        df_specimen_records = specimen_records),
                              bayes_tbl[node, grep("node|label",
                                                   names(bayes_tbl),
                                                   invert = TRUE)]))
        } else
        # Resolve mismatch between `bayes_tbl` label and `dna_spp`.
        # For nodes with multiple taxa, combine specimen and bayes data.
          if (!(TRUE %in% grepl(label, dna_spp$taxa_label)) && !is.na(label)) {

            # Write the sample label identifier to a log file.
            cat(file = label_resolve_log, sep = "\n", append = TRUE,
                paste(label, "Does not match DNA_META, i == ", node))

            id_dupes <-  # Extract concatenated labels of identical sequences.
              # The regular expression matches an abbreviated genus prefixed to
              # and abbrevieated specific epithet with a sample collection ID.
              stringr::str_extract_all(label,
                                       "[PL][A-Z]+_[A-Z]?[A-Z]?_?[0-9]+") %>%
                unlist()

            # Nested map of label IDs denoting identical sample sequences.
            map_dfr(.x = id_dupes, .y = rep(x = node, times = length(id_dupes)),
                    .f = function(taxon_label, ...) {
                      dplyr::bind_cols(node = node, label = taxon_label,
                        list(spp_match(phylo_label = taxon_label,
                                       df_dna_spp = dna_spp,
                                       df_specimen_records = specimen_records),
                             bayes_tbl[node, grep("node|label",
                                                  names(bayes_tbl),
                                                  invert = TRUE)]))
                      })
            } else
              # Bind inherited columns from inner tree nodes missing labels.
              if (is.na(label)) {
                dplyr::bind_cols(node = node, label = NA, taxa_label = NA,
                                 bayes_tbl[node, grep("node|label",
                                                      names(bayes_tbl),
                                                      invert = TRUE)])
                }
      })
  close(con = label_resolve_log)
  return(bayes_tbl_merge)
}

# Herbarium tree plot ----

#' Plot merged tibble ggtree with specimen annotations.
#'
#' This function is a wrapper for plotting data from the `phylo_tbl()` function
#' using the `ggtree` R package.  Options for plot configuration include
#' color and shape aesthetics, legend and plot titles, plot scaling, and legend
#' position using the `lemon` R package for 'freshing up' `ggplot2` plots.
#'
#' @param phylo_tbl_obj Tibble output by `phylo_tbl()` function built from
#'   merged BEAST tree and herbarium records.
#' @param spp_id Character vector matching column name for ggplot call
#'   of color and shape aesthetics for ggtree build.
#' @param legend_title Character vector passed to ggplot manual scales.
#' @param plot_title Character vector passed to `ggplot2::ggtitle()`.
#' @param phylo_layout Character vector passed as `layout` argument of
#'   `ggtree::ggtree()`.
#' @param legend_col Numeric vector passed as `ncol` argument of
#'   `ggplot2::guide_legend()`.
#' @param x_expand Numeric vector passed to `x` argument of
#'   `ggplot2::expand_limits()`.
#' @param legend_y_pos Numeric vector of length 2 passed to `legend.position`
#'   argument of `ggplot2::theme()`.
#'
#' @inherit phylo_tbl examples
#'
#' @examples
#'
#' \dontrun{
#' phylo_ggplot(tbl_phylo_ml, plot_title = "Multi-locus Phylogeny",
#'              legend_col = 1, x_expand = 0.02)
#' }
#'
phylo_ggplot <- function(phylo_tbl_obj, spp_id = "Physaria_syn",
                         legend_title = "Previous Annotations",
                         plot_title = "A phylogenetic tree.",
                         phylo_layout = "slanted",
                         legend_col = 2, x_expand = 0.02,
                         legend_y_pos = c(0, 0.9)) {

  # Index vectors to subset tibble by nodes with single or multiple samples.
  index_single_nodes <-
    match(which(dplyr::count(phylo_tbl_obj,
                             node)[, "n"] == 1), phylo_tbl_obj$node)
  index_multi_nodes <-
    sapply(which(dplyr::count(phylo_tbl_obj, node)[, "n"] > 1),
           USE.NAMES = FALSE, function(node) {
             which(phylo_tbl_obj$node %in% node == TRUE)
             }) %>% unlist()

  # Filter rows for inner (non-tip) nodes as tibble.
  tbl_inner_node <- dplyr::filter(phylo_tbl_obj, isTip == FALSE)

  # Select tibble from nodes with multiple specimen labels.
  tbl_multi_node <-
    phylo_tbl_obj[index_multi_nodes, ] %>%
    dplyr::select(everything()) %>%
    dplyr::bind_cols(., row_name_immut = seq_along(1:nrow(.)))  # row index

  # Extend tibble columns to include geom size calculations for plotting.
  tbl_multi_node_ext <-
    purrr::pmap_dfr(tbl_multi_node,
      function(node, row_name_immut, ...) {

        # Filter tibble of rows matching mapped node value.
        node_check <- node
        tbl_node_subset <-
          dplyr::select(tbl_multi_node,
                        one_of("node", spp_id, "row_name_immut")) %>%
          dplyr::filter(., tbl_multi_node["node"] == node_check) # %>%

        # Order table of specimen identifications into tibble.
        node_table <- select(tbl_node_subset, spp_id) %>% table()
        node_order <- node_table[order(node_table, decreasing = TRUE)]
        tbl_spp_order <-
          bind_cols(species = names(node_order), n = node_order)

        # Arrange tibble by ordered factor of specimen counts.
        tbl_order_nodes <- tbl_node_subset %>%
          dplyr::arrange(., factor(
            select(tbl_node_subset, spp_id)[[1]],
            levels = tbl_spp_order$species, ordered = TRUE))

        # Calculate geom size from column mutations.
        tbl_node_plot <-
          dplyr::mutate(tbl_order_nodes,
                        geom_size = seq(from = 7, to = 5,
                                        length.out = nrow(tbl_order_nodes)) %>%
                          round(., digits = 2),
                        alpha_val =  seq(from = 0.4, to = 0.6,
                                         length.out = nrow(tbl_order_nodes)) %>%
                          round(., digits = 2))

        # Mutate join of geom sizes by row number constant.
        tbl_node_merge <-
          dplyr::full_join(tbl_multi_node[row_name_immut, ],
                           tbl_node_plot[grep(paste0("^", row_name_immut, "$"),
                                              tbl_node_plot$row_name_immut), ],
                           by = "row_name_immut")
        dplyr::bind_cols(data = tbl_node_merge)
        })

  # Create tibble for multi-specimen node label annotation.
  tbl_multi_node_labels <-
    dplyr::arrange(tbl_multi_node_ext,
                   factor(tbl_multi_node_ext$node.x,
                          levels = names(sort(table(tbl_multi_node_ext$node.x),
                                              decreasing = TRUE)))) %>%
    dplyr::select(., node.x, x, y) %>%
    dplyr::mutate(., Genotype =
                    map_dbl(.x = .[["node.x"]], function(node) {
                      which(unique(.[["node.x"]]) %in% node)
                    })) %>% dplyr::distinct() %>%
    dplyr::mutate(.,
      genotype_label =
        purrr::map_chr(Genotype,
                       function(genotype) {
                         paste("Genotype", as.character(genotype))
                         }))

  # Plot ggtree object with annotations of specimen record and collection label.
  phylo_ggtree <-
    ggtree(phylo_tbl_obj, layout = phylo_layout) +

    # Map tips of genotypes from multiple samples with identical sequence.
    geom_point(data = tbl_multi_node_ext,
               aes_string(colour = paste0(spp_id, ".x"),
                          shape = paste0(spp_id, ".x")),
               size = tbl_multi_node_ext$geom_size,
               alpha = tbl_multi_node_ext$alpha_val,
               na.rm = TRUE) +
    geom_point(data = tbl_multi_node_ext, size = 1.5,
               color = "black", shape = 18) +
    geom_text(data = tbl_multi_node_labels,
              aes(x = x, y = y, label = genotype_label),
              nudge_x = (range(tbl_inner_node$x)[2] * 0.05),
              size = 3.5, hjust = 0, fontface = "bold") +

    # Map text strings of probabilities to inner nodes as labels.
    geom_label(data = tbl_inner_node,
               aes(x = x, y = y,
                   label = sprintf("%0.3f", as.numeric(tbl_inner_node$prob),
                                   digits = 3)),
               nudge_x = (-range(tbl_inner_node$x)[2] * 0.05),
               fontface = "bold", fill = "lightgoldenrod",
               size = 4.5, alpha = 0.5) +

    # Map tips with unique genotypes by species identity and collection label.
    geom_point(data = phylo_tbl_obj[index_single_nodes, ],
               aes_string(colour = spp_id, shape = spp_id),
               size = 3, na.rm = TRUE) +
    geom_text(data = phylo_tbl_obj[index_single_nodes, ],
              aes(x = x, y = y, label = label), na.rm = TRUE,
              nudge_x = (range(tbl_inner_node$x)[2] * 0.05),
              size = 3.5, hjust = 0) +

    # Theme adjustment for legend and scales.
    theme(legend.position = legend_y_pos,
          legend.justification = c(0, 1),
          legend.direction = "vertical",
          legend.text = element_text(size = 7),
          legend.box.background = element_blank()) +
    guides(colour = guide_legend(ncol = legend_col, byrow = TRUE)) +
    scale_color_manual(legend_title, values = spp_color) +
    scale_shape_manual(legend_title, values = spp_shape) +
    expand_limits(x = x_expand) +
    geom_treescale() +
    ggtitle(plot_title)

    # Reposition legend with R package `lemon`.
    phylo_lemon <-
      lemon::reposition_legend(phylo_ggtree, 'top left')
}

# Kable wrappers ----

#' Multi-specimen node tibble.
#'
#' Subset and filter tibbles output by `phylo_tbl()` for `knitr::kable()` plot.
#' Calculates a column `Genotype` based on the node frequency, where genotype
#' `1` is the genotype containing the most identical specimens.  The column
#' `Species` is based on the input of the `spp_id` argument.
#'
#' @inheritParams phylo_ggplot
#'
#' @examples
#' \dontrun{
#'   phylo_multi_tbl(phylo_tbl_obj = tbl_phylo_ml) %>%
#'     phylo_kable(tbl_multi_node = .,
#'                 kable_caption =
#'                   paste0("$Physaria$ DNA specimens with identical sequences",
#'                          " after concatenation."))
#' }
#'
phylo_multi_tbl <- function(phylo_tbl_obj,
                            spp_id = "Physaria_syn") {

  # Index vectors to subset tibble by nodes with multiple samples.
  index_multi_nodes <-
    sapply(which(dplyr::count(phylo_tbl_obj, node)[, "n"] > 1),
           USE.NAMES = FALSE, function(node) {
             which(phylo_tbl_obj$node %in% node == TRUE)
           }) %>% unlist()
  tbl_multi_node <-
    phylo_tbl_obj[index_multi_nodes, ] %>%
    dplyr::select(everything()) %>%
    dplyr::bind_cols(., row_name_immut = seq_along(1:nrow(.)))  # row index

  # Build kable for rows with multiple specimens per node.
  kable_multi_node <-
    dplyr::select(tbl_multi_node, node, State, spp_id,
                        Collector, Collection_Number) %>%
    dplyr::arrange(., factor(tbl_multi_node$node,
                             levels = names(sort(table(tbl_multi_node$node),
                                                 decreasing = TRUE)))) %>%
    dplyr::mutate(., Genotype =
                    map_dbl(.x = .[["node"]], function(node) {
                      which(unique(.[["node"]]) %in% node)
                      })) %>%
    dplyr::select(., Genotype, spp_id, State, Collector, Collection_Number) %>%
    dplyr::mutate(., Collector =
                    map_chr(.[["Collector"]], function(collector) {
                      gsub("[A-Z]\\. ?", "", collector) %>%
                        gsub("&|with", "and", x = .)
                      })) %>%
    dplyr::rename(., Species = spp_id,
                  `Collection Number` = Collection_Number)
  return(kable_multi_node)
}

#' Kable build for multi-specimen nodes.
#'
#' @inherit phylo_multi_tbl examples
#'
phylo_kable <- function(tbl_multi_node, kable_caption) {
  kable(tbl_multi_node, caption = kable_caption, format = knitr_chunk,
        escape = F, row.names = FALSE,
        align=c("c", "c", "l", "l", "l")) %>%
    kable_styling(full_width = FALSE, font_size = 10,
                  latex_options= "hold_position") %>%
    row_spec(row = 0, bold = TRUE, font_size = 10) %>%
    column_spec(1, border_left = TRUE) %>%
    column_spec(2, width = "3.7cm") %>%
    column_spec(5, border_right = TRUE, width = "2cm") %>%
    collapse_rows(columns = 1:3)
}

