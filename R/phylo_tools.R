# Workflow for phylogenetic tree annotation with ggtree R package

### Herbarium tree tibble ----

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
phylo_tbl <- function(bayes_file, specimen_records,
                      dna_spp_file = "data/phylogeny/dna_specimens.csv") {

  # Read in conensus tree file .nexus file as tibble object.
  bayes_tbl <- treeio::read.mrbayes(bayes_file) %>% ggplot2::fortify()

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
              stringr::str_extract_all(label, "[PL][A-Z]+_[0-9]+") %>% unlist()

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

