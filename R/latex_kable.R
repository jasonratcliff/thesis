# Function to format IDENTICAL_SEQS data frame .csv written from subset
# of LABEL_RESOLVE by tree_taxa() function sourced by herb_tree.R
#
# # LABEL_RESOLVE_CSV: path to identical sequence file written by tree_taxa().
# # TABLE_SPLIT: numeric vector should denote inclusive row breakpoints.
# # TABLE_NAMES: define names of subset from IDENTICAL_SEQS data frame.
#
kable_format <- function(label_resolve_csv, table_split = NULL,
                         table_names = c("node", "State", "Physaria_syn", 
                                         "Collector", "Collection_Number")) {
  id_seqs <- read.csv(file = label_resolve_csv,
                        header = TRUE, row.names = NULL, as.is = TRUE,
                        check.names = FALSE, stringsAsFactors = FALSE, 
                        colClasses = "character")
  id_seqs_table <- id_seqs[, table_names]
  names(id_seqs_table) <- c("node", "State", "Species", 
                            "Collector", "Collection Number")
  
  # Clean up "Collector" column
  id_seqs_table$Collector <- gsub("&", "and", id_seqs_table$Collector)
  id_seqs_table$Collector <- gsub("[A-Z]\\.[A-Z]\\.", "", id_seqs_table$Collector)
  id_seqs_table$Collector <- gsub("[A-Z]\\. ", "", id_seqs_table$Collector)
  id_seqs_table$Collector <- gsub("^,? ", "", id_seqs_table$Collector)
  id_seqs_table$Collector <- gsub("with", "and", id_seqs_table$Collector)
  
  # Add italicization to the "Species" column
  species <- sapply(id_seqs_table$Species,
                    USE.NAMES = FALSE, simplify = TRUE,
                    function(species) {
                      if (grepl("ssp\\. ", species) == TRUE) {
                        # print(species)
                        ssp <- strsplit(species, split = " ssp\\. ")
                        subsp <- sapply(ssp, USE.NAMES = FALSE,
                                        simplify = TRUE, function(subspecies) {
                                          paste0("$", subspecies, "$")
                                        })
                        ssp <- paste0(subsp[1], " ssp. ", subsp[2])
                      } else {
                        ssp <- strsplit(species, split = " ")
                        ssp <- sapply(ssp, USE.NAMES = FALSE,
                                      simplify = TRUE, function(spp_split) {
                                        paste0("$", spp_split, "$")
                                      })
                        ssp <- paste(ssp[1], ssp[2])
                      }
                      return(ssp)
                    })
  species <- gsub(" textit", "textit", species)
  id_seqs_table$Species <- species
  
  # Correct node numbering to reflecting genotype numbering in taxa_label().
  i <- 1
  for (node in unique(id_seqs_table$node)) {
    id_seqs_table$node[which(id_seqs_table$node == node)] <- i
    i <- i + 1
  }
  names(id_seqs_table)[names(id_seqs_table) == "node"] <- "Genotype"
  
  # Option to split kable by genotypes into multiple .csv files.
  if (!is.null(table_split)) {
    if (!is.numeric(table_split)) { stop("Enter numeric value for table_split")
    } else {
      i <- 1
      j <- 1
      for (genotype in table_split) {
        genotypes <- seq(i, genotype)
        row_index <- lapply(genotypes, #simplify = TRUE, USE.NAMES = FALSE,
                            function(gt) { 
                              which(id_seqs_table$Genotype %in% gt)
                            })
        id_seq_table_subset <- id_seqs_table[unlist(row_index), ]
        id_seq_filename <- unlist(strsplit(label_resolve_csv, split = "/"))
        id_seq_filename <- id_seq_filename[length(id_seq_filename)]
        id_seq_filename <- gsub(".csv", "", id_seq_filename)
        id_seq_filename <- paste0("Phys_DNA/Seq-Tables/",
                                  id_seq_filename, "-table", j, ".csv")
        write.csv(id_seq_table_subset, file = id_seq_filename, row.names = F)
        i <- genotype + 1
        j <- j + 1
      }
    }
  } else {
    id_seq_filename <- gsub(".csv", "", label_resolve_csv)
    id_seq_filename <- paste0(id_seq_filename, "-table.csv")
    write.csv(id_seqs_table, file = id_seq_filename, row.names = F)
  }
}
  chunk_type <- opts_knit$get("rmarkdown.pandoc.to")
    kable_styling(full_width = FALSE, font_size = 10,
                  latex_options= "hold_position") %>%
    row_spec(row = 0, bold = TRUE, font_size = 10) %>%
    column_spec(1, border_left = TRUE) %>%
    column_spec(3, width = "3.6cm") %>%
    column_spec(5, border_right = TRUE, width = "2cm") %>%
  return(id_seq_kable)
}