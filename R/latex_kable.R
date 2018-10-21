latex_kable <- function(label_resolve_csv, kable_caption, table_split = NULL,
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
  id_seqs_table$Collector <- gsub("^ ", "", id_seqs_table$Collector)
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
                        ssp <- paste0(subsp[1], " spp. ", subsp[2])
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
  
  # knitr::kable with additional kableExtra:: options
  chunk_type <- opts_knit$get("rmarkdown.pandoc.to")
  id_seq_kable <- kable(id_seqs_table, format = chunk_type, escape = F, 
                        row.names = FALSE) %>%
    kable_styling(full_width = FALSE, font_size = 10,
                  latex_options= "hold_position") %>%
    row_spec(row = 0, bold = TRUE, font_size = 10) %>%
    column_spec(1, border_left = TRUE) %>%
    column_spec(3, width = "3.6cm") %>%
    column_spec(5, border_right = TRUE, width = "2cm") %>%
    collapse_rows(columns = 1:3) %>%
    kableExtra::footnote(general = kable_caption, general_title = "",
                         threeparttable = TRUE, escape = FALSE)
  return(id_seq_kable)
}