library(Biostrings)
library(magrittr)

#' Write FASTA files with intersecting sample headers.
#' 
#' The Biostrings library is used to read in FASTA files as DNAStringSets
#' to compare intersecting sequence headers.  New FASTA files are written
#' with the subset of intersecting headers common to all input FASTA files.
#' FASTA headers are formatted by two fields '[sample] [loci]' with a single
#' whitespace separating the sample identifier from the name of the loci.
#' For example: ">PACUT_48 rITS" or ">PACUT_12821 rps" where a given FASTA file
#' is assumed to contain sequences sampled from a single genetic locus and
#' all FASTA headers in the file contain the locus name in the second field.
#' 
#' @param fasta_path File path containing raw FASTA files to compare.
#' 
#' @examples
#' fasta_subset(fasta_path = "data/raw")
#' list.files("data/subset")
#' # [1] "rITS-subset.fasta" "rps-subset.fasta"  "ycf1-subset.fasta"
fasta_subset <- function(fasta_path) {
  
  # Assign list of DNAStringSet objects from FASTA files in input directory.
  fasta_files <- list.files(fasta_path, pattern = ".fasta$", full.names = TRUE)
  fasta_list <- lapply(fasta_files, readDNAStringSet)
  name_list <- lapply(fasta_list, names)  # List of FASTA header name vectors.
  
  # Remove gene metadata from list of FASTA headers.
  name_split <- lapply(name_list, function(list_element) {
    sapply(list_element, USE.NAMES = FALSE,  function(fas_header) {
      split_name <- unlist(strsplit(x = fas_header, " "))[1]
    })
  })
  
  # Assign unique loci names from each FASTA file.
  fasta_genes <- lapply(name_list, function(list_element) {
    unique(sapply(list_element, USE.NAMES = FALSE,  
                  function(fas_header) {
                    split_name <- unlist(strsplit(x = fas_header, " "))[2]
                  }))
  })
  
  # Check length of DNASringSet names equals split name vectors.
  if (!identical(lapply(name_list, length),
                 lapply(name_split, length))) {
    stop("Error in FASTA header splitting.")
  }
  
  # Identify common FASTA headers among split names.
  fasta_common <- Reduce(intersect, name_split)
  
  # Grep common names against DNAStringSet names for subset indexing.
  name_matches <- lapply(name_list, function(list_element) {
    sapply(fasta_common, function(fasta_header) {
      grep(fasta_header, list_element)
    })
  })
  
  # Subset FASTA sequences by index of common FASTA headers and write to file.
  if (!dir.exists("data/subset/")) {
    dir.create("data/subset/")
  }
  fasta_subsets <- list()
  for (i in seq_along(name_matches)) {
    fasta_subsets[[i]] <- fasta_list[[i]][name_matches[[i]]]
    writeXStringSet(x = fasta_subsets[[i]], 
                    filepath = paste0("data/subset/", fasta_genes[[i]],
                                      "-subset.fasta"))
  }
}

fasta_subset(fasta_path = "data/raw")

