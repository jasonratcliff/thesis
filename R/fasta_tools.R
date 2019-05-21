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

#' Check FASTA sequence headers and sequence content.
#' 
#' Tool to verify FASTA headers are identical between two FASTA files 
#' (e.g. pre- and post-alignment files).  Returns text to stdout by `cat` with
#' information about number of sequences and range of sequence lengths.  
#' FASTA files should have consistent naming conventions for sorting
#' (e.g. [loci]-subset.fasta, [loci]-aligned.fasta).
#' 
#' @param fasta_path1 Path to subset FASTA files.
#' @param fasta_path2 Path to aligned FASTA files.
#' 
#' @examples
#' fasta_check(fasta_path1 = "data/subset",
#'             fasta_path2 = "data/alignments-concatenated")
#' # File 1: data/subset/rITS-subset.fasta 
#' # File 2: data/alignments-concatenated/rITS-ml_aligned.fasta 
#' # Sequence headers agree:  TRUE 
#' # Number of sequences:  45 
#' # Sequence range data/subset/rITS-subset.fasta :  
#' #   581 583 
#' # Sequence range data/alignments-concatenated/rITS-ml_aligned.fasta :  
#' #   586 586 
#' # .... [TRUNCATED] 
fasta_check <- function(fasta_path1, fasta_path2) {
  
  # Assign DNAStringSets from original sequence analysis pipeline trimming.
  fasta_files1 <- list.files(fasta_path1, pattern = ".fasta$", 
                                  full.names = TRUE)
  fasta_list1 <- lapply(fasta_files1, readDNAStringSet)
  
  # Assign DNAStringSets from R sequence analysis pipeline trimming.
  fasta_files2 <- list.files(fasta_path2, pattern = ".fasta$", 
                                   full.names = TRUE)
  fasta_list2 <- lapply(fasta_files2, readDNAStringSet)
  
  # Compare sorted headers in each gene between old and new trimming pipeline.
  for (i in seq_along(fasta_list1)) {
    cat("File 1:", fasta_files1[i], "\n")
    cat("File 2:", fasta_files2[i], "\n")
    fasta_compare <- identical((names(fasta_list1[[i]]) %>% sort()), 
                               (names(fasta_list2[[i]]) %>% sort()))
    if (fasta_compare == FALSE) {
      quit("Sequence header difference for: ", fasta_files2[[i]])
    }
    cat("Sequence headers agree: ", fasta_compare, "\n")
    cat("Number of sequences: ",
        length(fasta_list2[[i]]), "\n")
    cat("Sequence range", fasta_files1[[i]], ": ", "\n\t",
        range(sapply(fasta_list1[[i]], length)), "\n")
    cat("Sequence range", fasta_files2[[i]], ": ", "\n\t",
        range(sapply(fasta_list2[[i]], length)), "\n\n")
  }
}

# Write FASTA file subset from common sequence headers.
fasta_subset(fasta_path = "data/raw")

# Compare sequence headers and length pre- and post-alignment.
fasta_check(fasta_path1 = "data/subset",
            fasta_path2 = "data/alignments-concatenated")            

