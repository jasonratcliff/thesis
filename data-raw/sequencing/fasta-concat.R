## FASTA Concatenation ----

# Assign list of DNAStringSet objects from FASTA files in input directory.
ml_aligned <-
  list.files(path = "data-raw/sequencing/3.alignments-concatenated/",
             pattern = ".fasta$", full.names = TRUE) %>%
  lapply(X = ., Biostrings::readDNAStringSet)

# Assign lists of FASTA header name vectors and associated loci.
header_list <- purrr::map(ml_aligned, names)

# Extract locus names.
fasta_loci <- purrr::map(header_list, function(locus_headers) {
  purrr::map_chr(locus_headers, function(header) {
    stringr::str_extract(string = header, pattern = "(?<= ).+$")
  }) %>% unique()
})

# Remove gene metadata from list of FASTA headers and test split.
ml_specimens <- purrr::map(header_list, function(locus_headers) {
  purrr::map_chr(locus_headers, function(header) {
    stringr::str_extract(string = header, pattern = "^.+(?= )")
  })
})

# Check for string extract error by comparing length of list elements.
if (!identical(lapply(header_list, length),
               lapply(ml_specimens, length))) {
  stop("Error in FASTA header splitting.")
}

# Identify common FASTA headers among split names.
fasta_common <- Reduce(intersect, ml_specimens) %>% sort()

# Grep common names against DNAStringSet names for subset indexing.
fasta_matches <- purrr::map(header_list, function(locus_headers) {
  purrr::map_int(fasta_common, function(fasta_header) {
    grep(fasta_header, locus_headers)
  })
})

# Subset FASTA sequences by index of common FASTA headers and write to file.
ml_concat <- purrr::map2(ml_aligned, fasta_matches,
                         function(fasta, index) {
                           fasta[index]
                           })

# Concatenate DNAStringSet elements
ml_concat <- Biostrings::xscat(ml_concat[[1]], ml_concat[[2]], ml_concat[[3]])
names(ml_concat) <- fasta_common  # Add names attribute from sorted headers.

# Write DNAStringSet to FASTA file.
Biostrings::writeXStringSet(ml_concat, width = 1999,
  filepath = paste0("data-raw/sequencing/3.multi-locus/",
                    "ml_concatenated.fasta"))

