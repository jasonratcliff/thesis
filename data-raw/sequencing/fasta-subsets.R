## FASTA Subsetting ----

# Assign list of raw FASTA files from project subdirectory.
fasta_files <-
  list.files(system.file("extdata/FASTA", package = "ThesisPackage"),
             full.names = TRUE)

## Read FASTA into R ----

# Assign list of DNAStringSet objects from FASTA files in input directory.
fasta_list <- purrr::map(fasta_files, Biostrings::readDNAStringSet)

## Extract Header Names ----

# Assign lists of FASTA header name vectors and associated loci.
# Extract character vectors from names attributes.
header_list <- purrr::map(fasta_list, names)

## Three loci are expected:
# - rITS
# - *rps*
# - *ycf1*

fasta_loci <- purrr::map(header_list, function(locus_headers) {
  purrr::map_chr(locus_headers, function(header) {
    stringr::str_extract(string = header, pattern = "(?<= ).+$")
  }) %>% unique()
})

## Log FASTA Headers ----

# Loci extracted from the FASTA headers are assigned as the names attribute of
# the sequence header list.

# Assign names attribute to list of FASTA headers.
names(header_list) <- fasta_loci

# Write headers to locus-specific .txt files to log header extraction.
header_directory <- "data-raw/sequencing/1.raw-fastas/"
if (!dir.exists(header_directory)) {
  dir.create(header_directory)
}
purrr::walk2(header_list, fasta_loci, function(headers, locus) {
  writeLines(
    text = headers,
    con = paste0(header_directory, "headers-", locus, ".txt")
  )
})

## Reduce Header List ----

# Remove gene metadata from list of FASTA headers.
specimen_list <- purrr::map(header_list, function(locus_headers) {
  purrr::map_chr(locus_headers, function(header) {
    stringr::str_extract(string = header, pattern = "^.+(?= )")
    })
  })

# Check for string extract error by comparing length of list elements.
if (!identical(lapply(header_list, length),
               lapply(specimen_list, length))) {
  stop("Error in FASTA header splitting.")
}

# Identify common FASTA headers among split names.
fasta_common <- Reduce(intersect, specimen_list) %>% sort()

# Grep common names against DNAStringSet names for subset indexing.
fasta_matches <- purrr::map(header_list, function(locus_headers) {
  purrr::map_int(fasta_common, function(fasta_header) {
    grep(fasta_header, locus_headers)
  })
})

# Subset the DNAStringSets by the header indexes and the common sequences from
# each locus to a new .fasta file.
purrr::pwalk(list(fasta_matches, fasta_list, fasta_loci),
              function(index, fasta, locus) {
               Biostrings::writeXStringSet(x = fasta[index], width = 50,
                 filepath = paste0("data-raw/sequencing/2.subset-fastas/",
                                   locus, "-subset.fasta"))
             })

