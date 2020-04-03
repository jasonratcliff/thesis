## FASTA Subsetting ----

# Multi-FASTA files from sequenced loci were assembled from chromatograms
# and deposited to the`data-raw/sequencing/1.raw-fastas/` package subdirectory.
# Here, FASTA headers are formatted with two fields `accession` and `locus`
# following ">". A single whitespace separates the sample accession from the
# name of the locus, for example: `>PACUT_48 rITS` or `>PACUT_12821 rps`.
# A given FASTA file contains sequences sampled from a single genetic locus and
# all FASTA headers in the file contain the same locus name in the second field.

# Assign list of raw FASTA files from project subdirectory.
fasta_files <-
  list.files(path = "data-raw/sequencing/1.raw-fastas",
             pattern = ".fasta$", full.names = TRUE)

## Read FASTA into R ----

# The Biostrings package (Pagès et al. 2019) was used to read in FASTA files as
# DNAStringSets, R S4 objects of the XString subclass.  A list is assigned to
# hold the DNAStringSets read from the FASTA files, where each element of the
# list contains the DNA sequences read from a single FASTA file.

# Assign list of DNAStringSet objects from FASTA files in input directory.
fasta_list <- purrr::map(fasta_files, Biostrings::readDNAStringSet)

## Extract Header Names ----

# To find the intersecting set of sequence headers among multiple FASTA files,
# the DNAStringSet names attributes are extracted and assigned to a list.

# Assign lists of FASTA header name vectors and associated loci.
header_list <- purrr::map(fasta_list, names)

# FASTA loci separated by whitespace from the specimen accession label are
# extracted by mapping the names attribute character vectors in the header list
# object to match the locus preceded by whitespace. The unique elements of the
# mapped name vectors are returned as a list.

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
purrr::walk2(header_list, fasta_loci, function(headers, locus) {
  writeLines(text = headers,
             con = paste0("data-raw/sequencing/1.raw-fastas/",
                          "headers-", locus, ".txt"))
  })

## Reduce Header List ----

# To identify the intersecting set of sequence headers, the locus name is
# removed from the sequence headers and vectors of the sample accessions are
# stored in a new list.

# Remove gene metadata from list of FASTA headers and test split.
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

# The intersecting set of FASTA headers common to all input files is used to
# index `fasta_list` containing the DNAStringSet sequences.

# Identify common FASTA headers among split names.
fasta_common <- Reduce(intersect, specimen_list) %>% sort()

# For each vector of FASTA headers in `header_list`, index the common headers
# agains the full locus FASTA headers.

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

## Sequence Alignment ----

# Untrimmed, single locus FASTA files from `data-raw/sequencing/1.raw-fastas`
# and FASTA files filtered to common specimens in
# `data-raw/sequencing/2.subset-fastas` were aligned using MAFFT.
# The G-INS-i alignment strategy with iterative refinement and 1PAM / k=2
# nucleotide scoring matrix were set sat as the alignment parameters.

# Print aligned FASTA file, number of sequences, and range of alignments
fasta_summaries <- function(fasta_path) {

  # Read FASTA files from path.
  fasta_list <-
    list.files(fasta_path, pattern = "\\.fasta$", full.names = TRUE) %>%
    purrr::map(., Biostrings::readDNAStringSet)

  # Extract locus headers from DNAStringSet.
  fasta_loci <- purrr::map(fasta_list, names) %>%
    purrr::map(., function(locus_headers) {
      purrr::map_chr(locus_headers, function(header) {
        stringr::str_extract(string = header, pattern = "(?<= ).+$")
        }) %>% unique()
      }) %>% unlist()

  # Return summary tibble from DNAStringSet object.
  tibble::tibble(Locus = unlist(fasta_loci),
                 Sequences = purrr::map_int(fasta_list, length),
                 `Min Length` = purrr::map_int(fasta_list,
                    function(fastas) min(Biostrings::width(fastas))),
                 `Max Length` = purrr::map_int(fasta_list,
                    function(fastas) max(Biostrings::width(fastas))))
}

# Print summary tibbles for each of raw, subset, aligned, and concatenated.
fasta_summaries("data-raw/sequencing/1.raw-fastas/")
fasta_summaries("data-raw/sequencing/2.subset-fastas/")
fasta_summaries("data-raw/sequencing/3.alignments-single/")
fasta_summaries("data-raw/sequencing/3.alignments-concatenated/")

