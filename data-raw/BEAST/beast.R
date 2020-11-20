library(magrittr)
fs::dir_create("data-raw/BEAST/NEXUS/")

# Walk concatenated aligned subsets to write nexus files
list.files("data-raw/sequencing/3.alignments-subset",
           full.names = TRUE) %>%
  purrr::walk(.x = ., function(fasta_file) {

    # Extract locus substring from filepath.
    locus <- fs::path_file(path = fasta_file) %>%
      stringr::str_remove(pattern = "\\-ml\\.fasta")

    # Read in FASTA file and remove locus from header sample IDs.
    locus_fasta <- Biostrings::readDNAStringSet(filepath = fasta_file)
    names(locus_fasta) <- gsub(" .+$", "", names(locus_fasta))

    # Exclude outgroup taxa.
    locus_fasta <-
      locus_fasta[!grepl("^L(FEND|ARGY)", x = names(locus_fasta))]

    # Calculate sequence width and write nexus file
    char_wrap <- lapply(locus_fasta, length) %>% unique() %>% unlist()
    ape::write.nexus.data(x = locus_fasta, interleaved = FALSE,
                          charsperline = char_wrap,
                          file = paste0("data-raw/BEAST/NEXUS/",
                                        locus, ".nex"))
})

# Write subset of specimen IDs from multi-locus sample set.
ThesisPackage::dna_specimens %>%
  dplyr::select("label", "ID_final", # "Latitude.x", "Longitude.x",
                "multiLocus") %>% dplyr::filter(multiLocus == TRUE) %>%
  dplyr::select(-c("multiLocus")) %>%
  dplyr::rename("traits" = "label", "species" = "ID_final") %>%
readr::write_delim(x = ., path = "data-raw/BEAST/final_ids.txt", delim = "\t")
