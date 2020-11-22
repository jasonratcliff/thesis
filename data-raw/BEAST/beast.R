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

# *BEAST ----

# Exclude outgroup taxa from FASTA alignment and write NEXUS output.
list.files("data-raw/sequencing/3.alignments-single",
           full.names = TRUE) %>%

  purrr::walk(.x = ., function(fasta_file) {
    locus_fasta <- Biostrings::readDNAStringSet(filepath = fasta_file)
    names(locus_fasta) <- gsub(" .+$", "", names(locus_fasta))
    locus_fasta <-
      locus_fasta[!grepl("^(LFEND|LARGY|POBCO)", x = names(locus_fasta))]

    tree_file <-
      fs::path(
        "data-raw/BEAST/NEXUS/",
        paste0(
          gsub("-single", "", fs::path_ext_remove(basename(fasta_file))),
          "-species"
        ),
        ext = "nex"
      )

    char_wrap <- lapply(locus_fasta, length) %>% unique() %>% unlist()
    ape::write.nexus.data(
      x = locus_fasta,
      file = tree_file,
      charsperline = char_wrap,
      interleaved = FALSE
    )
  })

# Write subset of specimen IDs for species tree taxa labels.
spp_tree_taxa <- ThesisPackage::dna_specimens %>%
  dplyr::select(1:11, dplyr::matches("header_")) %>%
  dplyr::filter(
    !grepl("^L(FEND|ARGY|POBCO)", x = .data$label)
  ) %>%
  # Replace unsampled / frameshift taxa headers as missing.
  dplyr::mutate_at(
    .vars = dplyr::vars(dplyr::matches("header_")),
    .funs = ~ gsub("MISSING|FRAMESHIFT", NA, x = .x)
  ) %>%
  dplyr::select(
    "label", "ID_final", # "Latitude.x", "Longitude.x", "multiLocus"
   ) %>%
  dplyr::mutate(
    ID_final = gsub(
      x = .data$ID_final,
      pattern = " ",
      replacement = "_"
      ) %>%
      gsub(pattern = "\\.", replacement = "", x = .)
  ) %>%
  dplyr::rename(
    "traits" = "label",
    "species" = "ID_final"
  )
