library(magrittr)

fs::dir_create("data-raw/BEAST/NEXUS/")

# Standard BEAST ----

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
    !grepl("^L(FEND|ARGY)|POBCO", x = .data$label)
  ) %>%
  # Replace unsampled / frameshift taxa headers as missing.
  dplyr::mutate_at(
    .vars = dplyr::vars(dplyr::matches("header_")),
    .funs = ~ gsub("MISSING|FRAMESHIFT", NA, x = .x)
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

# Phylogeography ----

# Subset specimens to complete rITS and rps loci.
spp_phylogeography <- spp_tree_taxa %>%
  dplyr::select("traits", "species",
                dplyr::matches("header_(rps|rITS)"),
                "Longitude.x", "Latitude.x") %>%
  dplyr::rename(
    "lat" = "Latitude.x",
    "long" = "Longitude.x"
  ) %>%
  # Replace unsampled / frameshift taxa headers as missing.
  dplyr::mutate_at(
    .vars = dplyr::vars(dplyr::matches("header_")),
    .funs = ~ gsub("MISSING|FRAMESHIFT", NA, x = .x)
  ) %>%
  dplyr::filter(!is.na(header_rps) & !is.na(header_rITS))

# Write specimens with geographic coordinates to import traits in BEAST v1.10.4.
spp_phylogeography %>%
  dplyr::select(-c("species", dplyr::matches("header"))) %>%
  readr::write_delim(
    x = .,
    file = "data-raw/BEAST/Phylogeography/complete-phylogeography.txt",
    delim = "\t"
  )

# Write rITS / rps FASTA subsets for continuous phylogeography.
list.files(
  path = "data-raw/sequencing/3.alignments-single",
  full.names = TRUE,
  pattern = "rps|rITS"
) %>%
  purrr::walk(.x = ., function(fasta_file) {

    locus_fasta <- Biostrings::readDNAStringSet(filepath = fasta_file)
    names(locus_fasta) <- gsub(" .+$", "", names(locus_fasta))
    locus_fasta <-
      locus_fasta[!grepl("^(LFEND|LARGY|POBCO)", x = names(locus_fasta))]
    locus_fasta <-
      locus_fasta[names(locus_fasta) %in% spp_phylogeography$traits]

    tree_file <-
      fs::path(
        "data-raw/BEAST/NEXUS/",
        paste0(
          gsub("-single", "", fs::path_ext_remove(basename(fasta_file))),
          "-geography"
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

# Species Tree Hypotheses ----

spp_hypotheses <- spp_tree_taxa %>%
  dplyr::select("traits", "species")

# Hypothesis 1:
# - Split P. didymocarpa subsp.
#   - P. d. subsp. didymocarpa
#   - P. d. subsp. lanata
#   - P. d. subsp. lyrata
# - Lump P. saximontana subsp.
spp_hypotheses %>%
readr::write_delim(
    x = .,
    file = "data-raw/BEAST/Species-Tree/species-hypothesis-1.txt",
    delim = "\t"
  )

# Hypothesis 2:
# - Lump P. d. subsp. didymocarpa to include:
#   - P. d. subsp. lyrata
#   - P. saximontana
# - Elevate P. d. subsp. lanata to specific rank
spp_hypotheses %>%
  dplyr::mutate(
    species = gsub(
      pattern = "ssp_lyrata",
      replacement = "ssp_didymocarpa",
      x = .data$species),
    species = gsub(
      pattern = "Physaria_saximontana",
      replacement = "Physaria_didymocarpa_ssp_didymocarpa",
      x = .data$species),
    species = gsub(
      pattern = "_didymocarpa_ssp",
      replacement = "",
      x = .data$species)
  ) %>%
  readr::write_delim(
    x = .,
    file = "data-raw/BEAST/Species-Tree/species-hypothesis-2.txt",
    delim = "\t"
  )

# Hypothesis 3:
# - Lump P. d. subsp. didymocarpa to include P. saximontana
# - Lump P. d. subsp. lyrata with P. integrifolia
# - Lump P. d. subsp. lanata with P. acutifolia
spp_hypotheses %>%
  dplyr::mutate(
    species = gsub(
      pattern = "Physaria_didymocarpa_ssp_lyrata",
      replacement = "Physaria_integrifolia",
      x = .data$species),
    species = gsub(
      pattern = "Physaria_didymocarpa_ssp_lanata",
      replacement = "Physaria_acutifolia",
      x = .data$species),
    species = gsub(
      pattern = "Physaria_saximontana",
      replacement = "Physaria_didymocarpa",
      x = .data$species)
  ) %>%
  readr::write_delim(
    x = .,
    file = "data-raw/BEAST/Species-Tree/species-hypothesis-3.txt",
    delim = "\t"
  )
