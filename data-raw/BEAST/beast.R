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

# Write subset of specimen IDs for species tree taxa labels.
spp_combined <- Thesis::dna_specimens %>%
  dplyr::select(1:11, dplyr::matches("header_")) %>%

  # Exclude outgroup and duplicated sample localities.
  dplyr::filter(
    !grepl(
      pattern = paste(
        "^L(FEND|ARGY)|POBCO",
        "PDIDY_DI_46|PDIDY_LY_2689|PDIDY_LA_3138|PEBUR_3121",
        sep = "|"
      ),
      x = .data$label)
  ) %>%

  # Replace unsampled / frameshift taxa headers as missing.
  dplyr::mutate_at(
    .vars = dplyr::vars(dplyr::matches("header_")),
    .funs = ~ gsub("MISSING|FRAMESHIFT", NA, x = .x)
  ) %>%
  dplyr::filter(!is.na(header_rps) & !is.na(header_rITS)) %>%

  # Remove spaces from specimen identifications.
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
    "species" = "ID_final",
    "lat" = "Latitude.x",
    "long" = "Longitude.x"
  )

list.files(
  path = "data-raw/sequencing/3.alignments-single",
  full.names = TRUE,
  pattern = "rITS|rps"
) %>%
  purrr::walk(.x = ., function(fasta_file) {
    locus_fasta <- Biostrings::readDNAStringSet(filepath = fasta_file)
    names(locus_fasta) <- gsub(" .+$", "", names(locus_fasta))
    locus_fasta <-
      locus_fasta[which(names(locus_fasta) %in% spp_combined$traits)]
    tree_file <-
      fs::path(
        "data-raw/BEAST/NEXUS/",
        paste0(
          gsub("-single", "", fs::path_ext_remove(basename(fasta_file))),
          "-combined"
        ),
        ext = "nex"
      )
    char_wrap <- unique(vapply(locus_fasta, length, integer(1)))
    ape::write.nexus.data(
      x = locus_fasta,
      file = tree_file,
      charsperline = char_wrap,
      interleaved = FALSE
    )
  })

# Phylogeography ----

# Write specimens with geographic coordinates to import traits in BEAST v1.10.4.
spp_combined %>%
  dplyr::select(traits, lat, long) %>%
  readr::write_delim(
    x = .,
    file = "data-raw/BEAST/Phylogeography/phylogeography.txt",
    delim = "\t"
  )

# Species Tree Hypotheses ----

spp_hypothesis1 <- spp_combined %>%
  dplyr::select("traits", "species")

# Hypothesis 1:
# - Split P. didymocarpa subsp.
#   - P. d. subsp. didymocarpa
#   - P. d. subsp. lanata
#   - P. d. subsp. lyrata
# - Split P. saximontana subsp.
#   - P. s. subsp. saximontana
#   - P. s. subsp. dentata
readr::write_delim(
    x = spp_hypothesis1,
    file = "data-raw/BEAST/Species-Tree/species-hypothesis-1.txt",
    delim = "\t"
  )

# Hypothesis 2:
# - Lumped P. saximontana subsp.
spp_hypothesis2 <- spp_hypothesis1 %>%
  dplyr::mutate(
    species = stringr::str_replace(
      string = .data$species,
      pattern = "Physaria_saximontana.+",
      replacement = "Physaria_saximontana"
    )
  )
readr::write_delim(
  x = spp_hypothesis2,
  file = "data-raw/BEAST/Species-Tree/species-hypothesis-2.txt",
  delim = "\t"
)

# Hypothesis 3:
# - Lumped P. didymocarpa and P. saximontana
spp_hypothesis3 <- spp_hypothesis2 %>%
  dplyr::mutate(
    species = stringr::str_replace(
      string = .data$species,
      pattern = "Physaria_saximontana",
      replacement = "Physaria_didymocarpa_ssp_didymocarpa"
    )
  )
readr::write_delim(
  x = spp_hypothesis3,
  file = "data-raw/BEAST/Species-Tree/species-hypothesis-3.txt",
  delim = "\t"
)

# Hypothesis 4:
# - Lumped P. didymocarpa subsp. lyrata
spp_hypothesis4 <- spp_hypothesis3 %>%
  dplyr::mutate(
    species = stringr::str_replace(
      string = .data$species,
      pattern = "ssp_lyrata",
      replacement = "ssp_didymocarpa"
    )
  )
readr::write_delim(
  x = spp_hypothesis4,
  file = "data-raw/BEAST/Species-Tree/species-hypothesis-4.txt",
  delim = "\t"
)

# Write specimen hypothesis identifications to file.
tibble::tibble(
  spp_combined$traits,
  spp_hypothesis1$species,
  spp_hypothesis2$species,
  spp_hypothesis3$species,
  spp_hypothesis4$species
) %>%
  dplyr::rename_at(
    .vars = dplyr::vars(dplyr::everything()),
    ~ gsub(pattern = "\\$.+$", replacement = "", x = .x)
  ) %>%
  dplyr::mutate_at(
    .tbl = .,
    .vars = dplyr::vars(dplyr::matches("spp_hypothesis")),
    ~ gsub("_", " ", x = .x) %>% gsub("Physaria", "P.", x = .)
  ) %>%
  readr::write_csv(x = .,
    file = "inst/extdata/BEAST/spp_hypotheses.csv"
  )

