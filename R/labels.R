#' Parse Taxa Labels
#'
#' Pair with [haplotype_labels()] and [species_plot()] to create plotmath
#' expressions parsed as [ggplot2::ggplot()] labels.
#'
#' @param tree_tibble
#' @param id_column
#' @family labels
#' @export
#'
#' @examples
#' system.file("extdata/BEAST/spp-hypothesis-1.mcc", package = "Thesis") %>%
#'   treeio::read.beast(file = .) %>%
#'   ggtree::fortify() %>%
#'   dplyr::mutate(
#'     label = gsub("medicinae", "'medicinae'", x = .data$label)
#'   ) %>%
#'   dplyr::mutate(
#'     label = parse_taxa(tree_tibble = ., id_column = "label")
#'   ) %>%
#'   dplyr::select(label)
#'
parse_taxa <- function(tree_tibble, id_column) {
  # TODO Improve OOP understanding and implementations.. 
  # Here, generalize to either tbl or tbl_tree
  # stopifnot(identical( 
  #   class(tree_data), 
  #   c("tbl_tree", "tbl_tree", "tbl_df", "tbl", "data.frame")
  # ))
  dplyr::select(tree_tibble, .data[[id_column]]) %>%
    dplyr::mutate(
      !!id_column := stringr::str_replace_all(
        string = .data[[id_column]],
        pattern = "( |_)+",
        replacement = " "
      ),
      genus = stringr::str_extract(
        string = .data[[id_column]],
        pattern = "^[A-Z][a-z]+(?= |$?)"
      ),
      epithet = stringr::str_extract(
        string = .data[[id_column]],
        pattern = paste0(
          "(?<=^", .data$genus, " )",
          "'?[a-z]+'?(?=( (subsp|ssp)\\.?|$))"
        )
      ),
      subspecies = stringr::str_extract(
        string = .data[[id_column]],
        pattern = "(?<=(subsp|ssp)\\.? )[a-z]+$"
      )
    ) %>%
    purrr::pmap_dfr(.l = ., .f = function(genus, epithet, subspecies, ...) {
      genus <- ifelse(
        test = !is.na(epithet),
        yes = abbreviate_taxa(taxa_rank = genus),
        no = genus
      )
      epithet <- ifelse(
        test = !is.na(subspecies),
        yes = abbreviate_taxa(taxa_rank = epithet),
        no = epithet
      )
      dplyr::bind_cols(
        label_genus = genus,
        label_epithet = epithet,
        label_subspecies = subspecies, ...
      )
    }) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with("label_"),
        .fns = ~ replace(x = .x, is.na(.), "")
      ),
      label = purrr::pmap_chr(
        .l = list(.data$label_genus, .data$label_epithet,
                  .data$label_subspecies, .data[[id_column]]),
        .f = function(genus, epithet, subspecies, id_column) {
          if (!is.na(id_column)) {
            paste0(
              "italic(", genus,
              ifelse(
                test = nchar(epithet) > 0 &
                  !grepl(pattern = "^'[a-z]+'$", x = epithet),
                yes = paste0(" ", epithet, ")"),
                no = ifelse(
                  test = grepl(pattern = "^'[a-z]+'$", x = epithet),
                  yes = paste0(") \"", epithet, "\""),
                  no = paste0(")")
                )
              ),
              ifelse(
                test = nchar(subspecies) > 0,
                yes = paste0(" subsp. italic(", subspecies, ")"),
                no = ""
              )
            ) %>%
              gsub(pattern = " +", replacement = "~", x = .)
          } else {
            NA_character_
          }
        })
    ) %>%
    dplyr::pull(label)
}

abbreviate_taxa <- function(taxa_rank) {
  stringr::str_split(string = taxa_rank, pattern = "")[[1]][1] %>%
    paste0(., ".")
}

