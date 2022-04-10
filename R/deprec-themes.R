# Labels ----

#' Generate HTML vector for
#' [element_markdown()][ggtext::element_markdown]
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Requires installation of [ggtext] as follows:
#'   remotes::install_github("clauswilke/ggtext")
#'
#' @inheritParams layer_specimens
#' @export
#'
#' @return Character vector of html markup named by specimen identification.
#'
#' @examples
#' spl_labels(
#'   specimen_tbl = spp_co_front_range,
#'   id_column = "prior_id"
#' )
#'
spl_labels <- function(specimen_tbl, id_column) {
  label_markdown <- function(label_vector) {
    purrr::map_chr(.x = label_vector, .f = function(label) {
      split_label <- unlist(strsplit(label, " "))
      if (length(split_label) %in% c(1, 2)) {
        parsed_label <- ifelse(
          test = grepl("'medicinae'", x = label),
          yes = "*Physaria* 'medicinae'",
          no = paste0("*", label, "*")
        )
      } else {
        if (grepl("subsp\\.", x = label)) {
          # Add html formatting to split subsp. onto second line.
          parsed_label <-
            paste0("*", paste0(split_label[1:2], collapse = " "),
              "*",
              collapse = ""
            ) %>%
            paste0(
              ., gsub(
                pattern = "s(ub)?sp\\.|var\\.", x = split_label[3],
                replacement = "<br><span>&nbsp;&nbsp;&nbsp;</span>  subsp\\. *"
              ),
              split_label[4:length(split_label)], "*"
            )
        } else { # Any other cases
          parsed_label <- paste0("*", label, "*")
        }
      }
      return(parsed_label)
    })
  }

  # Add italics, html line break and non-breaking space characters.
  labels_vector <- specimen_tbl %>%
    dplyr::select(., !!id_column) %>%
    dplyr::pull() %>%
    unique()
  labels_html <- label_markdown(labels_vector)
  names(labels_html) <- labels_vector

  # Return named vector of final identifications and markdown expression.
  return(labels_html)
}

#' Parse Taxa Labels
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Pair with [haplotype_labels()] and [species_plot()] to create plotmath
#' expressions parsed as [ggplot2::ggplot()] labels.
#'
#' Plotmath Reference
#' - https://rdrr.io/r/grDevices/plotmath.html
#'
#' @param tree_tibble `tbl_tree` object with label vector `id_column`.
#' @param id_column Character vector for indirect tidy evaluation.
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
        .l = list(
          .data$label_genus, .data$label_epithet,
          .data$label_subspecies, .data[[id_column]]
        ),
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
        }
      )
    ) %>%
    dplyr::pull("label")
}

# Replace with base::abbreviate(..., dot = TRUE, minlength = 1)
abbreviate_taxa <- function(taxa_rank) {
  stringr::str_split(string = taxa_rank, pattern = "")[[1]][1] %>%
    paste0(., ".")
}
