library(magrittr)
library(fs)

# Set path from home directory to digiKam photo subdirectory.
# Script assumes digiKam database is located in users' home directory.
digikam_path <- c("jPhoto/Physaria")
if (Sys.getenv(x = "USER") != "jasonratcliff") {
  stop("Set `digikam_path` for local execution by user: ",
       Sys.getenv(x = "USER"),
       "\n  See path variable in: ",
       rstudioapi::getActiveDocumentContext()[["path"]])
}

specimens_xlsx <- # Specimens external data file path.
  system.file("extdata/specimens.xlsx", package = "ThesisPackage")

if (!file_exists(specimens_xlsx)) stop("Verify `specimens_xlsx` path variable.")

#' Search digiKam Directory
#'
#' Identify relative paths to specimen voucher photos in digiKam database.
#' 
#' @param sheetname Name of *.xlsx* file sheet name
#' @param specimens Data frame of specimen vouchers
#'
#' @return Tibble data frame of specimen information. Includes collector /
#'  collection number with inferred surname search pattern, state / county
#'  locality information and relative path to specimen photo.
#'
#' @examples
#' digi_search(
#'   sheetname = "P. Remaining",
#'   specimens = ThesisPackage::herbarium_specimens,
#'   digi_path = digikam_path
#' )
#'
digi_search <- function(sheetname, specimens, digi_path) {

  # Extract surnames and build patterns for file search.
  collection_data <- specimens %>% dplyr::filter(excel_sheet == sheetname) %>%
    dplyr::select(Collector, Collection_Number,
                  State, County) %>%
    dplyr::mutate(
      # Extract first matched surname for approximate pattern matching.
      surname = stringr::str_extract(string = Collector,
                                     pattern = "[A-Z][a-z]+"),
      search_pattern = ifelse(!is.na(Collection_Number),
                              paste0(surname, "_", Collection_Number),
                              paste0(surname, "_", "sn"))
      )

  filepath_index <-
    list.files(path = path_home(digi_path),
               recursive = TRUE,  full.names = TRUE)
  
  # Recursive search of digKam photo subdirectory
  search_data <- collection_data$search_pattern %>%
    purrr::keep(~ !is.na(.x)) %>%
    purrr::map_dfr(function(search_pattern) {
      search_results <-
        grep(pattern = search_pattern, x = filepath_index, value = TRUE) %>%
        gsub(pattern = path_dir(path_home(digi_path)),
             replacement = "", x = .)
      pattern <- ifelse(length(search_results) == 0, search_pattern,
                        rep(search_pattern, times = length(search_results)))
      results <- ifelse(length(search_results) == 0, "", search_results)
      tibble::tibble(pattern = pattern, path = results)
      })

  # Join search results back to collection metadata by search pattern
  joined_data <-
    dplyr::left_join(collection_data, search_data,
                     by = c("search_pattern" = "pattern"))

  return(joined_data)
}

# Map sheet names to search digikam photo directories for joined search patterns
readxl::excel_sheets(path = specimens_xlsx) %>%
purrr::map_dfr(., function(sheet) {
  digi_paths <-
    digi_search(sheetname = sheet,
                specimens = ThesisPackage::herbarium_specimens,
                digi_path = digikam_path)
  dplyr::bind_cols(excel_sheet = rep(sheet, times = nrow(digi_paths)),
                   digi_paths)
}) %>%
  # setNames(object = ., nm = sheets) %>%
  writexl::write_xlsx(x = ., path = "Appendix/digikam.xlsx")

