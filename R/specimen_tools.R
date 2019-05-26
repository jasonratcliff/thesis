# Specimen Access Tools ----

#' Find specimen(s) from herbarium data frame.
#' 
#' Function to return a subset of specimen records from the `total_physaria` 
#' data frame given input parameters for record identification.  Search by
#' row index, collector and collection number with the option to include
#' additional specimen data for prior identifications, collection locality,
#' and geographic coordinates.
#' 
#' @param taxa_frame Data frame from which to subset specimen(s) of interest.
#' @param collector Character vector of length one matching specimen collector 
#' in column `Collector` of `total_physaria` data frame.
#' @param collection_number Numeric vector of length one with specimen
#' number in column `Collection_Number` of `total_physaria` data frame.
#' @param row_id Numeric vector of length one matching row index to subset.
#' @param priors Logical vector of length one. Option to include prior 
#'   identification columns when TRUE. Default = FALSE.
#' @param locale Logical vector of length one. Option to include herbarium,
#'   state, and county of collection(s) when TRUE. Default = FALSE.
#' @param geoms Logical vector of length one. Option to include geographic 
#'   coordinate data when true. Default = FALSE.
#'
#' @examples
#' # Subset of samples collected by O'Kane
#' okane_collections <- find_spp(taxa_frame = total_physaria,
#'                               collector = "O'Kane")
#' # Find specimen collection by Rollins
#' find_spp(total_physaria, collector = "Rollins", collection_number = 7940)
#' #      Taxon_a_posteriori     Collector Collection_Number
#' # 462 Physaria acutifolia R. C. Rollins              7940
#' 
find_spp <- function(taxa_frame, 
                     collector = NULL, collection_number = NULL, row_id = NULL, 
                     priors = FALSE, locale = FALSE, geom = FALSE) {
  
  # Establish column index to subset specimen information from taxa data frame.
  col_core <- c("Taxon_a_posteriori", "Collector", "Collection_Number")
  
  # Parse the function system call for subsetting user-specified columns.
  user_args <- as.list(sys.call())
  col_user <- user_args[grep("priors|locale|geom", names(user_args))]
  invisible(mapply(col_user, names(col_user),
                   FUN = function(arg, arg_name) {
                     if (!(arg == TRUE || arg == FALSE)) {
                       stop(paste0("Argument '", arg_name, " = ", arg, 
                                   "' must be logical TRUE or FALSE."))
                     }
  }))
  
  # Check user-specified columns against default columns and combine lists.
  col_formals <- formals()[grep("priors|locale|geom", names(formals()))]
  col_match <- match(names(col_user), names(col_formals))
  col_defaults <- col_formals[which(!(seq_along(col_formals) %in%
                                            col_match))]
  col_extra <- c(col_user, col_defaults)

  # Subset list of column names based on user input and default arguments.
  col_list <- list(priors = names(taxa_frame)[grep(pattern = "Physaria",
                                                    names(taxa_frame))],
                     geom = c("Latitude", "Longitude"),
                     locale = c("Herbarium", "State", "County"))
  col_include <- col_extra[which(col_extra == TRUE)]
  col_names <- unlist(col_list[names(col_include)], use.names = FALSE)
  col_total <- c(col_core, col_names)
  
  # Return data frame by row ID index.
  if (!is.null(row_id) && 
      (!is.null(collector) || !is.null(collection_number))) {
    stop("If entering row ID number, exclude collector and collection number.")
  } else {
    return_df <- taxa_frame[row_id, col_total]
  }

  # Return data frame by rows matching collector and collection number.
  if (!is.null(collector) || !is.null(collection_number) &&
      is.null(row_id)) {  # Intersection of collector and collection number.
    if (!is.null(collector) && !is.null(collection_number)) {
      spp_collection <- 
        intersect(grep(collector, taxa_frame$Collector),
                  grep(collection_number, taxa_frame$Collection_Number))
      return_df <- taxa_frame[spp_collection, col_total]
    } else {  # Return matched collector or collection number records.
      collection_args <- user_args[grep("collector|collection_number", 
                                        names(user_args))]
      collection_cols <- list(collector = "Collector",
                              collection_number = "Collection_Number")
      collection_col <- collection_cols[[names(collection_args)]]
      return_df <- 
        taxa_frame[grep(collection_args[[1]], 
                        taxa_frame[, collection_col]), col_total]
    }
  }
  return(return_df)
}

#' Subset herbarium data frame.
#' 
#' @inheritParams find_spp
#' 
subset_spp <- function(taxa_frame, taxa_col, exclude = c(FALSE, TRUE),
                       state = NULL, county = NULL, spp_str = NULL,
                       longitude = NULL, latitude = NULL, set_name = NULL) {
  
  user_args <- as.list(sys.call())  # list of user arguments
  
  # Subset specimen records by state and county user arguments.
  if (!is.null(state) || !is.null(county)) {
    
    # Parse border inputs to combine vectors into regular expression.
    border_cols <- list(state = "State", county = "County")
    border_args <- user_args[grep("state|county", names(user_args))] %>%
      lapply(function(border) paste(border, collapse="|") %>%
               gsub("^c\\|", "", x = .))

    # Determine row index from intersection of records matching state or county.
    index_borders <- 
      mapply(border_args, names(border_args), USE.NAMES = FALSE, 
             SIMPLIFY = FALSE, FUN = function(borders, type) {
               index_rows <- grep(borders, taxa_frame[, border_cols[[type]]])
               }) %>% Reduce(intersect, x = .)
  }
}

