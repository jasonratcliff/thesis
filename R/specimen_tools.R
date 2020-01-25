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
#' @return Data frame `return_df` matching specimen records.
#'
#' @examples
#' # Subset of samples collected by O'Kane
#' okane_collections <- spp_find(taxa_frame = total_physaria,
#'                               collector = "O'Kane")
#' # Find specimen collection by Rollins
#' spp_find(total_physaria, collector = "Rollins", collection_number = 7940)
#' #      Taxon_a_posteriori     Collector Collection_Number
#' # 462 Physaria acutifolia R. C. Rollins              7940
#'
spp_find <- function(taxa_frame, collector = NULL, collection_number = NULL,
                     row_id = NULL, priors = FALSE, locale = FALSE,
                     geom = FALSE, label = FALSE) {

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

  # Add label aesthetic variable for ggplot mapping of individual specimen.
  if (label ==TRUE) {
    return_df <-
      dplyr::mutate(return_df,
                    taxon_label = paste(return_df$Collector,
                                        return_df$Collection_Number,
                                        sep = "\n") %>%
                      # Clean up collector(s') name
                      map_if(., function(full_name) {
                        grepl("[A-Z]\\. |with", full_name)
                      }, function(name) {
                        gsub("[A-Z]\\. ", "", name) %>%
                          gsub("with", "&", .)
                      }) %>% unlist()
                    )
  }
  return(return_df)
}

#' Subset herbarium data frame.
#'
#' Function to subset herbarium specimen data frame by state, county, geographic
#' coordinates, and species identification regular expression.  Subset is
#' determined by the intersection of rows matching input arguments.
#'
#' @param state Character vector of state(s) to subset $State column.
#' @param county Character vector of county(ies) to subset $County column.
#' @param longitude Numeric vector of length two to subset $Longitude column.
#' @param latitude Numeric vector of length two to subset $Latitude column.
#' @param spp_str Character vector of length one with species regular expression
#' by which to subset specimen IDs in `grep` function call.
#' @param taxa_col Character vector of length one with `grep` column name.
#' @param exclude Logical vector of length one to invert `grep` call.
#' @param set_name Character vector of length one to name output .csv file.'
#' @inheritParams spp_find
#'
#' @return Data frame `taxa_subset` containing records subset by input criteria.
#'
#' @examples
#' didy_subset <- spp_subset(taxa_frame = total_physaria,
#'                           state = "Wyoming", county = "Hot Springs",
#'                           longitude = c(-109, -108), latitude = c(43, 44),
#'                           spp_str = "didymocarpa", taxa_col = "Physaria_syn",
#'                           set_name = "hot-springs-didymocarpa")
#'
#' unique(didy_subset$Physaria_syn)
#' #  [1] "Physaria didymocarpa ssp. didymocarpa"
#'
spp_subset <- function(taxa_frame, state = NULL, county = NULL,
                       longitude = NULL, latitude = NULL,
                       spp_str = NULL, taxa_col = NULL,
                       exclude = c(FALSE, TRUE), set_name = NULL) {

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

  # Subset specimen records by geographic coordinates.
  if (!is.null(longitude) || !is.null(latitude)) {

    # Check coordinates for vector length and geographic position.
    coord_cols <- list(longitude = "Longitude", latitude = "Latitude")
    coord_args <- user_args[grep("longitude|latitude", names(user_args))]
    invisible(lapply(names(coord_args), function(coord_name) {
      coord_value <- get(coord_name)
      if (!is.numeric(coord_value) || length(coord_value) != 2) {
        stop(print(c(coord_name, get(coord_name))),
             "Enter a numeric vector of length two for geographic coordinates.")
        }
      if (coord_name == "longitude" && TRUE %in% (coord_value > 0) ||
          coord_name == "latitude" && TRUE %in% (coord_value < 0)) {
        stop(print(c(coord_name, coord_value)),
             "Enter coordinates for the western hemisphere.")
        }
    }))

    # Sort coordinate vectors and index by geographic coordinates.
    index_coords <-
      lapply(names(coord_args), function(coord_name) {
      coord_value <- sort(get(coord_name))
      coord_col <- coord_cols[[coord_name]]
      intersect(which(taxa_frame[, coord_col] > min(coord_value)),
                which(taxa_frame[, coord_col] < max(coord_value)))
    }) %>% Reduce(intersect, x = .)
  }

  # Subset specimen records by species string.
  if (!is.null(spp_str)) {
    if (is.null(taxa_col)) {
      stop("Set `taxa_col` argument for column subset.")
    } else {
      index_spp <- grep(paste(spp_str, collapse = "|"),
                        taxa_frame[, taxa_col], invert = exclude)
    }
  }

  # Reduce index vectors to intersecting set for return subset.
  vector_names <- ls(pattern = "^index")
  if (length(vector_names > 0)) {
    vector_index <- mget(vector_names, inherits = TRUE) %>%
      Reduce(intersect, x = .)
  }
  taxa_subset <- taxa_frame[vector_index, ]

  # Option to write .csv file from taxa subset.
  if (!is.null(set_name)) {
    dir.create("output/subsets", showWarnings = FALSE)
    write.csv(taxa_subset,
              file = paste0("output/subsets/",
                            gsub("\\.$", "", set_name), ".csv"))
  }
  return(taxa_subset)
}


#' Subset specimen data by geographic coordinates.
#' 
#' Given an input data.frame and at least one of latitude or longitude
#' vectors with minimum and maximum values to subset coordinate data.
#' 
#' @param specimen_df Data.frame with columns Latitude / Longitude.
#'   Should be numeric vectors of decimal degree coordinates.
#' @param Latitude Character vector length two min / max latitude.
#' @param Longitude Character vector length two min / max longitude.
#' @return Data.frame of specimens within coordinate bounds.
#' @examples
#' \dontrun{
#' specimen_subset <- subset_coords(specimen_df = total_physaria,
#'                      Latitude = c(41, 45), Longitude = c(-109, -105))
#' range(specimen_subset$Latitude)
#' range(specimen_subset$Longitude)
#' }
subset_coords <- function(specimen_df, Latitude = NULL, Longitude = NULL) {
  
  # Test for at list one of latitude / longitude vectors.
  if (is.null(Latitude) & is.null(Longitude)) {
    stop("Enter a vector for subsetting coordinates.")
  }
  
  # Assign list of coordinates and test that values are ranges.
  filter_coordinates <-
    list("Longitude" = Longitude, "Latitude" = Latitude) %>%
    lapply(X = ., function(coordinate) {
      if (!is.null(coordinate) & length(coordinate) != 2) {
        stop("Enter a coordinate range of two values.")
      } else {
        return(coordinate)
      }
    })
  
  # Map coordinate tibble ranges with minimum / maximum coordinate values.
  coordinate_tbl <-
    filter_coordinates[which(lapply(filter_coordinates, is.null) == FALSE)] %>%
    purrr::map2_dfr(.x = ., .y = names(.),
      .f =  function(coordinate, parallel_name) {
        if (identical(!unique(coordinate > 0), TRUE) ||
            identical(!unique(coordinate < 0), TRUE)) {
          coordinate_ranges <- range(coordinate)
          } else {
            warning("Coordinates not in the same hemisphere...")
            }
        dplyr::bind_rows(
          dplyr::bind_cols(parallel = parallel_name, edge = "minimum", 
                           coordinate = min(coordinate_ranges)),
          dplyr::bind_cols(parallel = parallel_name, edge = "maximum",
                           coordinate = max(coordinate_ranges))
          )
        })
  
  # Construct dplyr filter statements from mapped coordinate tibble.
  filter_statements <-
    purrr::pmap_dfr(coordinate_tbl, function(parallel, edge, coordinate) {
      operand <- ifelse(edge == "minimum", ">", "<")
      filter_statement <- paste(parallel, operand, coordinate, collapse = " ")
      bind_cols(construct = filter_statement)
    }) %>% purrr::pluck("construct") %>% paste0("filter(", .) %>% paste0(., ")")
  
  # Construct data filter pipeline by coordinate ranges and return evaluation.
  filter_expr <- paste("specimen_df %>%",
                       paste(filter_statements[1:length(filter_statements) - 1],
                             collapse = " %>% "), "%>%",
                       filter_statements[length(filter_statements)])
  eval(rlang::parse_expr(filter_expr))
}

