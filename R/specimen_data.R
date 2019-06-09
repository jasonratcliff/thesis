# 1. Read in specimen data ----

#' Read herbarium specimen data into R from an .xlsx file
#'
#' An Excel file located in the `data/` subdirectory contains specimen voucher
#' information for project herbarium records.  These data are used for
#' analysis of distribution, morphology, ecology, and phylogenetics.  Records
#' contain geographic coordinate data in degree decimal format, collection
#' dates, annotation information, trait measurements and observations.
#'
#' @param herbarium_file Path to .xlsx file with herbarium specimen records.
#' @return Large list of data frames of species tabs.
specimens_read <- function(herbarium_file) {

  # Vector of .xlsx spreadsheet tabs to read.
  species_tabs <- c("P. Remaining", "P. acutifolia", "P. saximontana",
                    "P. eburniflora", "P. didymocarpa", "P. chambersii",
                    "P. vitulifera_CO", "P. vitulifera_WY",
                    "P. integrifolia", "P. Idaho", "P. others",
                    "P. DNA", "P. NC_WY", "P. brassicoides")

  # Assign list of data frames read from excel tabs and filter by annotation.
  specimen_list <- lapply(species_tabs, function(xlsx_tab) {
    read.xlsx(xlsxFile = herbarium_file, sheet = xlsx_tab,
              cols = c(1:4, 5:8, 10:14, 15:17, 19:21, 28:65),
              colNames = TRUE, rowNames = FALSE, detectDates = TRUE) %>%
      filter(grepl("Physaria|Lesquerella|Brassicaceae", Taxon))
  })
  names(specimen_list) <- species_tabs
  return(specimen_list)
}

specimen_list <- specimens_read("data/Phys_species_totals.xlsx")

# 2. Check date format ----

#' Write date mismatch log files
#'
#' In the herbarium specimen .xlsx file, dates were converted to an
#' Excel Date format "mm/dd/yyyy" (e.g. 06/15/2003) and saved to a new column
#' using the expression '=TEXT(<CELL>, "mm/dd/yyyy")'.  That column was copied
#' and saved to a new column using 'paste special...' by value.  The
#' `date_mismatch()` function writes two log files to the subdirectory
#' `log/date_logs/` by checking a regular expression against the string in the
#' respective date columns of data frames returned by `specimens_read()`.
#'
#' @param specimen_list List of data frames returned by `specimens_read()`
date_mismatch <- function(specimen_list) {

  # Create log directories and open file connections.
  dir.create("log", showWarnings = FALSE)
  dir.create("log/date_logs", showWarnings = FALSE)
  path_collection <- "log/date_logs/collection_dates.txt"
  path_identification <- "log/date_logs/identification_dates.txt"
  file_collection <- file(description = path_collection, open = "w")
  file_identification <- file(description = path_identification, open = "w")

  # Apply log file output function to list of file connections
  # for collection and identification date columns.
  invisible(lapply(list(file_collection, file_identification),
    function(file_connection) {

      # Set file variables based on the file connection summary string.
      file_description <- summary(file_connection)$description
      if (grepl("collection", file_description))  {
        file_col <- "Date"
        date_regex <- "[0-1][0-9]/[0-3][0-9]/[1-2][0-9][0-9][0-9]"
        }
      if (grepl("identification", file_description)) {
        file_col <- "ID"
        date_regex <-"[!C\\?] [0-1][0-9]/[0-3][0-9]/[1][5-9]"
        }

      # For each specimen data frame, subset non-matching row indices and
      # date strings and write table to file.
      for (i in seq_along(specimen_list)) {
        cat(names(specimen_list)[i],
            paste(c("nrow:", "ncol:"), dim(specimen_list[[i]])), "\n",
            file = file_connection, append = TRUE, sep = "\n")
        date_fix <- as.matrix(grep(date_regex,
                                   specimen_list[[i]][, file_col],
                                   invert = TRUE))
        date_fix <- cbind(date_fix,
                          specimen_list[[i]][date_fix, file_col])
        write.table(date_fix, file = file_connection, append = TRUE,
                    quote = FALSE, row.names = FALSE,
                    col.names = FALSE, sep = "\t\t")
        cat("\n\n", file = file_connection, append = TRUE)
        }
      }))
  close(file_collection)
  close(file_identification)
}

date_mismatch(specimen_list)

# 3. Prior annotations and synonyms ----

# Verify consistent column names and row bind data frames in list.
if (length(unique(sapply(lapply(specimen_list, names), length))) == 1) {

  # Row bind list of specimens data frames into a total specimen data frame.
  specimen_df <- do.call(rbind, specimen_list)

  # Establish list of data frame rows to index splitting .csv files.
  specimen_rows <- lapply(specimen_list, nrow)
  specimen_index <- list()
  row_start <- 1
  for (i in seq_along(specimen_rows)) {
    row_end <- row_start + specimen_rows[[i]] - 1  # Non-inclusive indexing
    specimen_index[[i]] <- seq(row_start, row_end)  # Integer sequence of rows
    row_start <- row_start + specimen_rows[[i]]  # Reset row start index
  }
  names(specimen_index) <- names(specimen_list)
  rm(specimen_list, i, row_start, row_end, specimen_rows)
} else {
  stop("Check column names in excel file.")
}

#' Parse prior annotation column
#'
#' Previous specimen identifications by reviewing botanists were taken from
#' vouchers and stored as comma separated entries for each record
#' (e.g. ""Physaria, Physaria acutifolia, Physaria vitulifera").
#' Here, records with multiple comma-separated annotations in the column
#' `$Taxon` are split into separate entries and built into a new data frame.
#'
#' @param specimen_annotations Character vector of comma separated annotations.
#' @return Data frame of split prior annotations with columns equal to the
#' number of previous annotations plus an additional column with the most
#' recent specimen annotation.
parse_priors <- function(specimen_annotations) {

  # Split list element strings by commas into character vectors of annotations.
  prior_list <- lapply(specimen_annotations, function(taxon) {
    if (grepl(",", taxon))  {
      unlist(strsplit(taxon, ", "))
    } else {
      taxon
    }
  })

  # Replace "!" concurring ID annotation with previous specimen annotation.
  prior_list <- lapply(prior_list, function(priors) {
    if (TRUE %in% grepl("!", priors)) {
      concur_index <- grep("!", priors)
      for (index in concur_index) {
        priors[index] <- priors[index - 1]
      }
      return(priors)
    } else {
      return(priors)
    }
  })

  # Use `plyr::ldply()` to bind data frame from unequal annotation vectors.
  prior_df <- plyr::ldply(prior_list, rbind)
  prior_df[] <- lapply(prior_df, as.character)  # Prevent factor coercion.
  names(prior_df) <- paste0("Physaria_a_priori_", 1:ncol(prior_df))

  # Column bind vector of most recent (i.e. non-missing) annotations.
  prior_recent <- apply(prior_df, MARGIN = 1, function(priors) {
    if (!TRUE %in% is.na(priors)) {
      prior_index <- length(priors)
    } else {
      prior_index <- (min(which(is.na(priors))) - 1)
    }
    priors[prior_index]
  })
  prior_df <- cbind(prior_df,
                    as.data.frame.character(prior_recent,
                                            stringsAsFactors = FALSE))
  names(prior_df)[ncol(prior_df)] <- "Physaria_recent"
  return(prior_df)
}

# Establish data frame of prior identifications and most recent annotations.
prior_df <- parse_priors(specimen_annotations = specimen_df$Taxon)

#' Convert specimen synonyms
#'
#' Given an input character vector of recent annotations, ouput a new vector
#' with text substitutions of accepted synonyms (O'Kane 2010 Flora North Am.).
#'
#' @param recent_annotations Character vector of most recent specimen annotation
#' column from `prior_df` output by `parse_priors()` function.
parse_synonyms <- function(recent_annotations) {

  # Assign vectors for species synonyms.
  syn_acut <- c("^Physaria acutifolia var. purpurea$",
                "^Physaria acutifolia var. stylosa$",
                "^Physaria australis$",
                "^Physaria didymocarpa var australis$",
                "^Physaria didymocarpa var. australis$",
                "^Physaria australis (Payson) Rollins$")

  syn_didy <- c("^Physaria didymocarpa (Hook.) A. Gray$",
                "^Physaria didymocarpa (Hook.) Gray$",
                "^Physaria didymocarpa (Hook.) Gray var.$",
                "^Physaria didymocarpa (Hook.) Gray var. didymocarpa$",
                "^Physaria didymocarpa var normalis$",
                "^Physaria didymocarpa var didymocarpa$",
                "^Physaria didymocarpa var. didymocarpa$",
                "^Physaria didymocarpa ssp didymocarpa$",
                "^Physaria didymocarpa$")

  syn_inte <- c("^Physaria didymocarpa (Hook.) Gray var. integrifolia Rollins$",
                "^Physaria didymocarpa var. integrifolia$",
                "^Physaria integrifolia var integrifolia$",
                "^Physaria integrifolia var. integrifolia$",
                "^Physaria integrifolia var. monticola$")

  syn_saxi_saxi <- c("^Physaria saximontana$",
                     "^Physaria saximontana ssp saximontana$")

  syn_saxi_dent <- "^Physaria saximontana ssp dentata$"

  syn_lyra <- c("^Physaria didymocarpa (Hook.) Gray var. lyrata$",
                "^Physaria didymocarpa (Hook.) Gray var. lyrata Hitch.$",
                "^Physaria didymocarpa ssp lyrata$")

  syn_lana <- c("^Physaria didymocarpa ssp lanata$",
                "^Physaria didymocarpa var lanata$",
                "^Physaria lanata$")

  phys_syns <- list("acut" = syn_acut, "didy" = syn_didy,
                    "inte" = syn_inte, "saxi" = syn_saxi_saxi,
                    "dent" = syn_saxi_dent, "lyra" = syn_lyra,
                    "lana" = syn_lana)

  # Vector for synonym conversions.
  phys_conv <- c("acut" = "Physaria acutifolia",
                 "didy" = "Physaria didymocarpa ssp. didymocarpa",
                 "inte" = "Physaria integrifolia",
                 "saxi" = "Physaria saximontana ssp. saximontana",
                 "dent" = "Physaria saximontana ssp. dentata",
                 "lyra" = "Physaria didymocarpa ssp. lyrata",
                 "lana" = "Physaria didymocarpa ssp. lanata")

  # Compare recent annotations against synonyms and replace matching records.
  synonyms <- sapply(recent_annotations, USE.NAMES = FALSE,
                     function(id) {
                       species <- paste0("^", id, "$")
                       annotation <- character()
                       for (synonym in names(phys_syns)) {
                         if (TRUE %in% grepl(pattern = species,
                                             phys_syns[synonym][[1]],
                                             fixed = TRUE)) {
                           annotation <- phys_conv[synonym]
                           break
                           }
                         }
                       if (length(annotation) == 0) {
                         annotation <- id
                       }
                       return(annotation)
                     })
  return(synonyms)  # Return vector of converted annotation synonyms.
}

# Establish vector of most recent annotation with converted synonyms.
prior_synonyms <- parse_synonyms(recent_annotations = prior_df$Physaria_recent)

# 4. Write specimen output files ----

# Build data frame from `parse_priors()` and `parse_synonyms()` output.
total_physaria <- dplyr::bind_cols(prior_df,
                                   as.data.frame(prior_synonyms,
                                                 stringsAsFactors = FALSE),
                                   specimen_df)
names(total_physaria)[grep("prior_synonyms", names(total_physaria))] <-
  "Physaria_syn"

# Convert geographic coordinate column classes from character to numeric.
total_physaria$Longitude <- as.numeric(total_physaria$Longitude)
total_physaria$Latitude <- as.numeric(total_physaria$Latitude)

#' Write identification logs and output .csv files.
#'
#' @param specimen_df Data frame of total specimen data combined with output
#' from `parse_priors` and `parse_synonyms` functions.
specimens_write <- function(total_specimens) {

  # Write summary tables of most recent specimen annotations to log file.
  dir.create("log/synonyms", showWarnings = FALSE)
  file_syn <- file("log/synonyms/priors_log.txt", open = "w")
  file_conv <- file("log/synonyms/post_log.txt", open = "w")
  invisible(lapply(list(file_syn, file_conv), function(file_connection) {
    file_description <- summary(file_connection)$description
    if (grepl("priors", file_description))  {
      file_title <- "Prior annotations"
      file_col <- "Physaria_recent"
    }
    if (grepl("post", file_description)) {
      file_title <- "Converted annotations"
      file_col <- "Physaria_syn"
    }
    cat(file_title, "\n",
        capture.output(as.matrix(table(as.factor(
          total_specimens[, file_col])))),
        file = file_connection, sep = "\n", append = FALSE)
    close(file_connection)
  }))

  # Write .csv output files from combined data frame indexed by `specimen_list`.
  dir.create("output", showWarnings = FALSE)
  dir.create("output/specimens", showWarnings = FALSE)
  invisible(  # See section 3. for definition of `specimen_index` list.
    mapply(specimen_index, names(specimen_index), USE.NAMES = FALSE,
           FUN = function(df_rows, df_name) {
             specimen_subset <- total_specimens[df_rows, ]
             specimen_name <- gsub(" +", "", df_name) %>%
               gsub("\\.", "_", x = .)
             write.csv(specimen_subset,
                       file = paste0("output/specimens/",
                                     specimen_name, ".csv"))
  }))
}

# Write annotation logs and output species excel tab-specific .csv files.
specimens_write(total_physaria)

# Clean up workspace to remove unnecessary objects and functions.
rm(prior_df, specimen_df, specimen_index, prior_synonyms)

