Data Workflow
=============

``` r
#' Function to list files in a specimen data project subdirectory.
#'
#' Get full path to project directory with the `here::` package (Müller 2017).
#' Path is identified by the .git file located at the project root.
#' Utilizes `magrittr::` %>% pipe operator (Bache and Wickham 2014)
#'
#' @param project_directory Project subdirectory character vector
#' @param file_pattern Optional string for parsing by regular expression.
#' @return Character vector of file names in `project_directory`.
project_files <- function(project_directory, file_pattern = "") {
  full_path <- here::here()  # Assigns vector of path to project root.
  list.files(path = paste0(full_path, project_directory), 
             full.names = TRUE, pattern = file_pattern) %>%  
  gsub(pattern = full_path, replacement = "", x = .)
}
```

1) Specimen Data
================

Records of voucher specimens on loan from lending herbaria (RM, NY, MO, F, ISTC, MONTU, MONT, RSA-POM, UC, UTC, GH, US, CAS, IDS) were compiled into an excel *.xlsx* file. Information recorded from the vouchers included the identification history, collector, collection number, date, institution, locality, geographic coordinates, elevation, ecological description, and measurements of continuous and discrete specimen traits. The file named "Phys\_species\_totals.xlsx" is located in the `/data/1.specimens/` project subdirectory. In total, this project considered 1635 unique collections dated as early as 1844 (Fremont's 2nd Expedition).

``` r
# List files in the specimen data project subdirectory.
project_files("/data/1.specimens")
```

    ## [1] "/data/1.specimens/~$dna_specimens-annotated.xlsx"
    ## [2] "/data/1.specimens/dna_map_spp.csv"               
    ## [3] "/data/1.specimens/dna_specimens-annotated.xlsx"  
    ## [4] "/data/1.specimens/dna_specimens.csv"             
    ## [5] "/data/1.specimens/Phys_species_totals.xlsx"

To read the data into R for downstream phylogenetic, distribution, and morphological analyses, the script `/R/specimen_data.R` is sourced within `/index.Rmd` during the document build using `bookdown::` (Xie 2018). Briefly, the script accomplishes the following:

-   Read in data from excel sheets with `specimens_read()`
    -   Utilizes `openxlsx::` (Walker 2018)

-   Filter records using `dplyr::` (Wickham et al. 2019) to annotations with:
    -   *Physaria*
    -   *Lesquerella*
    -   Brassicaceae
-   Check collection date by regular expression with `date_mismatch()`
    -   Expected date format is MM/DD/YYYY
    -   Write mismatches to the `/log/date_logs/` project subdirectory
-   Combine data from the *.xlsx* file species sheets into a single data frame
    -   Index sheet-respective rows to write .csv files after data cleaning
-   Parse prior annotations with `parse_priors()`
    -   Split the column `$Taxon` containing comma-separated annotation history
    -   Account for identification agreement denoted by "!"
    -   Combine list into data frame with `plyr::` (Wickham 2011)
    -   Extract most recent annotations into a new column
-   Update recent annotations by synonym with `parse_synonyms()`
    -   Synonyms follow O'Kane (2010)
-   Rebuild data frame with parsed annotations and finish cleaning
    -   Cast `$Longitude` and `$Latitude` columns as numeric vectors
    -   Use `lubridate::` (Grolemund and Wickham 2011) to clean dates
        -   Add a column mutation to remove year from date for seasonality
-   Write files with `specimens_write()`
    -   Sheetname indexed .csv files to `/output/specimens/`
    -   Tables of prior and reviewed identifications to `/log/synonyms/`

2) Distribution Maps
====================

TODO - Writeup of `map_tools.R`

DNA Specimens
-------------

A script was written to take the subset of specimens with sequencing data written in the `sequencedHerbariumRecords` chunk of `index.Rmd` and plot distribution maps. Each map is scale to a subset of the taxa and labelled by laboratory accession.

``` r
# Source script to write PDFs of map sequenced specimens.
source("R/map_sequenced")
```

``` r
# List files in the distribution mapping project subdirectory.
project_files("/data/2.distributions/sequenced")
```

    ## [1] "/data/2.distributions/sequenced/map_specimens_co_ut.pdf"  
    ## [2] "/data/2.distributions/sequenced/map_specimens_mt_east.pdf"
    ## [3] "/data/2.distributions/sequenced/map_specimens_mt_west.pdf"
    ## [4] "/data/2.distributions/sequenced/map_specimens_wy_east.pdf"
    ## [5] "/data/2.distributions/sequenced/map_specimens_wy_west.pdf"

References
==========

Bache SM, Wickham H. 2014. magrittr: A forward-pipe operator for R. R package version 1.5. <https://CRAN.R-project.org/package=magrittr>

Grolemund G, Wickham H. 2011. Dates and times made easy with lubridate. Journal of Statistical Software. 40(3):1-25.

Henry L, Wickham H. 2019. purrr: Functional programming tools. R package version 0.3.0. <https://CRAN.R-project.org/package=purrr>

Müller K. 2017. here: A simpler way to find your files. R package version 0.1. <https://CRAN.R-project.org/package=here>

Müller K, Wickham H. 2019. tibble: Simple data frames. R package version 2.0.1. <https://CRAN.R-project.org/package=tibble>

O’Kane SL. 2010. *Physaria*. In: Flora of North America editorial committee. Flora of North America north of Mexico. Volume 7. Salicaceae to Brassicaceae. New York (NY): Oxford Univ. Press. p. 616-665. Payson EB. 1918. Notes on certain Cruciferae. Annals of the Missouri Botanical Garden. 5(2):143-147.

Pagès H, Aboyoun P, Gentleman R, DebRoy S. 2019. Biostrings: Efficient manipulation of biological strings. R package version 2.50.2. <https://www.bioconductor.org/packages/release/bioc/html/Biostrings.html>

Walker A. 2018. openxlsx: Read, write and edit XLSX files. R package version 4.1.0. <https://CRAN.R-project.org/package=openxlsx>

Wickham H. 2011. The split-apply-combine strategy for data analysis. Journal of Statistical Software. 40(1):1-29.

Wickham H, Francois R, Henry L, Müller K. 2019. dplyr: a grammar of data manipulation. R package version 0.8.0.1. <https://CRAN.R-project.org/package=dplyr>

Xie Y. 2018. bookdown: Authoring books and technical documents with R markdown. R package version 0.9. <https://cran.r-project.org/package=bookdown>
