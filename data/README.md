``` r
# Get full path to project directory with the `here::` package (Müller 2017)
require(here)

# Set the root directory to knit at the project main level
require(knitr)
opts_knit$set(root.dir = here::here())  # Indexed to repository .git file

# Utilizes `magrittr::` %>% pipe operator (Bache and Wickham 2014)
require(magrittr)

# Functions for FASTA reading and DNAStringSet manipulation
require(Biostrings)

# Nifty functional programming tools and data structures from tidyverse
require(purrr)
require(tibble)
```

Data Workflow
-------------

The root directory to knit this *.Rmd* document is set as the project root with the function `here::here()` defined by the top level *.git* file (Müller 2017). Example paths in this document should be considered from the project root.

Specimen Data
=============

Records of voucher specimens on loan from lending herbaria (RM, NY, MO, F, ISTC, MONTU, MONT, RSA-POM, UC, UTC, GH, US, CAS, IDS) were compiled into the excel file `Phys_species_totals.xlsx`. Information recorded from the vouchers included the identification history, collector, collection number, date, institution, locality, geographic coordinates, elevation, ecological description, and measurements of continuous and discrete specimen traits. The file is located in the `data/1.specimens/` project subdirectory. In total, this project considered 1635 unique collections dated as early as 1844 (Fremont's 2nd Expedition).

    data/1.specimens/Phys_species_totals.xlsx

To read the data into R for downstream phylogenetic, distribution, and morphological analyses, the script `R/specimen_data.R` is sourced within `index.Rmd` during the document build by `bookdown::render_book()` (Xie 2018). Briefly, the script accomplishes the following:

-   Read in data from excel sheets with `specimens_read()`
    -   Utilizes `openxlsx::` (Walker 2018)

-   Filter records using `dplyr::` (Wickham et al. 2019) to annotations with:
    -   *Physaria*
    -   *Lesquerella*
    -   Brassicaceae
-   Check collection date by regular expression with `date_mismatch()`
    -   Expected date format is MM/DD/YYYY
    -   Write mismatches to the `log/date_logs/` project subdirectory
-   Combine data from the *.xlsx* file species sheets into a single data frame
    -   Index sheet-respective rows to write *.csv* files after data cleaning
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
    -   Sheetname indexed *.csv* files to `output/specimens/`
    -   Tables of prior and reviewed identifications to `log/synonyms/`

Distribution Mapping
====================

TODO - Writeup of `map_tools.R`

DNA Specimens
-------------

The script `R/map_sequenced.R` writes distribution map subsets of specimens with sequencing data. A data frame for this script is written to `data/dna_map_spp.csv` in the `sequencedHerbariumRecords` chunk of `index.Rmd`. Each map is scaled to a subset of the taxa and labelled by laboratory accession.

``` r
# Source script to write PDFs of map sequenced specimens.
source("R/map_sequenced.R")
```

    data/2.distributions/sequenced/map_specimens_co_ut.pdf
    data/2.distributions/sequenced/map_specimens_mt_east.pdf
    data/2.distributions/sequenced/map_specimens_mt_west.pdf
    data/2.distributions/sequenced/map_specimens_wy_east.pdf
    data/2.distributions/sequenced/map_specimens_wy_west.pdf

FASTA Subset
============

Multi-FASTA files from sequenced loci were assembled from sequence chromatograms and deposited in the `data/3.raw-fastas/` project subdirectory. Here, FASTA headers are formatted with two fields `accession` and `locus` following "&gt;". A single whitespace separates the sample accession from the name of the locus, for example: `>PACUT_48 rITS` or `>PACUT_12821 rps`. A given FASTA file contains sequences sampled from a single genetic locus and all FASTA headers in the file contain the same locus name in the second field.

``` r
# Assign list of raw FASTA files from project subdirectory.
fasta_files <-
   list.files(path = "data/3.raw-fastas",
              pattern = ".fasta$", full.names = TRUE)
```

------------------------------------------------------------------------

Read FASTA into R
-----------------

The Biostrings package (Pagès et al. 2019) was used to read in FASTA files as DNAStringSets, R S4 objects of the XString subclass. A list is assigned to hold the DNAStringSets read from the FASTA files, where each element of the list contains the DNA sequences read from a single FASTA file.

``` r
# Assign list of DNAStringSet objects from FASTA files in input directory.
fasta_list <- lapply(fasta_files, Biostrings::readDNAStringSet)
```

``` r
class(fasta_list[[1]])  # Object class of 1st list element
```

    ## [1] "DNAStringSet"
    ## attr(,"package")
    ## [1] "Biostrings"

``` r
fasta_list[[1]]  # S4 print of 1st DNAStrinSet
```

    ##   A DNAStringSet instance of length 72
    ##      width seq                                         names               
    ##  [1]   582 CCCGCGAACCTATTATCACC...CTCTCCCGAAGCTCTTGATG PACUT_48 rITS
    ##  [2]   582 CCCGCGAACCTATTATCACC...CTCTCCCGAAGCTCTTGATG PACUT_3721 rITS
    ##  [3]   582 CCCGCGAACCTATTATCACC...CTCTCCCGAAGCTCTTGATG PACUT_3769 rITS
    ##  [4]   582 CCYGCGAACCTATTATCACC...CTCTCCCGAAGCTCTTGATG PACUT_8621 rITS
    ##  [5]   582 CCCGCGAACCTATTATCACC...CTCTCCCGAAGCTCTTGATG PACUT_12821 rITS
    ##  ...   ... ...
    ## [68]   582 CCCGCGAACCTATTATCACC...CTCTCCCGAAGCTCTTGATG PVITU_16713 rITS
    ## [69]   582 CCCGCGAACCTATTATCACC...CTCTCCCGAAGCTCTTGATG PVITU_19075 rITS
    ## [70]   582 CCCGCGAACCTATTATCACC...CTCTCCCGAAGCTCTCGATG LARGY_7548 rITS
    ## [71]   582 CCCGCGAACYTATTATCACC...CTCTCCCGAAGCTCTTGATG LFEND_4355 rITS
    ## [72]   581 CCCGCGAACCTATTATCACC...CTCTCCCGAAGCTCTTGATG POBCO_3762 rITS

Extract Header Names
--------------------

To find the intersecting set of sequence headers among multiple FASTA files, the names attributes are extracted and assigned to a list.

``` r
# Assign lists of FASTA header name vectors and associated loci.
header_list <- lapply(fasta_list, names)
head(header_list[1][[1]], n = 9)
```

    ## [1] "PACUT_48 rITS"    "PACUT_3721 rITS"  "PACUT_3769 rITS" 
    ## [4] "PACUT_8621 rITS"  "PACUT_12821 rITS" "PACUT_14050 rITS"
    ## [7] "PACUT_14222 rITS" "PACUT_16125 rITS" "PACUT_26706 rITS"

FASTA loci separated by whitespace from the specimen accession label are extracted by iteratively splitting the vector elements of the list, subsetting the second element of the string split (i.e. the locus), and applying a call of `unique` to ensure a single element was extracted from the FASTA header splits.

Three loci are expected:

-   rITS
-   *rps*
-   *ycf1*

``` r
(fasta_loci <- lapply(header_list, function(locus_headers) {
  unique(sapply(locus_headers, USE.NAMES = FALSE,
                function(fas_header) {
                  split_name <- unlist(strsplit(x = fas_header, " "))[2]
                  }))
  }))
```

    ## [[1]]
    ## [1] "rITS"
    ## 
    ## [[2]]
    ## [1] "rps"
    ## 
    ## [[3]]
    ## [1] "ycf1"

Extract FASTA Headers
---------------------

Loci extracted from the FASTA headers are assigned as the names attribute of the sequence header list. To make logs of the header extraction, the header and locus name vectors are simultaneously mapped using the `purrr::map2()` (Henry and Wickham 2019) and `tibble::tibble()` (Müller and Wickham 2019) functions.

``` r
# Assign names attribute to list of FASTA headers.
names(header_list) <- fasta_loci

# Write headers to locus-specific .csv files to log header extraction.
invisible(
  purrr::map2(header_list, fasta_loci, function(headers, locus) {
    unlist(headers) %>% tibble::tibble() %>%
      write.csv(x = ., file = paste0("data/3.raw-fastas/",
                                     locus, "-headers.csv"))
    })
  )
```

Clean and Reduce Header List
----------------------------

To identify the intersecting set of sequence headers, the locus name is removed from the sequence headers and vectors of the sample accessions are stored in a new list, `specimen_list`. This list contains character vectors of sequence headers for each FASTA file with the locus removed, matching the unprocessed sequence header character vectors in `header_list`. The character vectors of `specimen_list` should be of equal length relative to `header_list`.

``` r
# Remove gene metadata from list of FASTA headers and test split.
specimen_list <- lapply(header_list, function(locus_headers) {
  sapply(locus_headers, USE.NAMES = FALSE,  function(fas_header) {
    split_name <- unlist(strsplit(x = fas_header, " "))[1]
    })
  })
if (!identical(lapply(header_list, length),
               lapply(specimen_list, length))) {
  stop("Error in FASTA header splitting.")
}
```

    header_list

    $rITS
    [1] 72

    $rps
    [1] 66

    $ycf1
    [1] 51


    specimen_list

    $rITS
    [1] 72

    $rps
    [1] 66

    $ycf1
    [1] 51

The intersecting set of FASTA headers common to all input files is used to index `fasta_list` containing the DNAStringSet sequences.

``` r
# Identify common FASTA headers among split names.
(fasta_common <- Reduce(intersect, specimen_list) %>% sort())
```

    ##  [1] "LARGY_7548"     "LFEND_4355"     "PACUT_14050"    "PACUT_14222"   
    ##  [5] "PACUT_26706"    "PACUT_3721"     "PACUT_48"       "PACUT_5199"    
    ##  [9] "PACUT_7997"     "PACUT_8621"     "PBRAS_3122"     "PBRAS_3130"    
    ## [13] "PBRAS_4225"     "PBRAS_53"       "PBRAS_54"       "PCOND_16765"   
    ## [17] "PCOND_3787"     "PCOND_40"       "PCOND_41"       "PDIDY_DI_12677"
    ## [21] "PDIDY_DI_3794"  "PDIDY_DI_46"    "PDIDY_DI_47"    "PDIDY_LA_14773"
    ## [25] "PDIDY_LA_32"    "PDIDY_LA_52"    "PDIDY_LY_2689"  "PDIDY_LY_3855" 
    ## [29] "PDIDY_LY_45"    "PDORN_42"       "PDORN_4376"     "PEBUR_10410"   
    ## [33] "PEBUR_14841"    "PEBUR_37"       "PEBUR_38"       "PINTE_17493"   
    ## [37] "PINTE_19288"    "PINTE_43"       "PINTE_44"       "PSAXI_DE_4481" 
    ## [41] "PSAXI_DE_56"    "PSAXI_DE_9833"  "PSAXI_SA_39"    "PSAXI_SA_50"   
    ## [45] "PSAXI_SA_74315" "PSAXI_SA_9636"  "PVITU_10105"    "PVITU_16713"   
    ## [49] "PVITU_19075"    "PVITU_3754"     "PVITU_9837"

For each vector of FASTA headers in `header_list`, the common headers are indexed against the locus FASTA headers.

``` r
# Grep common names against DNAStringSet names for subset indexing.
fasta_matches <- lapply(header_list, function(locus_headers) {
  sapply(fasta_common, function(fasta_header) {
    grep(fasta_header, locus_headers)
    })
  })
```

The header indexes are then used to subset the DNAStringSets and assign a new list with the common sequences for each locus. Each subset DNAStringSet is written to a new FASTA file in the `data/4.subset-fastas/` project subdirectory.

``` r
# Subset FASTA sequences by index of common FASTA headers and write to file.
fasta_subsets <- list()
for (i in seq_along(fasta_matches)) {
  fasta_subsets[[i]] <- fasta_list[[i]][fasta_matches[[i]]]
  writeXStringSet(x = fasta_subsets[[i]], width = 50,
                  filepath = paste0("data/4.subset-fastas/",
                                    fasta_loci[[i]], "-subset.fasta"))
}
```

Sequence Alignment and Concatenation
====================================

Untrimmed, single locus FASTA files from `data/3.raw-fastas` and FASTA files filtered to common specimens in `data/4.subset-fastas` were aligned using MAFFT. The G-INS-i alignment strategy with iterative refinement and 1PAM / k=2 nucleotide scoring matrix were set sat as the alignment parameters.

    data/3.raw-fastas/rITS-combined_raw.fasta 

        Number of sequences: 72 
        Range of sequence width: 581 583 

    data/3.raw-fastas/rps-combined_raw.fasta 

        Number of sequences: 66 
        Range of sequence width: 802 852 

    data/3.raw-fastas/ycf1-combined_raw.fasta 

        Number of sequences: 51 
        Range of sequence width: 511 524 

    data/4.subset-fastas/rITS-subset.fasta 

        Number of sequences: 51 
        Range of sequence width: 581 583 

    data/4.subset-fastas/rps-subset.fasta 

        Number of sequences: 51 
        Range of sequence width: 802 852 

    data/4.subset-fastas/ycf1-subset.fasta 

        Number of sequences: 51 
        Range of sequence width: 511 524 

    data/5.alignments-concatenated/rITS-ml.fasta 

        Number of sequences: 51 
        Range of sequence width: 586 586 

    data/5.alignments-concatenated/rps-ml.fasta 

        Number of sequences: 51 
        Range of sequence width: 889 889 

    data/5.alignments-concatenated/ycf1-ml.fasta 

        Number of sequences: 51 
        Range of sequence width: 524 524 

    data/5.alignments-single/rITS-single.fasta 

        Number of sequences: 72 
        Range of sequence width: 586 586 

    data/5.alignments-single/rps-single.fasta 

        Number of sequences: 66 
        Range of sequence width: 889 889 

    data/5.alignments-single/ycf1-single.fasta 

        Number of sequences: 51 
        Range of sequence width: 524 524 

The pipeline outlined above for subsetting FASTA files was used to read in the aligned subset of specimens for concatenated analysis.

``` r
# Assign list of DNAStringSet objects from FASTA files in input directory.
ml_aligned <-
  list.files(path = "data/5.alignments-concatenated/",
             pattern = ".fasta$", full.names = TRUE) %>%
  lapply(X = ., Biostrings::readDNAStringSet)

# Assign lists of FASTA header name vectors and associated loci.
ml_headers <- lapply(ml_aligned, names)

# Extract locus names. 
fasta_loci <- lapply(ml_headers, function(locus_headers) {
  unique(sapply(locus_headers, USE.NAMES = FALSE,
                function(fas_header) {
                  split_name <- unlist(strsplit(x = fas_header, " "))[2]
                }))
})

# Remove gene metadata from list of FASTA headers and test split.
ml_specimens <- lapply(ml_headers, function(locus_headers) {
  sapply(locus_headers, USE.NAMES = FALSE,  function(fas_header) {
    split_name <- unlist(strsplit(x = fas_header, " "))[1]
  })
})
if (!identical(lapply(header_list, length),
               lapply(specimen_list, length))) {
  stop("Error in FASTA header splitting.")
}

# Identify common FASTA headers among split names.
fasta_common <- Reduce(intersect, ml_specimens) %>% sort()

# Grep common names against DNAStringSet names for subset indexing.
fasta_matches <- lapply(ml_headers, function(locus_headers) {
  sapply(fasta_common, function(fasta_header) {
    grep(fasta_header, locus_headers)
  })
})

# Subset FASTA sequences by index of common FASTA headers and write to file.
ml_fasta <- list()
for (i in seq_along(fasta_matches)) {
  ml_fasta[[i]] <- ml_aligned[[i]][fasta_matches[[i]]]
}

# Concatenate xstring (DNAStringSet) sequences sorted by name.
ml_concat <-
  xscat(ml_fasta[[1]], ml_fasta[[2]], ml_fasta[[3]])
names(ml_concat) <- fasta_common  # Add names attribute from sorted headers.

# Write DNAStringSet to FASTA file.
writeXStringSet(ml_concat, width = 1999,
                filepath = "data/5.multi-locus/ml_concatenated.fasta")
```

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
