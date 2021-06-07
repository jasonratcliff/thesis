# Specimen Data ----

#' Extracted DNA Samples
#'
#' Collated by Jason Ratcliff
#'
#' \describe{
#'   \item{label}{DNA specimen label annotation by `species_collection number`}
#'   \item{Collector}{Name of specimen collector}
#'   \item{Collection_Number}{Number of specimen collection by collector}
#'   \item{Herbarium}{Herbarium institution code of specimen voucher(s)}
#'   \item{State}{State of collection locality}
#'   \item{County}{County of collection locality}
#'   \item{Latitude,Longitude}{Degree decimal of collection coordinates
#'   from GPS or estimated from township, range, section reports.}
#'   \item{ID_prior,ID_final}{Most recent and reviewed annotations}
#'   \item{multiLocus}{Logical vector indicating specimens with multi-locus data.}
#'   \item{header_rITS,header_rps,header_ycf1}{FASTA header identificaitons for
#'   corresponding locus sequence.}
#'   \item{beastSpecies}{Annotation for BEAST analysis}
#'   \item{excel_sheet}{.xlsx sheet name from data import.}
#'   \item{prior_id,prior_1,prior_2,prior_3,prior_4}{Prior identifications}
#'   \item{Taxon}{Unparsed taxa identifications}
#'   \item{Taxon_a_posteriori}{Reviewed taxa identifications}
#'   \item{Date}{Character vector of dates in mm/dd/yyyy format}
#'   \item{Date_parsed}{Date vector in yyyy-mm-dd format}
#'   \item{Date_md}{Date vector with flattened year (i.e. month / date)}
#'   \item{Location}{Text description of collection locality}
#'   \item{ID,App.A,Imaged}{Tracking of identifcations, appendix, and imaging.}
#'   \item{Elev_m,Elev_ft}{Reported elevation range}
#'   \item{Elev_var,Elev_raw}{Non-missing elevation data for `range_split()`}
#'   \item{Elev_raw_min,Elev_raw_max}{Split elevation range output by
#'   `range_split()` function.}
#'   \item{TRS1,TRS2}{Reported township, range, and section data}
#'   \item{Rosulate,Caudex,Pubescence,Basal_leaf_trichomes,Fruit_trichomes,
#'   Stem_count,Stem_shape,Stem_length_dm,Petiole,Basal_leaf_length_cm,
#'   Basal_leaf_shape,Basal_leaf_margins,Cauline_leaf_length_mm,
#'   Cauline_leaf_shape,Cauline_leaf_margins,Racemes,Pedicel_shape,
#'   Pedicels_secund,Sepal_length_mm,Sepal_shape,Petal_color,Petal_length_mm,
#'   Petal_shape,Style_length_mm,Mature_fruit_length_mm,Mature_fruit_width_mm,
#'   Fruit,Mature_fruit_apices,Replum_pubescence,Inner_valve_pubescence,
#'   Ovule_number,Replum_shape,Seed_color,Seed_shape,Mature_seed_length_mm,
#'   Chromosomes}{Morphological trait quantitative measurements and
#'   qualitative observations}
#' }
#'
"dna_specimens"

#' Herbarium Specimen Information
#'
#' Collated by Jason Ratcliff
#'
#' \describe{
#'   \item{excel_sheet}{.xlsx sheet name from data import.}
#'   \item{prior_id,prior_1,prior_2,prior_3,prior_4}{Prior identifications}
#'   \item{Taxon}{Unparsed taxa identifications}
#'   \item{Taxon_a_posteriori}{Reviewed taxa identifications}
#'   \item{Collector}{Name of specimen collector}
#'   \item{Collection_Number}{Number of specimen collection by collector}
#'   \item{Date}{Character vector of dates in mm/dd/yyyy format}
#'   \item{Date_parsed}{Date vector in yyyy-mm-dd format}
#'   \item{Date_md}{Date vector with flattened year (i.e. month / date)}
#'   \item{Herbarium}{Herbarium institution code of specimen voucher(s)}
#'   \item{State}{State of collection locality}
#'   \item{County}{County of collection locality}
#'   \item{Location}{Text description of collection locality}
#'   \item{Latitude,Longitude}{Degree decimal of collection coordinates
#'   from GPS or estimated from township, range, section reports.}
#'   \item{ID,App.A,Imaged}{Tracking of identifcations, appendix, and imaging.}
#'   \item{Elev_m,Elev_ft}{Reported elevation range}
#'   \item{Elev_var,Elev_raw}{Non-missing elevation data for `range_split()`}
#'   \item{Elev_raw_min,Elev_raw_max}{Split elevation range output by
#'   `range_split()` function.}
#'   \item{TRS1,TRS2}{Reported township, range, and section data}
#'   \item{Rosulate,Caudex,Pubescence,Basal_leaf_trichomes,Fruit_trichomes,
#'   Stem_count,Stem_shape,Stem_length_dm,Petiole,Basal_leaf_length_cm,
#'   Basal_leaf_shape,Basal_leaf_margins,Cauline_leaf_length_mm,
#'   Cauline_leaf_shape,Cauline_leaf_margins,Racemes,Pedicel_shape,
#'   Pedicels_secund,Sepal_length_mm,Sepal_shape,Petal_color,Petal_length_mm,
#'   Petal_shape,Style_length_mm,Mature_fruit_length_mm,Mature_fruit_width_mm,
#'   Fruit,Mature_fruit_apices,Replum_pubescence,Inner_valve_pubescence,
#'   Ovule_number,Replum_shape,Seed_color,Seed_shape,Mature_seed_length_mm,
#'   Chromosomes}{Morphological trait quantitative measurements and
#'   qualitative observations}
#' }
#'
"herbarium_specimens"

#' SEINet Data
#'
#' SEINet records for *P. floribunda* and *P. bellii*.
#'
#' @source <https://swbiodiversity.org/seinet/index.php>
#'
#' \describe{
#'   \item{scientificName}{Identification by scientific name}
#'   \item{scientificNameAuthorship}{Taxon author}
#'   \item{genus}{Genus}
#'   \item{specificEpithet}{Specific epithet}
#'   \item{taxonRank}{Taxon rank (e.g. var., subsp.)}
#'   \item{infraspecificEpithet}{Infraspecific epithet}
#'   \item{identifiedBy}{Record identifier}
#'   \item{recordedBy}{Record collector}
#'   \item{recordNumber}{Collection number}
#'   \item{eventDate}{Date of collection}
#'   \item{year}{Year of collection}
#'   \item{stateProvince}{State}
#'   \item{county}{County}
#'   \item{locality}{Description of collection locality}
#'   \item{Latitude}{Degree decimal latitude}
#'   \item{Longitude}{Degree decimal longitude}
#'   \item{verbatimCoordinates}{Verbatim coordinates: TRS, WGS}
#'   \item{verbatimElevation}{}
#'   \item{recordId}{SEINet unique record identifier}
#'   \item{references}{SEINet record site reference}
#' }
#'
"seinet_coords"

#' CO Front Range Subset
#'
#' Used for Roxygen @@examples and testthat unit test. \cr
#' Bounding box:
#' - Longitude: c(-107.5, -104.5)
#' - Latitude: c(38, 41)
#'
"spp_co_front_range"

# ggplot Scales ----

#' Species Color Aesthetics
#'
#' \describe{
#'   \item{spp_color}{Vector with values for ggplot color aesthetics}
#' }
"spp_color"

#' Species Shape Aesthetics
#'
#' \describe{
#'   \item{spp_shape}{Vector with values for ggplot shape aesthetics}
#' }
"spp_shape"

