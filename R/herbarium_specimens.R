#' Herbarium Specimen Information
#'
#' Collated by Jason Ratcliff
#'
#' \describe{
#' \item{excel_sheet}{.xlsx sheet name from data import.}
#' \item{prior_id,prior_1,prior_2,prior_3,prior_4}{Prior identifications}
#' \item{Taxon}{Unparsed taxa identifications}
#' \item{Taxon_a_posteriori}{Reviewed taxa identifications}
#' \item{Collector}{Name of specimen collector}
#' \item{Collection_Number}{Number of specimen collection by collector}
#' \item{Date}{Character vector of dates in mm/dd/yyyy format}
#' \item{Date_parsed}{Date vector in yyyy-mm-dd format}
#' \item{Date_md}{Date vector with flattened year (i.e. month / date)}
#' \item{Herbarium}{Herbarium institution code of specimen voucher(s)}
#' \item{State}{State of collection locality}
#' \item{County}{County of collection locality}
#' \item{Location}{Text description of collection locality}
#' \item{Latitude,Longitude}{Degree decimal of collection coordinates
#'   from GPS or estimated from township, range, section reports.}
#' \item{ID,App.A,Imaged}{Tracking of identifcations, appendix, and imaging.}
#' \item{Elev_m,Elev_ft}{Reported elevation range}
#' \item{Elev_var,Elev_raw}{Non-missing elevation data for `range_split()`}
#' \item{Elev_raw_min,Elev_raw_max}{Split elevation range output by
#'   `range_split()` function.}
#' \item{TRS1,TRS2}{Reported township, range, and section data}
#' \item{Rosulate,Caudex,Pubescence,Basal_leaf_trichomes,Fruit_trichomes,
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

