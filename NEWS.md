# ThesisPackage 0.1.2

* New `separate_discrete_trait()` handles discrete trait variables where
  multiple values have been observed from a single specimen voucher.

* New `map_trait_distribution()` provides a convenient wrapper for displaying
  trait distribution maps (e.g., from discrete or continuous variables).

* Add specimen data subsets for continuous and discrete trait distributions.

* `layer_specimens()` now returns a jitter geom instead of points, gaining
  parameters `jitter_width` and `jitter_height` to adjust position.

* New `capitalize()` provides vectorized capitalization of character strings.

* A GNU Makefile provides useful automation checks for `.rda` data creation.

# ThesisPackage 0.1.1

* Add `count_specimens()` to define total unique voucher specimen number.

* `data-raw/`
  - Update `README`
    - Replace DNA map *.pdf* file with *.png*
    - Include SEINet specimens missing coordinate data in *.Rda*
    - `range_split()` returns only min / max values (i.e. two column tibble)
  - Begin migration to reference *.bib* and *.csl* files.

* Add Shiny app for mapping specimen data.

* Move *specimens.xlsx* file to `inst/extdata/` for external availability.

* New `save_plot()` to facilitate `Makefile` *.pdf* / *.png* figure builds.

* New `spl_id()` replaces `map_spp_id()` for specimen identification layering.

# ThesisPackage 0.1.0

* Added a `NEWS.md` file to track changes to the package.

* Specimen mapping functions now use simple features via the `sf` package.
  Plot functions abstract `ggplot2` layer operations. Administrative borders
  are built from data downloaded with the `tigris` package. An error with
  maps built from `elevatr` elevation data occurred for plots containing
  continuous and discrete scales when rendering legend text from HTML via
  the `ggtext` package; addressed using `cowplot` legend extraction and
  grid plotting functions.

* Refactored a shell script to handle multiple sequence alignments.
  `Biostrings` and `msa` R packages are used to write *.tex* files for rendering
  appendix MSA figures with `LaTeX` package `TEXshade`.

* Added distance calculations for Bayesian inference, model tests outputs,
  MSA alignment *.fasta* / *.nexus* files, and results consensus trees (#1).
