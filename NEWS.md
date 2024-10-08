# thesis (development version)

## Breaking changes

* Utilize simple features via `sf` for spatial operations
    + New superclass `Extent` 
        - Public method `$bbox()` replaces `Specimen$filter_limit()`

* Rename exported package data objects:

| Old             | New       |
|:----------------|:----------|
| `seinet_coords` | `seinet`  |

## New features

* Implement Darwin Core biodiversity standard terms for specimen data (#106).
  - Improve `tidyverse` data carpentry for `vouchers` raw data (#105). 

* Reduce evalution time of `roxygen` documentation example code (#99).

* Replace local caching of census border shapefiles with persistent data (#102).

## Minor improvements and fixes

* Consolidate `@importFrom` statements into `R/thesis-package.R`

* Remove non-portable `data-raw/appendix` scripts (#72).

* Consolidate package raw data and R scripts to `data-raw/` (#71).
  - `specimens.xlsx` is no longer installed as raw package data.

# thesis 0.3.2

## Breaking changes

* Improve package interface by prefixing `Specimen` filter methods (#93).
    + `filter_limit()` replaces `limit()`
    + `filter_taxa()` replaces `taxa()`
    + `filter_collections()` replaces `collections()`

* Clarify when the `records` field tibble is updated vs. returning a tibble.
    + Filter methods gain a `.return` parameter to specify chained operations.

* Allow flexible specification of `records` variable for taxonomic operations.
    + Taxa filtering and annotation methods gain a `.identifier` parameter.

## New features

* Update `renv.lock` lockfile to support R v4.3.0 (#75).

* Add `Gitpod` configuration to enable continuous development environment (#74).
    + Include `renv` support for R v4.3.0 environment (#78).

* Add `pkgdown` YAML configuration for package website (#70, #76).
    + Move DNA specimen map from `data-raw/README.Rmd` to `vignettes/articles`
      (#66).

## Minor improvements and fixes

* Fix `mustashe` environment evaluation error by requiring packing development
  version in `DESCRIPTION` and `renv.lock` (#79).

* Add `rgdal` package import required by `elevatr` (#83).

* `SpecimenMap$features()` gains two new parameters:
    + `sf_states` passes a character vector to `tigris::counties()`
       for layering county borders from additional states.

    + `expand` controls limit expansion by `ggplot2::coord_sf()` to account
      for clipping raster layers with satellite or elevation data.

* `SpecimenMap$repel()` gains a `vouchers` parameter to specify arbitrary
  labels given a tibble with collector, collection, longitude, and latitude.

* Specimen R scripts and raw data are consolidated into `data-raw/specimens`.

| New                                | Old                                        |
|------------------------------------|--------------------------------------------|
| `data-raw/specimens/aesthetics.R`  | `data-raw/mapping/aesthetics.R`            |
| `data-raw/specimens/dna.R`         | `data-raw/specimens/dna_specimens.R`       |
| `data-raw/specimens/dna.csv`       | `data-raw/specimens/dna_specimens.csv`     |
| `data-raw/specimens/vouchers.R`    | `data-raw/specimens/herbarium_specimens.R` |
| `data-raw/specimens/vouchers.xlsx` | `inst/extdata/specimens.xlsx`              |

* `data-raw/appendix` scripts configured for local paths are removed for
  package portability (#72).

# thesis 0.3.1

## Minor improvements and fixes

* `Specimens$collections()` returns a tibble without modifying the `records`
  field, allowing for use within other public methods (#67).

# thesis 0.3.0

## Breaking changes

* Rename package from `Thesis` to lowercase `thesis` (#64).

## New features

* A number of previously exported functions are refactored into a specimen
  container using the R6 OOP system. Despite the non-(R)-idiomatic style of R6,
  the reference semantics present a useful implementation for the various
  subsetting and plotting operations covered by these functions (#63).

* Manual scale values from `spp_color` and `spp_shape` are combined into a new
  exported data `aesthetics` tibble.

### `Specimen`

* New R6 superclass `Specimen` data structure consolidates specimen record
  tibbles with previously exported functions for subsetting and theme options.

| method                    | function            | status                 |
|---------------------------|---------------------|------------------------|
| `Specimen$census()`       | `count_specimens()` | `R/deprec-specimens.R` |
| `Specimen$limit()`        | `subset_coords()`   | `R/deprec-specimens.R` |
| `Specimen$taxa()`         | `filter_reviewed()` | `R/deprec-specimens.R` |
| `Specimen$collections()`  | `find_spp()`        | `R/deprec-specimens.R` |
| `Specimen$annotations()`  | `spl_labels()`      | `R/deprec-themes.R`    |
| `Specimen$labels()`       | `parse_taxa()`      | `R/deprec-themes.R`    |

### `SpecimenMap`

* The `SpecimenMap` inherits methods from the superclass, and defines public
  and private methods to consolidate previously exported functions (#61, #45).

| method                    | function            | status              |
|---------------------------|---------------------|---------------------|
| `SpecimenMap$map()`       | `build_map()`       | `R/deprec-maps.R`   |
| `SpecimenMap$features()`  | `layer_borders()`   | `R/deprec-maps.R`   |
| `SpecimenMap$specimens()` | `layer_specimens()` | `R/deprec-maps.R`   |
| `SpecimenMap$scales()`    | `layer_themes()`    | `R/deprec-maps.R`   |
| `SpecimenMap$theme()`     | `layer_themes()`    | `R/deprec-maps.R`   |
| `SpecimenMap$repel()`     | `spl_id()`          | `R/deprec-maps.R`   |
| `private$base_ggmap()`    | `layer_ggmap()`     | `R/deprec-maps.R`   |
| `private$base_elevatr()`  | `layer_elevation()` | `R/deprec-maps.R`   |

- Superseded helper functions:
  - `spl_states()`
  - `spl_order()`
  - `spl_bbox()`
  - `spl_legend()`

# thesis 0.2.3

* New `parse_taxa()` creates plotmath expressions for `ggplot2` parsed labels.
    - The following functions are refactored to generate expressions:
        - `haplotype_labels()`
        - `species_plot()`

* New `italicize()` wraps text vector by `knitr` pandoc conversion.

* Manual scale values from `spp_color` and `spp_shape` are combined into a new
  exported data `aesthetics` tibble.

* `haplotype_labels()` gains `id_column` parameter.
    - Specifies data variable for tidy evaluation

* `species_plot()` gains `label_size` parameter.

* Manuscript Revisions
    - Add multilocus phylogenetics results and species discussions
    - Update dichotomous key, descriptions, and `.bib` references
    - Typeset discussion and `.tex` appendices
    - Standardize `.Rmd` chunk names, crosslinking, and captions.
        - Differentiates `LaTeX` short captions for LoT & LoF
    - General spelling and grammar check

* Figure Revisions
    - Add repelled taxa labels and node probabilities to phylogenetic trees
    - Standardize `ggplot` guide legends
    - Re-size discussion trait plot margins

* Update `Makefile` target dependencies for building specimen `.rda`

* `LaTeX` Edits
    - Add thesis abstract
    - Proof specimen appendices
    - Remove window margins

* Specific epithet of prospective lineage is now quoted
    - Table 2 \*BEAST hypotheses is formatted with `LaTeX` quotation

<!-- TODO: See v0.2.2.9000-->

# thesis 0.2.2

* Add `ggrepel` function wrappers to specify label position nudges leveraging
  `rlang` expression handling:
    - `repel_map_labels()`
    - `repel_tree_labels()`
    - `repel_node_labels()`

* Use markdown for Roxygen documentation

* Add `pkgdown` infrastructure

# thesis 0.2.1

* Medicine Bow and CO Front Range satellite map proofs.

* Add `git` punchard to `README`.

* Implement `purrr` map / reduce of `ggplot` calls to add labels with `ggrepel`.

* Tune plot margins for *.png* and *.pdf* images.

* Remove trait `.rda` data files from `data/`.

* Update project `Makefile` and package dependencies.
    - Render figures before install and check to compile manuscript.

* Fix join of DNA specimens by specifying collection coordinate variables.

# thesis 0.2.0

* Merge `ThesisPackage` into single `Thesis` R package repository.
    - Compartmentalize `bookdown` manuscript and `xaringan` slide rendering.
    - Move top-level project directories to `inst/` and `data-raw/`.
        - Appendix summaries
        - Species descriptions
        - R markdown Word *.docx* chapters
    - Update namespace for functions, data, and installed file paths.

# ThesisPackage 0.1.9

* Replace all instances of `ssp.` abbreviation with string `subsp.`.

# ThesisPackage 0.1.8

* New `jitter_violin()` wraps `ggplot()` to layer violin and jitter geoms
  by reviewed ID (x-axis) with color and shape aethetics binding to
  prior identifications.

* New `trait_phenology()` wraps `ggplot()` to plot a trait by continuous date
  of collection with a smoothing geom.

* New `annotation_grid()` builds extractable legend with prior / reviewed
  shape and color aesthetic bindings to build with `cowplot::plot_grid()`.

* New `filter_reviewed()` subsets specimen records to taxa of interest.
  Non-study specimens were excluded from `herbarium_specimens` and
  outgroup taxa were separated to an `outgroup` *.xlsx* sheet.

# ThesisPackage 0.1.7

* `layer_specimens()` gains `jitter_alpha` argument to set aesthetic alpha value.

* Re-balance `ggplot` color and shape aesthetics for maps and trees.

* Fix column specification in `layer_elevation()` wrapper to `elevatr`.

# ThesisPackage 0.1.6

* Include phylogeography *.xml*, *.mcc*, and coordinate *.txt* files.

* Add *.csv* for species tree \*BEAST hypotheses.

* Modify `knitr_section()` to accommodate *.docx* output rendering.

# ThesisPackage 0.1.5

* New `species_plot()` builds `ggtree` objects from *BEAST MCC tree data.

* Species Tree *BEAST
    * `beast.R` script to write taxa assignments for species hypotheses.
    * Update final `dna.csv` annotations.
    * Include `.mcc`, `.xml`, and `.txt` files for BEAST2 runs.

* Update binary *.xlsx* and *.rda* files with herbarium and DNA specimen data.

* Add phylogeography coordinate files.

# ThesisPackage 0.1.4

* BEAST Runs
    * Update `data-raw/README` with analysis workflow.
    * Add `BEAST/README` with installation and run info.
    * Include script to write NEXUS files for ingroup taxa.
    * Input *.nex* files and multi-locus partitioned BEAUTi *.xml* file.
    * Combined analysis maximum clade credibility (*.mcc*) tree file.


* New function `potentially_informative_sites()` wraps `ggmsa()` call from
  `Biostrings` multiple sequence alignment sites with multiple character states.

* Add Thesis methods / results *.csv* files for collections, primers, and *ycf1*.

* `node_labels()` output to `bayes_kable()` genotypes ordered by group size.


* `bayes_plot()` gains `ggtree_layout` argument
    - Specifies `ggtree()` argument `layout` for shape of tree plot.

# ThesisPackage 0.1.3

* Refactor phylogeny functions:
    - Tree Data
        - read_tree()
        - node_labels()
        - node_geoms()
        - join_bayes()
        - conserved_vouchers()
    - MrBayes
        - bayes_ggtree()
        - bayes_themes()
        - bayes_plot()
        - bayes_kable()
    - BEAST
        - beast_posterior()
        - beast_labels()
        - beast_theme()
        - beast_plot()
        - beast_legend_color()
        - beast_legend_posterior()

* Update ggplot theme manual scale specification for color and shape.

* `save_plot()` gains `...` to forward arguments to `ggplot2::ggsave()`.

* Base layer `geom_point()` call removed from `layer_elevation()`.

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

* Move *vouchers.xlsx* file to `inst/extdata/` for external availability.

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
