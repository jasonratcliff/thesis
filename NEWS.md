# ThesisPackage 0.1.6

* Include phylogeography *.xml*, *.mcc*, and coordinate *.txt* files.

* Add *.csv* for species tree \*BEAST hypotheses.

* Modify `knitr_section()` to accommodate *.docx* output rendering.

# ThesisPackage 0.1.5

* New `species_plot()` builds `ggtree` objects from *BEAST MCC tree data.

* Species Tree *BEAST
    * `beast.R` script to write taxa assignments for species hypotheses.
    * Update final `dna_specimens.csv` annotations.
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
