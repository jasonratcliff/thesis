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
