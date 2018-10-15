#!/bin/bash
# Extract figures set by LaTeX lstlistings environments from .TeX file.
# Writes new .tex file that can be included in Bookdown build.
# Usage from project directory main level: 
# ./TeX/lstlisting_figs.sh TeX/Thesis_A-seq-analysis.tex
# See _bookdown.yml for PDF compilation instrunctions.
# Requires index.Rmd and associated LaTeX YAML formatting.
tex_file=${1}
tex_figs=${tex_file%.tex}-figs.tex
awk '
  { if (/^\\begin{lstlisting}*/) {
    i = 1}
  }
  { if (i == 1) {
    print $0}
  }
  { if (/^\\end{lstlisting}*/) {
    i--;
    print "\n", "\\clearpage" "\n"}
  }' $1 > $tex_figs
