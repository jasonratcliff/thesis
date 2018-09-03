#!/bin/bash
# Extract figures set by LaTeX environment lstlistings from .TeX file.
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
