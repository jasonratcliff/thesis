#!/bin/bash
# Extract R chunks from bookdown .Rmd files.

rmd_files=(index.Rmd 0[1-6]*Rmd)
touch log/chunks/rchunks.Rmd
cat /dev/null > log/chunks/rchunks.Rmd

for rmd in "${rmd_files[@]}"; do
  echo "${rmd}"
  awk '{
      if (/^```{r*/) {
          i = 1
      }
      if (i == 1) {
          print $0
      }
      if (/^``` +?$/) {
          i--;
          print "\n", "\\clearpage", "\n"
      }
  }' $rmd >> log/chunks/rchunks.Rmd
done

