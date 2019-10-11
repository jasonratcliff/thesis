#!/bin/bash
# Extract R chunks from bookdown .Rmd files.

# Set empty file, null previous contents, and establish temp file.
touch log/chunks/rchunks.Rmd
if [[ -f log/chunks/rchunks.Rmd ]]
then
  cat /dev/null > log/chunks/rchunks.Rmd
fi

# Test for empty string to set all .Rmd files from bookdown build.
if [[ -z "$1" ]]
then
  rmd_files=(index.Rmd 0[1-6]*Rmd)
elif [[ ! "$1" =~ ".Rmd" ]]
then
 echo "Enter an expression matching .Rmd files."
 exit
else
  rmd_files=(index.Rmd "$@" )  # Set array parameter
fi

# Iterate over array of .Rmd files to extract chunks to file.
for rmd in "${rmd_files[@]}"; do
  if [[ ! -f "$rmd" ]]
  then
    echo "$rmd" "... is not a file."
    exit
  fi
  echo "Processing file: " "$rmd"
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

