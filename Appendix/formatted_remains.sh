#!/bin/bash

# Bash script to return number of specimens remaining in formatted .tsv files.

echo "" > Appendix/appendix_remains.txt  # Open new file for appending.

# Append line counts for each formatted tabular data file to new file.
for filename in Appendix/*formatted.tsv; do
  wc -l $filename >> Appendix/appendix_remains.txt;
done

# Parse line count to three decimal places, pipe output to sed to remove
# initial line from file creation, and sort in reverse by number of specimens.
awk 'BEGIN {FS=" "}; 
  {$1 = sprintf("%03d", $1); printf "%s\n", $0}' \
  Appendix/appendix_remains.txt | \
  sed -n '2,$p' | sort -r > Appendix/appendix_remains_sorted.txt

# Print sum total of remaining specimens from column 1 to standard output.
awk 'BEGIN {FS=" "}; {spp+=$1}; END {printf "%s specimens remaining.\n", spp}' \
  Appendix/appendix_remains_sorted.txt
