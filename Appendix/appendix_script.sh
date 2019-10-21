#!/bin/bash

# Bash script to wrap appendx_script.R into AWK processing of individual sheets.

# Invoke R command line subshell to retrieve names from .xlsx file sheets
# and redirect standard output to static file with lines for each sheet name.
( R --slave << EOF

  library(openxlsx)
  library(here)
  
  setwd(here::here())  # Ensure working directory set to project root
  
  sheetnames <- 
    openxlsx::getSheetNames("data/1.specimens/Phys_species_totals.xlsx")

  cat(sheetnames, sep = "\n")

EOF
) > Appendix/appendix_sheetnames.txt

# Write new file containing only sheet names matching substring "P. ".
grep "P. *" Appendix/appendix_sheetnames.txt > \
  Appendix/appendix_phys_sheetnames.txt

echo
  
# Read lines from the sheet names file where inter field separator (IFS) is a 
# newline character, executing the R script to write a .tsv file for each sheet.
# The file output by the R script is used as input for AWK formatted printing.
# AWK standard output is redirected to a new file with a formatted extension.
while IFS=\n read -r sheet; do
  echo $sheet
  appendix_filename=$(./Appendix/appendix_script.R "$sheet")
  new_file=${appendix_filename%.tsv}_formatted.tsv
  echo "Writing new file" $new_file
  echo 
  awk 'BEGIN  { FS="\t" } 
    { if ( NF > 10 ) {
      printf("%s %s %s\047 elevation; %sm elevation; %s %s; %s; \\textit\{%s %s\} \(%s\)\n",
        $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11) }
    }' $appendix_filename > $new_file
done < Appendix/appendix_phys_sheetnames.txt

# Return sum of specimens remaining in formatted .tsv files.
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

echo

