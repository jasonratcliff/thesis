#!/bin/bash

# Bash script to wrap appendx_script.R into AWK processing of individual sheets.

# Invoke R command line subshell to retrieve names from .xlsx file sheets
# and redirect standard output to static file with lines for each sheet name.
( R --slave << EOF

  library(openxlsx)

  sheetnames <- openxlsx::getSheetNames("Phys_species_totals.xlsx")

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

