#!/bin/bash
# Script to search filenames of digikam photos in digikam directory: 
digi_root=/Users/jason/jPhoto/  # set absolute path from root

# Designate specific directories to search for files.
# Define an array with relative paths of directories to search.
digi_paths=("Physaria 2018" "Physaria Unsorted" "Utah")

# Define string of filename to find by positional argument.
digi_photo=$1
printf "\nSearching for filename string: '%s'\n" $digi_photo

# Iterate through string array of paths to search digikam directory file names.
IFS=""  # set interfield seperator for whitespace
for digi_path in "${digi_paths[@]}"; do
  digi_search=${digi_root}${digi_path}  # digikam search directory element
  printf "\n   Digikam directory: %s\n\n" $digi_search
  # Find files and remove prefix to clean up results.
  find $digi_search -name "*${digi_photo}*" -print | while read photo; do
    printf "\t%s\n" ${photo#*jPhoto/}
  done
done
