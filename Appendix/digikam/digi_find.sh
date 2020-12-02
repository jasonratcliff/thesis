#!/bin/bash
set -e
set -u
set -o pipefail

# Script to search filenames of digikam photos in digikam directory.

digi_path=/Users/jasonratcliff/jPhoto/Physaria  # absolute path to photos

# Filename string to find photos by positional argument.
digi_photo=$1

printf "  Searching for filename string: '%s'\n" "${digi_photo}"

find "${digi_path}" -name "*${digi_photo}*" -print0 | xargs -0 -I{} \
  sh -c 'path=$1 ; name=${path#*jPhoto}; echo "\t" $name' -- {}

