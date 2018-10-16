#!/bin/bash
#
# AWK script to process FASTA files by comparing sequence header differences
# for multi-FASTA concatenation.  Directory <D> should contain FASTA files
# at the main directory level.
#
# The following optional arguments can be passed:
#   "-D" Project directory containing FASTA files
#   "-N" Optional new directory to copy FASTA files to for manual edits
#   "-f" Optional string to append to loci in fasta files.

# Use builtin function GETOPTS to parse shell variables.
# http://man7.org/linux/man-pages/man1/getopts.1p.html
while getopts "D:N:f:" opt ; do
  case $opt in
    D)  # Directory containing FASTA files
      directory="$OPTARG"
      ;;
    N)  # New directory to copy FASTA files for manual edits
      new_directory="$OPTARG"
      ;;
    f)  # String to append to FASTA filename
      filename="$OPTARG"
      ;;
    ?) # Warning for non-matching option flags pass as arguments
      echo "Invalid option: -$OPTARG"
      echo "Usage: fasta_diff.sh [-D] <main directory> [-N] <new directory> [-f] <append filename suffix>"
      ;;
  esac
done

# Shifts away option arguments denoted by colon (i.e. "D:"),
# leaving your path as shell variable $1, and positional arguments as $@.
shift $((OPTIND - 1))

# Check if input argument [-D] is a directory AND ends in character "/".
dir_end=${directory:${#directory}-1}
if [ -d "$directory" ] && [ $dir_end  = "/" ] ; then
  # Create subdirectories for sequence headers and header diffs.
  if [ ! -d ${directory}seq_headers/ ] ; then  
    mkdir ${directory}seq_headers/ && mkdir ${directory}seq_diffs/
  else 
    echo  # notification that directory already exists ...
    echo "Directory" ${directory}seq_headers/ "already exists !"
  fi
# Exit program if argument [-D] is not a directory ending in "/".
else  
  echo "Exiting..." $directory "is not a directory..."; exit
fi

# Iterate through FASTA files in directory [-D] main level.
for fasta in ${directory}*.fasta; do
  echo $fasta  # full file path to fasta from main level
  
  # Slice the loci substring following variable substitution syntaxes.
  loci=${fasta##${directory}}
  loci=${loci%-*}
  echo '  Processing loci' $loci "..."
  
  # Extract headers from the FASTA file using AWK.
  headers=${directory}seq_headers/${loci}
  awk '/^>/ {print $1}; END {print NR}' $fasta > ${headers}-seqs.txt
 
  # Print number of FASTA headers.
  awk 'END {printf("\tFASTA headers:%s\n", NR)}' ${headers}-seqs.txt

  # Sort the sequence headers and redirect the output to a new file.
  sort ${headers}-seqs.txt > ${headers}-sorted_seqs.txt
  
done

# Create indexed array of sorted FASTA sequence header files.
sorted_fastas=(${directory}seq_headers/*sorted*)

# Iterate through array elements prior to the final element.
for ((i=0; i < (( ${#sorted_fastas[@]} - 1 )); i++)); do 

  # Nested loop for pairwise sequence header comparisions.
  for j in $(seq $i $(( ${#sorted_fastas[@]} - 2 ))); do
  
    # Slice variables from loci names for sequence header diff files.
    loci1=${sorted_fastas[i]##*/seq_headers/}
    loci1=${loci1%-sorted*}
    loci2=${sorted_fastas[$(( $j + 1 ))]##*/seq_headers/}
    loci2=${loci2%-sorted*}

    # Create file with pairwise diff calculations of sequence headers.
    # diff command flag "-b" ignores white space.
    diff -b ${sorted_fastas[i]} ${sorted_fastas[$(( $j + 1 ))]} > \
    ${directory}seq_diffs/diffs-${loci1}_${loci2}.txt
  
  done
  
done

# Option to create a new directory and copy FASTA files for manual edits.
if [ -n "$new_directory" ]; then

  new_dir_end=${new_directory:${#new_directory}-1}
  
  if [ -d "$new_directory" ]; then  # check directory existence
      echo "Directory" $new_directory "already exists !" 
  fi
  if [ "$new_dir_end"  != "/" ]; then  # check final directory character
      echo "Exiting..." $new_directory "is not a valid directory name."; 
      exit
  fi
  # Check if input argument [-N] exists AND ends in character "/".
  if [ ! -d "$new_directory" ] && [ "$new_dir_end"  = "/" ]; then 
    mkdir ${new_directory}
  fi
  
  # Copy multi-FASTA files into a second directory for manual trimming.
  cp ${directory}*fasta $new_directory

  # Optional string to append to filenames after copying to new directory.
  if [ -n "$filename" ]; then
    # Slice last six characters from filename string to verify extension.
    fasta_ext=${filename:${#filename}-6}  
    if [ "$fasta_ext" = ".fasta" ]; then
      filename=${filename%.fasta}  # remove ".fasta" extension if present
    fi
    # Rename the copied raw files prior to manual removing extra sequences.
    for fasta in ${new_directory}*fasta; do
      loci_file=${fasta##${new_directory}}
      loci=${loci_file%-*}
      mv $fasta ${new_directory}${loci}-${filename}.fasta
    done
  fi
fi
