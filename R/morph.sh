#!/bin/bash
# Input the directory containing discrete trait data files
# as an argument to the script

cd $1;

for trait in $(ls *.csv); do

# remove extension from filename
# using variable substitution
# '%' removes the matching suffix pattern
character=${trait%.csv}

# format the output to separate the data
echo -e "\n\n ## \t $character \t ## \n"

# use sed to clean the input
sed '

#  remove lines with missing data (i.e. NAs)
/NA/d

# substitute semicolons with commas globally
s/;/,/g

# remove commas at the end of lines
s/,$//

# remove whitespace at the end of lines
s/ *$//

# Pipe the cleaned data from file $trait into an AWK statement
' $trait | sort | awk '
BEGIN {FS = ", "}
    { for (i = 1; i <= NF; i += 1) 
           print $i;  
}' | sort | uniq -c | sort -rk 1;

done
