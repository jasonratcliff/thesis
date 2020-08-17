# Appendix

To facilitate formatting appendix files for specimens reviewed, an Rscript
was used to read in a column subset from each sheetname in the
`specimens.xlsx` external data from
[`ThesisPackage`](https://github.com/jasonratcliff/ThesisPackage).
A *.tsv* file is written from entries without appendix completion
(missing values in variable `App.A`). $\LaTeX$ formatting is substituted
for collection and collection number values.

## digiKam Photo Organization

Herbarium voucher specimens were photographed and cataloged by locality.
The following name convention was used for Digikam files:
    
`<Collector>_<Collection Number>_<Herbarium Code(-Accession Number)>`

Where:

 - **Collector** is a character string of the botanist's last name
 - **Collection Number** is an integer for voucher specimen collection number
 - **Herbarium Code** is a two letter uppercase string denoting 
    + **-Accession Number** is a possible voucher-specific herbarium accession
 
To search the designated subdirectories, a string passed from the command line is wrapped in **\*** wildcards to recursively find the expression from subdirectory file names.  If found, the file name including the relative path from the respective subdirectory is printed.

### Search by Collector

From the project directory, execute the `digi_find.sh` script that accepts a single positional argument (i.e. `$1`) to represent the string passed to the bash `find` utility.   

```
Jasons-MacBook:Thesis jason$ ./Appendix/digi_find.sh Hayden

Searching for filename string: 'Hayden'

   Digikam directory: /Users/jason/jPhoto/Physaria 2018

	Physaria 2018/Uncertain Locality/P_didymocarpa/Hayden_sn_MO-3833625.tiff
	Physaria 2018/Wyoming/Big Horn/Unknown Locality/Hayden_sn_MO-3093900_v1.tiff
	Physaria 2018/Wyoming/Hot Springs/Unknown Locality/Hayden_sn_MO-137159.tiff
	Physaria 2018/Wyoming/Hot Springs/Unknown Locality/Hayden_sn_MO-1923237.tiff

   Digikam directory: /Users/jason/jPhoto/Physaria Unsorted

	Physaria Unsorted/WY, Unknown/Hayden_sn_MO-3833631.tiff

   Digikam directory: /Users/jason/jPhoto/Utah

```

### Search by Herbarium Accession

```
# Pass string for herbarium accession number
Jasons-MacBook:Thesis jason$ ./Appendix/digi_find.sh MO-3093900

Searching for filename string: 'MO-3093900'

   Digikam directory: /Users/jason/jPhoto/Physaria 2018

	Physaria 2018/Wyoming/Big Horn/Unknown Locality/Hayden_sn_MO-3093900_v1.tiff

   Digikam directory: /Users/jason/jPhoto/Physaria Unsorted


   Digikam directory: /Users/jason/jPhoto/Utah

```