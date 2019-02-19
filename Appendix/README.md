# Appendix

To facilitate formatting appendix files for specimens reviewed, a pipeline utilizing a `shell` wrapper script was established to call an `R` function for each tab in an excel file.  The *.tsv* files written by this function are processed with `AWK` to structure herbarium specimen data in a $\LaTeX$ appendix format.  These records were then reviewed and added to their respective *TeX/Thesis_A-specimen\*.tex* files. 

## $\LaTeX$ Formatting Pipeline

```
Jasons-MacBook:Thesis jason$ ./Appendix/appendix_script.sh

P. Remaining  # Excel sheetname
Writing new file Appendix/P.Remaining_appendix_formatted.tsv

P. didymocarpa
Writing new file Appendix/P.didymocarpa_appendix_formatted.tsv

P. brassicoides
Writing new file Appendix/P.brassicoides_appendix_formatted.tsv

P. vitulifera_CO
Writing new file Appendix/P.vitulifera_CO_appendix_formatted.tsv

P. NC_WY
Writing new file Appendix/P.NC_WY_appendix_formatted.tsv

P. acutifolia
Writing new file Appendix/P.acutifolia_appendix_formatted.tsv

P. integrifolia
Writing new file Appendix/P.integrifolia_appendix_formatted.tsv

P. saximontana
Writing new file Appendix/P.saximontana_appendix_formatted.tsv

P. chambersii
Writing new file Appendix/P.chambersii_appendix_formatted.tsv

P. vitulifera_WY
Writing new file Appendix/P.vitulifera_WY_appendix_formatted.tsv

P. eburniflora
Writing new file Appendix/P.eburniflora_appendix_formatted.tsv

P. Idaho
Writing new file Appendix/P.Idaho_appendix_formatted.tsv

P. DNA
Writing new file Appendix/P.DNA_appendix_formatted.tsv

P. others
Writing new file Appendix/P.others_appendix_formatted.tsv

262 specimens remaining.  # specimens with NA value in App.A column
```

## digikam Photos

Herbarium voucher specimens were photographed and cataloged by species and locality (i.e. State and County).  The *Phys_species_totals.xlsx* file was used to track which specimens had been imaged by marking an "x" in the **Imaged** column.  

### File Naming Convention

Digikam files are named as follows:
    
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