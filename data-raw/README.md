data-raw README
================

# Specimen Data

Records of voucher specimens on loan from lending herbaria (RM, NY, MO,
F, ISTC, MONTU, MONT, RSA-POM, UC, UTC, GH, US, CAS, IDS) were compiled
into the excel file `Phys_species_totals.xlsx`. Information recorded
from the vouchers included the identification history, collector,
collection number, date, institution, locality, geographic coordinates,
elevation, ecological description, and measurements of continuous and
discrete specimen traits. In total, this project considered 1635 unique
collections dated as early as 1844 (Fremont’s 2nd Expedition).

    data-raw/Phys_species_totals.xlsx

The `herbarium_specimens.R` script creates an *.Rda* file from
`data-raw/Phys_species_totals.xlsx`, formatting specimen data for
downstream phylogenetic, distribution, and morphological analyses to
define a tibble `herbarium_specimens` in the `ThesisPackage` namespace.
Briefly, the script does the following:

  - Read data into R using `readxl` (Wickham and Bryan 2019)
      - Filter records with `dplyr` (Wickham et al. 2020) to annotations
        with:
          - *Physaria*
          - *Lesquerella*
          - Brassicaceae
      - Standardize dates and create a new vector with month and day
          - Log records mismatched with the date format `YYYY-MM-DD`
  - Split comma-separated prior annotations as separate variables
      - Identification agreements denoted by “\!” are replaced with
        corresponding text
      - Extract most recent annotations into a new column
          - Replace recent annotations by synonym according to O’Kane
            (2010)
  - Parse elevation data
      - Cast as numeric and remove special characters
      - Combine variables with m / ft. elevation data
      - Split minimum and maximum values and convert m to ft.
  - Combine DNA specimen metadata from `data-raw/dna_specimens.csv`
      - Create an *.Rda* file to define a tibble `dna_specimens` in the
        `ThesisPackage`
namespace

# DNA Map

<div class="figure">

<img src="README_files/figure-gfm/dnaSpecimenMap-1.png" alt="See data-raw/mapping/map-dna.pdf for higher resolution image." width="750px" />

<p class="caption">

See data-raw/mapping/map-dna.pdf for higher resolution image.

</p>

</div>

# DNA Sequences

## Fasta Subset

Multi-FASTA files for sequenced loci were assembled from sequence
chromatograms and deposited in the `data-raw/sequencing/1.raw-fastas/`
project subdirectory. Here, FASTA headers are formatted with two fields
`accession` and `locus` following “\>”. A single whitespace separates
the sample accession from the name of the locus, for example: `>PACUT_48
rITS` or `>PACUT_12821 rps`. A given FASTA file contains sequences
sampled from a single genetic locus and all FASTA headers in the file
contain the same locus name in the second field.

``` r
# Assign list of raw FASTA files from project subdirectory.
list.files(path = "data-raw/1.raw-fastas",
              pattern = ".fasta$", full.names = TRUE)
```

    ## character(0)

The Biostrings package (Pagès et al. 2019) was used to read in FASTA
files as DNAStringSets, R S4 objects of the XString subclass. A list is
assigned from with elements containing DNAStringSets read from a given
FASTA file. The intersecting set of sequence headers among the multiple
FASTA files was identified using the extracted DNAStringSet names
attributes. New subsets for each locus were indexed by the set of
specimens sequenced for all loci. Untrimmed, single locus FASTA files
from `data-raw/sequencing/1.raw-fastas` and FASTA files filtered to
common specimens in `data-raw/sequencing/2.subset-fastas` were aligned
using MAFFT. The G-INS-i alignment strategy with iterative refinement
and 1PAM / k=2 nucleotide scoring matrix were set sat as the alignment
parameters.

## FASTA Summaries

### Raw FASTAs

<table>

<thead>

<tr>

<th style="text-align:left;">

Locus

</th>

<th style="text-align:right;">

Sequences

</th>

<th style="text-align:right;">

Min Length

</th>

<th style="text-align:right;">

Max Length

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

rITS

</td>

<td style="text-align:right;">

72

</td>

<td style="text-align:right;">

581

</td>

<td style="text-align:right;">

583

</td>

</tr>

<tr>

<td style="text-align:left;">

rps

</td>

<td style="text-align:right;">

66

</td>

<td style="text-align:right;">

802

</td>

<td style="text-align:right;">

852

</td>

</tr>

<tr>

<td style="text-align:left;">

ycf1

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

511

</td>

<td style="text-align:right;">

524

</td>

</tr>

</tbody>

</table>

### Subset FASTAs

<table>

<thead>

<tr>

<th style="text-align:left;">

Locus

</th>

<th style="text-align:right;">

Sequences

</th>

<th style="text-align:right;">

Min Length

</th>

<th style="text-align:right;">

Max Length

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

rITS

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

581

</td>

<td style="text-align:right;">

583

</td>

</tr>

<tr>

<td style="text-align:left;">

rps

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

802

</td>

<td style="text-align:right;">

852

</td>

</tr>

<tr>

<td style="text-align:left;">

ycf1

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

511

</td>

<td style="text-align:right;">

524

</td>

</tr>

</tbody>

</table>

### Aligned (untrimmed)

<table>

<thead>

<tr>

<th style="text-align:left;">

Locus

</th>

<th style="text-align:right;">

Sequences

</th>

<th style="text-align:right;">

Min Length

</th>

<th style="text-align:right;">

Max Length

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

rITS

</td>

<td style="text-align:right;">

72

</td>

<td style="text-align:right;">

586

</td>

<td style="text-align:right;">

586

</td>

</tr>

<tr>

<td style="text-align:left;">

rps

</td>

<td style="text-align:right;">

66

</td>

<td style="text-align:right;">

889

</td>

<td style="text-align:right;">

889

</td>

</tr>

<tr>

<td style="text-align:left;">

ycf1

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

524

</td>

<td style="text-align:right;">

524

</td>

</tr>

</tbody>

</table>

### Aligned (trimmed)

<table>

<thead>

<tr>

<th style="text-align:left;">

Locus

</th>

<th style="text-align:right;">

Sequences

</th>

<th style="text-align:right;">

Min Length

</th>

<th style="text-align:right;">

Max Length

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

rITS

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

586

</td>

<td style="text-align:right;">

586

</td>

</tr>

<tr>

<td style="text-align:left;">

rps

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

889

</td>

<td style="text-align:right;">

889

</td>

</tr>

<tr>

<td style="text-align:left;">

ycf1

</td>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

524

</td>

<td style="text-align:right;">

524

</td>

</tr>

</tbody>

</table>

# References

Bache SM, Wickham H. 2014. magrittr: A forward-pipe operator for R. R
package version 1.5. <https://CRAN.R-project.org/package=magrittr>

Grolemund G, Wickham H. 2011. Dates and times made easy with lubridate.
Journal of Statistical Software. 40(3):1-25.

Henry L, Wickham H. 2019. purrr: Functional programming tools. R package
version 0.3.0. <https://CRAN.R-project.org/package=purrr>

Müller K, Wickham H. 2019. tibble: Simple data frames. R package version
2.0.1. <https://CRAN.R-project.org/package=tibble>

Müller K. 2017. here: A simpler way to find your files. R package
version 0.1. <https://CRAN.R-project.org/package=here>

O’Kane SL. 2010. *Physaria*. In: Flora of North America editorial
committee. Flora of North America north of Mexico. Volume 7. Salicaceae
to Brassicaceae. New York (NY): Oxford Univ. Press. p. 616-665. Payson
EB. 1918. Notes on certain Cruciferae. Annals of the Missouri Botanical
Garden. 5(2):143-147.

Pagès H, Aboyoun P, Gentleman R, DebRoy S. 2019. Biostrings: Efficient
manipulation of biological strings. R package version 2.50.2.
<https://www.bioconductor.org/packages/release/bioc/html/Biostrings.html>

Wickham H, Bryan J. 2019. readxl: Read excel files. R package version
1.3.1. <https://CRAN.R-project.org/package=readxl>

Wickham H, Francois R, Henry L, Müller K. 2020. dplyr: a grammar of data
manipulation. R package version 0.8.5.
<https://CRAN.R-project.org/package=dplyr>

Wickham H. 2019. stringr: Simple, consistent wrappers for common string
operations. R package version 1.4.0.
<https://CRAN.R-project.org/package=stringr>

Xie Y. 2018. knitr: A general-purpose package for dynamic report
generation in R. R package version 1.21.
<https://cran.r-project.org/package=knitr>

Wickham H. 2011. The split-apply-combine strategy for data analysis.
Journal of Statistical Software. 40(1):1-29.
