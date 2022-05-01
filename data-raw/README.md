data-raw README
================

# Specimen Data

Records of voucher specimens on loan from lending herbaria (RM, NY, MO,
F, ISTC, MONTU, MONT, RSA-POM, UC, UTC, GH, US, CAS, IDS) were compiled
into an excel file,
[`specimens.xlsx`](https://github.com/jasonratcliff/thesis/blob/master/inst/extdata/specimens.xlsx).
Data recorded from vouchers include locality, identification history,
collector, collection number, date, institution, geographic coordinates,
elevation, any ecological descriptions, and measurements of continuous
and discrete specimen traits. In total, 1722 unique collections were
reviewed.

``` r
# Path to installed file with specimens annotations.
system.file("extdata/specimens.xlsx", package = "thesis") %>%
  stringr::str_extract(string = ., pattern = "extdata.+")
```

    ## [1] "extdata/specimens.xlsx"

The
[`herbarium_specimens.R`](https://github.com/jasonratcliff/thesis/blob/master/data-raw/specimens/herbarium_specimens.R)
script creates an *.Rda* from `specimens.xlsx`, formatting specimen data
for downstream phylogenetic, distribution, and morphological analyses to
define a tibble `herbarium_specimens` in the `thesis` namespace.

``` r
thesis::herbarium_specimens
```

    ## # A tibble: 1,727 × 66
    ##    excel_sheet   prior_id      prior_1 prior_2 prior_3
    ##    <chr>         <chr>         <chr>   <chr>   <chr>  
    ##  1 P. acutifolia Physaria did… Physar… Physar… <NA>   
    ##  2 P. acutifolia Physaria acu… Physar… Physar… <NA>   
    ##  3 P. acutifolia Physaria did… Physar… <NA>    <NA>   
    ##  4 P. acutifolia Physaria acu… Physar… Physar… <NA>   
    ##  5 P. acutifolia Physaria did… Physar… <NA>    <NA>   
    ##  6 P. acutifolia Physaria acu… Physar… <NA>    <NA>   
    ##  7 P. acutifolia Physaria acu… Physar… Physar… <NA>   
    ##  8 P. acutifolia Physaria acu… Physar… Physar… Physar…
    ##  9 P. acutifolia Physaria did… Physar… Physar… <NA>   
    ## 10 P. acutifolia Physaria bra… Physar… Physar… <NA>   
    ## # … with 1,717 more rows, and 61 more variables:
    ## #   prior_4 <chr>, Taxon <chr>,
    ## #   Taxon_a_posteriori <chr>, Collector <chr>,
    ## #   Collection_Number <chr>, Date <chr>,
    ## #   Date_parsed <date>, Date_md <date>,
    ## #   Herbarium <chr>, State <chr>, County <chr>,
    ## #   Location <chr>, Longitude <dbl>, …

Briefly, the script does the following:

-   Read data into R using `readxl` ([Wickham and Bryan
    2019](#ref-R-readxl))
    -   Filter records with `dplyr` ([Wickham et al.
        2020](#ref-R-dplyr)) to annotations with:
        -   *Physaria*
        -   *Lesquerella*
        -   Brassicaceae
    -   Standardize dates with `lubridate` ([Spinu, Grolemund, and
        Wickham 2020](#ref-R-lubridate))
        -   New variable `Date_md` with month and day for phenology
        -   Log records mismatched with the date format `YYYY-MM-DD`
-   Split comma-separated prior annotations as separate variables
    -   Identification agreements denoted by “!” are substituted with ID
    -   Extract most recent annotations into a new column
        -   Replace recent annotations by synonym according to
            [O’Kane](#ref-OKane2010) ([2010](#ref-OKane2010))
-   Parse elevation data
    -   Cast as numeric and remove special characters
    -   Combine variables with m / ft. elevation data
    -   Split minimum and maximum values and convert m to ft.
-   Combine DNA specimen metadata from `specimens/dna_specimens.csv`
    -   Create *dna\_specimens.Rda* with tibble in the `thesis`
        namespace

# DNA Map

<div class="figure">

<img src="mapping/map-dna.png" alt="Sampled DNA Specimens. Distribution of specimens with prior identifications and locations of DNA samples." width="750px" />
<p class="caption">
Sampled DNA Specimens. Distribution of specimens with prior
identifications and locations of DNA samples.
</p>

</div>

# DNA Sequence Analysis

## Fasta Concatenation

Multi-FASTA files for sequenced loci were assembled from sequence
chromatograms and deposited in the
[`inst/extdata/FASTA`](https://github.com/jasonratcliff/thesis/blob/master/inst/extdata/FASTA)
subdirectory. Here, `FASTA` headers are formatted with two fields
`accession` and `locus` following “&gt;.” A single whitespace separates
the sample accession from the name of the locus, for example:
`>PACUT_48 rITS` or `>PACUT_12821 rps`. Each `FASTA` file contains
sequences sampled from a single genetic locus, where the locus is
indicated in the second field of the `FASTA` headers.

``` r
# Assign list of raw FASTA files from installed external package data.
list.files(
  path = system.file("extdata/FASTA", package = "thesis"),
  full.names = TRUE
  ) %>%
  stringr::str_extract(string = ., pattern = "extdata.+")
```

    ## [1] "extdata/FASTA/rITS-combined_raw.fasta"
    ## [2] "extdata/FASTA/rps-combined_raw.fasta" 
    ## [3] "extdata/FASTA/ycf1-combined_raw.fasta"

The `Biostrings` package ([Pagès et al. 2020](#ref-R-Biostrings)) was
used to read in `FASTA` files as `DNAStringSet` objects, an S4 class
inheriting the `XString` subclass. Briefly, a list is assigned with
elements containing a single `DNAStringSet` read from each `FASTA` file.
The intersecting set of sequence headers among all `FASTA` files is
identified from the extracted `DNAStringSet` names attributes. New
subsets for each locus were indexed by the set of specimens sequenced
for all loci. Untrimmed, single locus `FASTA` files from
[`inst/extdata/FASTA`]() and `FASTA` files filtered to common specimens
in
[`data-raw/sequencing/sequencing/2.subset-fastas`](https://github.com/jasonratcliff/thesis/tree/master/data-raw/sequencing/2.subset-fastas)
were aligned using MAFFT version 7.306B ([Katoh and Standley
2013](#ref-Katoh2013)). The G-INS-i alignment ([Katoh et al.
2005](#ref-Katoh2005)) with iterative refinement and 1PAM / k=2
nucleotide scoring matrix were set as alignment parameters. Aligned
`FASTA` files were deposited in the
[`data-raw/sequencing/3.alignments-single/`](https://github.com/jasonratcliff/thesis/tree/master/data-raw/sequencing/3.alignments-single)
and
[`data-raw/sequencing/3.alignments-subset/`](https://github.com/jasonratcliff/thesis/tree/master/data-raw/sequencing/3.alignments-subset)
subdirectories. For the subset of samples with DNA sequence data from
all three sample loci, a concatenated multi-FASTA file was compiled
using the
[`data-raw/sequencing/fasta-concat.R`](https://github.com/jasonratcliff/thesis/blob/master/data-raw/sequencing/fasta-concat.R)
Rscript, written to
[`data-raw/sequencing/3.multi-locus/`](https://github.com/jasonratcliff/thesis/tree/master/data-raw/sequencing/3.multi-locus).

<div id="htmlwidget-ea1fbfc004dcf4dd7387" style="width:672px;height:480px;" class="DiagrammeR html-widget"></div>
<script type="application/json" data-for="htmlwidget-ea1fbfc004dcf4dd7387">{"x":{"diagram":"\ngraph TD\n  %% Sequencing Workflow\n  S1[\"inst/extdata/FASTA\"]-->S2(fasta-subsets.R)\n  S2-->S3[\"sequencing/2.subset-fastas/\"]\n  S1-->S4{MAAFT}\n  S3-->S4\n  S4-->S5[\"sequencing/3.alignments-single/\"]\n  S4-->S6[\"sequencing/3.alignments-subset/\"]\n  S6-->S7(fasta-concat.R)\n  S7-->S8[\"sequencing/3.multi-locus/\"]\n"},"evals":[],"jsHooks":[]}</script>

## MEGA / MrBayes

Distance matrices were calculated from the full single-locus and subset
multi-locus alignment FASTA files to identify identical DNA sequences
using MEGA version 10.1.8 ([Kumar et al. 2018](#ref-Kumar2018);
[Stecher, Tamura, and Kumar 2020](#ref-Stecher2020)). Pairwise distances
were calculated by p-distance treating gaps as pairwise deletions.
Distance matrices and FASTA (*.fas*) files with combined headers for
duplicate DNA sequences were deposited in
[Bayes/MEGA/](https://github.com/jasonratcliff/thesis/tree/master/data-raw/Bayes/MEGA).
Non-identical single locus (rITS, *rps*, *ycf1*) and concatenated
multi-locus FASTA files were used as input for jModelTest ([Posada
2008](#ref-Posada2008)) to assess evolutionary model fit. Lastly,
alignment gaps were coded using 2matrix ([Salinas and Little
2014](#ref-Salinas2014)), a Perl script that implements “simple indel
coding” as described by [Simmons and Ochoterena](#ref-Simmons2000)
([2000](#ref-Simmons2000)). Partitioned NEXUS (*.nex*) files with
defined command blocks were deposited in
[Bayes/Runs/](https://github.com/jasonratcliff/thesis/tree/master/data-raw/Bayes/Runs).

<div id="htmlwidget-6f358baac00b048de77f" style="width:672px;height:480px;" class="DiagrammeR html-widget"></div>
<script type="application/json" data-for="htmlwidget-6f358baac00b048de77f">{"x":{"diagram":"\ngraph TD\n  S1[\"sequencing/3.alignments-single/\"]\n  S2[\"sequencing/3.multi-locus/\"]\n  %% MEGA files\n  S1-->D1{MEGA}\n  S2-->D1\n  D1-->D2(Distance matrices)\n  D1-->D3(De-duplicated multi-FASTAs)\n  D2-->D4[\"Bayes/MEGA/\"]\n  D3-->D4\n  %% jModelTest\n  D4-->J1{jModelTest}\n  J1-->J2[\"Bayes/jmodeltest/\"]\n  %% 2Matrix / Bayes Nexus\n  D4-->M1{2Matrix}\n  M1-->M2[\"Bayes/Runs/\"]\n  M2-->M4{MrBayes}\n  J2-->M4\n"},"evals":[],"jsHooks":[]}</script>

## BEAST

The `Biostrings` and `ape` ([Paradis and Schliep 2019](#ref-R-ape))
packages were used to read in the subset of specimen alignments from the
concatenated analysis and write `NEXUS` formatted alignment files. For
all three loci (rITS, *rps*, *ycf1*), alignments were imported into
`BEAUti` v2.6.3 ([Bouckaert et al. 2019](#ref-Bouckaert2019)) to
configure BEAST *.xml* files.

<div id="htmlwidget-0f7ca1b0bc94ffbe207a" style="width:672px;height:480px;" class="DiagrammeR html-widget"></div>
<script type="application/json" data-for="htmlwidget-0f7ca1b0bc94ffbe207a">{"x":{"diagram":"\ngraph TD\n  S1[\"sequencing/3.alignments-subset/\"]\n  S1-->B1(data-raw/BEAST/beast.R)\n  B1-->B2[\"BEAST/NEXUS/\"]\n  B2-->B3(BEAUTi)\n  B3-->B4(data-raw/BEAST/*.xml)\n"},"evals":[],"jsHooks":[]}</script>

BEAST v2.6.3 ([Bouckaert et al. 2019](#ref-Bouckaert2019)) results from
3 independed runs of 50M states were combined with `LogCombiner` v2.6.3
(*ibid.*) with 10,000 resampled states. A maximum clade credibility tree
was summarized using `TreeAnnotator` v2.6.3 (*ibid.*) and visualized
using the `ggplot2` ([Wickham 2020](#ref-R-ggplot2)) extension `ggtree`
([Yu et al. 2017](#ref-R-ggtree)).

<div id="htmlwidget-a86a7b562d0e36f2d1b7" style="width:672px;height:480px;" class="DiagrammeR html-widget"></div>
<script type="application/json" data-for="htmlwidget-a86a7b562d0e36f2d1b7">{"x":{"diagram":"\ngraph TD\n  B1(data-raw/BEAST/*.xml)-->B2\n  subgraph \"\"\n    B2[BEAST2]\n    B2-->B3[LogCombiner]\n    B3-->B4[TreeAnnotator]\n  end\n  B4-->B5(/inst/extdata/BEAST/*.mcc)\n  B5-->B6[R `ggtree`]\n"},"evals":[],"jsHooks":[]}</script>

## Evolutionary Models

-   <https://github.com/ddarriba/modeltest/wiki/Models-of-Evolution>

| Locus | Partition | jModelTest | MEGA | Par. Finder | MrBayes | BEAST |
|:------|:----------|:-----------|:-----|:------------|:--------|:------|
| rITS  | 1-659     | TrNef      | HKY  | K80         | nst=1   | TN93  |
| rps   | 660-1605  | F81        | T92  | F81 + i     | nst=1   | JC69  |
| ycf1  | 1606-2162 | F81        | T92  | F81 + i     | nst=1   | JC69  |

## FASTA Summaries

Define a function to read *.fasta* extension files from a given file
path as `DNAStringSet` objects, then calculate the number of unique
sequence headers and minimum / maxium sequence lengths.

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

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Bouckaert2019" class="csl-entry">

Bouckaert, Remco R, Timothy G Vaughan, Joëlle Barido-SottaniI, Sebastián
Duchêne, Mathieu Fourment, Alexandra Gavryushkina, Joseph Heled, et al.
2019. “BEAST 2.5: An Advanced Software Platform for Bayesian
Evolutionary Analysis.” *PLoS Computational Biology* 15 (4): e1006650.
<https://doi.org/10.1371/journal.pcbi.1006650>.

</div>

<div id="ref-Katoh2005" class="csl-entry">

Katoh, Kazutaka, Kei-ichi Kuma, Hiroyuki Toh, and Takashi Miyata. 2005.
“MAFFT Version 5: Improvement in Accuracy of Multiple Sequence
Alignment.” *Nucleic Acids Research* 33 (2): 511–18.

</div>

<div id="ref-Katoh2013" class="csl-entry">

Katoh, Kazutaka, and Daron M Standley. 2013. “MAFFT Multiple Sequence
Alignment Software Version 7: Improvements in Performance and
Usability.” *Molecular Biology and Evolution* 30 (4): 772–80.

</div>

<div id="ref-Kumar2018" class="csl-entry">

Kumar, Sudhir, Glen Stecher, Michael Li, Christina Knyaz, and Koichiro
Tamura. 2018. “MEGA X: Molecular Evolutionary Genetics Analysis Across
Computing Platforms.” *Molecular Biology and Evolution* 35 (6): 1547–49.

</div>

<div id="ref-OKane2010" class="csl-entry">

O’Kane, Steve L, Jr. 2010. “*Physaria*.” In *<span
class="nocase">Magnoliophya: Salicaceae to Brassicaceae</span>*, edited
by Flora of North America editorial committee, 1st ed., 7:616–65. <span
class="nocase">Flora of North America North of Mexico</span>. New York,
(NY): Oxford Univ. Press.

</div>

<div id="ref-R-Biostrings" class="csl-entry">

Pagès, H., P. Aboyoun, R. Gentleman, and S. DebRoy. 2020. *Biostrings:
Efficient Manipulation of Biological Strings*. Manual.

</div>

<div id="ref-R-ape" class="csl-entry">

Paradis, E., and K. Schliep. 2019. “Ape 5.0: An Environment for Modern
Phylogenetics and Evolutionary Analyses in R.” *Bioinformatics* 35:
526–28.

</div>

<div id="ref-Posada2008" class="csl-entry">

Posada, David. 2008. “<span class="nocase">jModelTest</span>:
Phylogenetic Model Averaging.” *Molecular Biology and Evolution* 25 (7):
1253–56.

</div>

<div id="ref-Salinas2014" class="csl-entry">

Salinas, Nelson R, and Damon P Little. 2014. “2Matrix: A Utility for
Indel Coding and Phylogenetic Matrix Concatenation.” *Applications in
Plant Science* 2 (1): 1300083. <https://doi.org/10.3732/apps.1300083>.

</div>

<div id="ref-Simmons2000" class="csl-entry">

Simmons, Mark P, and Helga Ochoterena. 2000. “Gaps as Characters in
Sequence-Based Phylogenetic Analyses.” *Systematic Biology* 49 (2):
369–81.

</div>

<div id="ref-R-lubridate" class="csl-entry">

Spinu, Vitalie, Garrett Grolemund, and Hadley Wickham. 2020. *<span
class="nocase">lubridate</span>: Make Dealing with Dates a Little
Easier*. Manual. <https://CRAN.R-project.org/package=lubridate>.

</div>

<div id="ref-Stecher2020" class="csl-entry">

Stecher, Glen, Koichiro Tamura, and Sudhir Kumar. 2020. “Molecular
Evolutionary Genetics Analysis (MEGA) for <span
class="nocase">macOS</span>.” *Molecular Biology and Evolution*.
<https://doi.org/10.1093/molbev/msz312>.

</div>

<div id="ref-R-ggplot2" class="csl-entry">

Wickham, Hadley. 2020. *<span class="nocase">ggplot2</span>: Elegant
Graphics for Data Analysis*. 3rd ed. New York, (NY): Springer-Verlag.
<https://ggplot2-book.org/>.

</div>

<div id="ref-R-readxl" class="csl-entry">

Wickham, Hadley, and Jennifer Bryan. 2019. *<span
class="nocase">readxl</span>: Read Excel Files*. Manual.
<https://CRAN.R-project.org/package=readxl>.

</div>

<div id="ref-R-dplyr" class="csl-entry">

Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2020.
*<span class="nocase">dplyr</span>: A Grammar of Data Manipulation*.
Manual. <https://CRAN.R-project.org/package=dplyr>.

</div>

<div id="ref-R-ggtree" class="csl-entry">

Yu, Guangchuang, David Smith, Huachen Zhu, Yi Guan, and Tommy Tsan-Yuk
Lam. 2017. “<span class="nocase">ggtree</span>: An R Package for
Visualization and Annotation of Phylogenetic Trees with Their Covariates
and Other Associated Data.” *Methods in Ecology and Evolution* 8 (1):
28–36. <https://doi.org/10.1111/2041-210X.12628>.

</div>

</div>
