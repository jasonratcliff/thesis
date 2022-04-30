
# Species Descriptions

The first approach to generating specimen descriptions from a template
*.Rmd* file involved the use of R markdown [parameterized
reports](https://bookdown.org/yihui/rmarkdown/parameterized-reports.html)
with species values used to filter data from
[`thesis`](https://github.com/jasonratcliff/thesis).
However, the reports are rendered as various [output
formats](https://rmarkdown.rstudio.com/lesson-9.html) which limited the
use of [`bookdown`](https://bookdown.org/yihui/bookdown/) for rendering
a report from multiple *.Rmd* input files. Instead, inspired by
[`usethis`](https://github.com/r-lib/usethis) templates,
[`whisker`](https://github.com/edwindj/whisker) logic-less [{{ mustache
}}](https://github.com/mustache/mustache.github.com) templating was used
to render *.Rmd* files for each species with \(n>20\) unique vouchers.
These files are rendered into a single `bookdown` document nested inside
the project subdirectory `_descriptions` following an example
[script](https://github.com/rstudio/bookdown/blob/master/inst/examples/_render.R)

``` r
# Render github document from README.md
rmarkdown::render(
  input = "_descriptions/README.md",
  output_format = "github_document",
  output_dir = "_descriptions"
)
```
