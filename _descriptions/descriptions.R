# Verify installation of ThesisPackage data
if (!"ThesisPackage" %in% installed.packages()) {
  stop("Make sure ThesisPackage data are installed:",
       "https://github.com/jasonratcliff/ThesisPackage")
} else {
  library(ThesisPackage)
  library(dplyr)
  library(purrr)
  library(fs)
  library(usethis)
  library(whisker)
  library(bookdown)
} 

setwd(proj_get())  # Verify working directory set to ~/Thesis/

# Generate tibble of reviewed specimen identification counts.
specimen_counts <- herbarium_specimens %>%
  select(Taxon_a_posteriori) %>%
  add_count(Taxon_a_posteriori) %>%
  distinct() %>%
  arrange(n)


# Abbreviate taxon name for naming ascii-friendly files.
spp_file <- function(taxon) {
  stopifnot(is.character(taxon))
  gsub("Physaria", "P", taxon) %>%
    gsub("\\.", "", x = .) %>%
    gsub(" ", "-", x = .)
}

# Filter to Physaria sensu stricto specimens
specimen_counts %>%
  filter(
    !is.na(Taxon_a_posteriori),
    n > 20, 
    grepl(pattern = "Physaria [a-z]+", x = Taxon_a_posteriori)
  ) %>%
  pull("Taxon_a_posteriori") %>%
  purrr::walk(.x = ., function(taxon) {
    template_description <-
      base::readLines("_descriptions/_description.Rmd",
        n = -1L, encoding = "UTF-8", warn = FALSE
      )

    # Whisker {{Mustache}} implementation in R for logicless templating.
    # - https://github.com/edwindj/whisker
    # Inspired by usethis::use_template()
    template_whisker <-
      strsplit(whisker::whisker.render(
        template = template_description,
        data = list(taxon = taxon)
      ), split = "\n")[[1]]

    write_over(
      path = fs::path("_descriptions", spp_file(taxon), ext = "Rmd"),
      lines = template_whisker
    )
  })

# Generate bookdown from .Rmd whisker templates.
owd <- proj_get()
setwd("_descriptions/")

if (file_exists("_main.Rmd")) file_delete("_main.Rmd")
render_book(
  input = "index.Rmd",
  output_format = "bookdown::gitbook",
  params = list(
    species_data = specimen_counts
  )
)

# Clean .Rmd files
if (path_file(path_wd()) == "_descriptions") {
  list.files(
    pattern = "^P-",
    full.names = TRUE,
  ) %>%
    file_delete()
  setwd(owd)
}


