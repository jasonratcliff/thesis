# Verify installation of thesis data
if (!"thesis" %in% installed.packages()) {
  stop("Make sure thesis data are installed:",
       "https://github.com/jasonratcliff/thesis")
} else {
  library(thesis)
  library(dplyr)
  library(purrr)
  library(fs)
  library(usethis)
  library(whisker)
  library(bookdown)
}

setwd(proj_get())  # Verify working directory set to ~/thesis/

# Generate tibble of reviewed specimen identification counts.
specimen_counts <- thesis::herbarium_specimens %>%
  select(Taxon_a_posteriori) %>%
  add_count(Taxon_a_posteriori) %>%
  distinct() %>%
  arrange(n)


# Abbreviate taxon name for naming ascii-friendly files.
spp_file <- function(taxon) {
  stopifnot(is.character(taxon))
  gsub("Physaria", "P", taxon) %>%
    gsub("\\'", "", x = .) %>%
    gsub("\\.", "", x = .) %>%
    gsub(" ", "-", x = .)
}

# Filter to Physaria sensu stricto specimens
specimen_counts %>%
  filter(
    !is.na(Taxon_a_posteriori),
    n > 20,
    grepl(pattern = "Physaria '?[a-z]+'?", x = Taxon_a_posteriori)
  ) %>%
  pull(var = "Taxon_a_posteriori") %>%
  purrr::walk(.x = ., function(taxon) {
    template_description <-
      base::readLines(con = "data-raw/descriptions/_description.Rmd",
        n = -1L, encoding = "UTF-8", warn = FALSE
      )

    chunk <- gsub(pattern = "Physaria", replacement = "P", x = taxon) %>%
      gsub(pattern = " ", replacement = "", x = .) %>%
      gsub(pattern = "\\'", replacement = "", x = .)

    # Whisker {{Mustache}} implementation in R for logicless templating.
    # - https://github.com/edwindj/whisker
    # Inspired by usethis::use_template()
    template_whisker <-
      strsplit(whisker::whisker.render(
        template = template_description,
        data = list(taxon = taxon, chunk = chunk)
      ), split = "\n")[[1]]

    # print(fs::path("_descriptions", spp_file(taxon), ext = "Rmd"))
    write_over(
      path = fs::path("data-raw/descriptions", spp_file(taxon), ext = "Rmd"),
      lines = template_whisker
    )
  })

# Generate bookdown from .Rmd whisker templates.
owd <- proj_get()
setwd("data-raw/descriptions")

if (file_exists("_main.Rmd")) file_delete("_main.Rmd")
clean_book(clean = TRUE)
render_book(
  input = "index.Rmd",
  output_format = "bookdown::gitbook"
)

# Clean .Rmd files
if (path_file(path_wd()) == "descriptions") {
  list.files(
    pattern = "^P-",
    full.names = TRUE,
  ) %>%
    file_delete()
  setwd(owd)
}
