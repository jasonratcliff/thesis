rmarkdown::render(
  input = "data-raw/README.Rmd",
  output_format = "github_document",
  output_file = "data-raw/README.md",
  output_dir = "data-raw"
)

rmarkdown::render(
	input = "README.Rmd",
	output_format = "github_document",
	output_file = "README.md"
)

fs::file_delete(path = c("README.html", "data-raw/README.html"))
