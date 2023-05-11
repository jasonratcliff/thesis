# Render PDF
bookdown::render_book(
  input = "docs/thesis",
  output_format = "bookdown::pdf_book",
  output_dir = "_book"
)

# Render HTML
bookdown::render_book(
  input = "docs/thesis",
  output_format = "bookdown::gitbook",
  output_dir = "_gitbook"
)
