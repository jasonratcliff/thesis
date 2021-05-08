.PHONY: all

all: pdf html

pdf: $(fig_pdf)
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'

html: $(figs_png)
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'
