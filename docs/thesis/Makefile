# inst/manuscript/
.PHONY: all

all: pdf html
	
pdf: $(fig_pdf)
	#rm -fr _book
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'

html: $(figs_png)
	#rm -fr _gitbook
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook", output_dir = "_gitbook")'
