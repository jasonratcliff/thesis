figs_R = $(wildcard R/Fig*.R)
figs_path = $(figs_R:R%=Figs%)
figs_pdf = $(figs_path:%.R=%.pdf)
figs_html = $(figs_path:%.R=%.png)

.PHONY: all md pdf html Figs

all: md pdf html Figs

md:
	Rscript -e 'rmarkdown::render(input = "README.Rmd", output_format = "github_document", output_file = "README.md")'

pdf: Figs $(figs_pdf)
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'

html: Figs $(figs_html)
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'

Figs:
	if [ ! -d Figs ]; then\
		mkdir -v Figs;\
	fi

Figs/Fig%.pdf: R/Fig%.R 
	Rscript $(<D)/$(<F)

Figs/Fig%.png: R/Fig%.R
	Rscript $(<D)/$(<F)

clean:
	rm -f README.html
	rm -fvr *.aux
	rm -fvr Figs/
	Rscript -e "bookdown::clean_book(clean = TRUE)"
