fig_scripts 	:= $(wildcard R/Fig*.R)
fig_outputs 	:= $(fig_scripts:R%=Figs%)
fig_pdf 	:= $(fig_outputs:%.R=%.pdf)
fig_html 	:= $(fig_outputs:%.R=%.png)

.PHONY: all md pdf html Figs

all: md pdf html Figs

md: README.Rmd
	Rscript -e 'rmarkdown::render(input = "README.Rmd", output_format = "github_document", output_file = "README.md")'

pdf: $(fig_pdf)
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'

html: $(fig_html)
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'

Figs: $(fig_pdf) $(fig_html)
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
	rm -fvr Figs/*
	Rscript -e "bookdown::clean_book(clean = TRUE)"
