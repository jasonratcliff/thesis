fig_scripts 	:= $(wildcard R/Fig*.R)
fig_outputs 	:= $(fig_scripts:R%=Figs%)
fig_pdf 	:= $(fig_outputs:%.R=%.pdf)
fig_html 	:= $(fig_outputs:%.R=%.png)

.PHONY: all

all: md Figs pdf html

md: README.Rmd
	Rscript -e 'rmarkdown::render(input = "README.Rmd", output_format = "github_document", output_file = "README.md")'

Figs: $(fig_pdf) $(fig_html)
	if [ ! -d Figs ]; then\
		mkdir -v Figs;\
	fi

Figs/Fig%.pdf: R/Fig%.R 
	Rscript $(<D)/$(<F)

Figs/Fig%.png: R/Fig%.R
	Rscript $(<D)/$(<F)

pdf: $(fig_pdf)
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")'

html: $(fig_html)
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'

word:
	Rscript scripts/render_word.R

descriptions:
	Rscript _descriptions/descriptions.R

clean_aux:
	rm -fvr *.aux
	rm -fvr TeX/*.aux

clean: clean_aux
	rm -f README.html
	rm -ifvr Figs/*
	Rscript -e "bookdown::clean_book(clean = TRUE)"
