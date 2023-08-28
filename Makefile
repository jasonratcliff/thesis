.PHONY : all data

SPECIMENS	:=	$(wildcard data-raw/specimens/*.R)
RDATA		:=	$(patsubst data-raw/specimens/%.R, data/%.rda, $(SPECIMENS))

all: $(RDATA)

data: $(RDATA)

data/dna.rda : data-raw/specimens/dna.csv
data/seinet.rda : data-raw/seinet/occurrences.tab
data/vouchers.rda : data-raw/specimens/vouchers.xlsx

data/%.rda : data-raw/specimens/%.R
	@Rscript $<

# `inst/scripts/` R scripts to render `inst/figures/` images.
inst_scripts 	:= $(wildcard inst/scripts/*.R)
inst_figures 	:= $(inst_scripts:inst/scripts%=inst/figures%)
pdf		:= $(inst_figures:%.R=%.pdf)
png		:= $(inst_figures:%.R=%.png)

# Write .png / .pdf image `inst/figures/` files.
figures: $(pdf) $(png) specimens
inst/figures/%.pdf: inst/scripts/%.R
	Rscript $(<D)/$(<F)
inst/figures/%.png: inst/scripts/%.R
	Rscript $(<D)/$(<F)

# Render R markdown .docx chapter files.
word:
	Rscript tools/render_word.R

# Build species descriptions bookdown.
descriptions:
	Rscript data-raw/descriptions/descriptions.R

# Render project README files.
readme: README.Rmd data-raw/README.Rmd
	Rscript exec/readme.R

# R CMD INSTALL
package:
	Rscript -e 'devtools::document()'
	Rscript -e 'devtools::install_local(path = "~/Thesis", force = TRUE)'

# R CMD check
check:
	Rscript -e 'devtools::check(args = c("--no-tests", "--no-examples"))'

quarto-book: figures
	quarto render docs/thesis --profile pdf
	quarto render docs/thesis --profile html
	quarto render docs/thesis --profile docx

# `pkgdown` Package Website
site: README.Rmd
	Rscript -e 'pkgdown::clean_site()'
	Rscript -e 'rmarkdown::render(input = "README.Rmd")'
	Rscript -e 'pkgdown::build_site()'

## `covr` | Report coverage
covr-report:
	 Rscript exec/coverage.R

clean:
	rm Rplots.pdf
	rm inst/figures/*
