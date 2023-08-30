.PHONY : all data

SPECIMENS	:=	$(wildcard data-raw/specimens/*.R)
RDATA		:=	$(patsubst data-raw/specimens/%.R, data/%.rda, $(SPECIMENS))

all: $(RDATA)

## data : Render package data files in `data/*.rda` from `data-raw/*.R` scripts.
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

## readme : Render project README files.
readme: README.Rmd data-raw/README.Rmd
	Rscript exec/readme.R

## document : Update `man/*.Rd` R documentation files.
document:
	Rscript -e 'devtools::document()'

## test : Run `testthat` package unit tests.
test: document
	Rscript -e 'devtools::test()'

## check : Check development status via `R CMD Check` wrapper.
check: document
	Rscript -e 'devtools::check()'

## coverage : Calculate test coverage report with `covr` package.
coverage: document
	 Rscript exec/coverage.R

## install : Install development package from project source with `renv`
install: document
	Rscript -e 'renv::install(".")'

## book : Render quarto book project profiles for .pdf, .html, and .docx
book: figures
	quarto render docs/thesis --profile pdf
	quarto render docs/thesis --profile html
	quarto render docs/thesis --profile docx

## pkgdown : Render package website with reference documentation and articles
pkgdown:
	Rscript \
 		-e 'pkgdown::clean_site()' \
		-e 'pkgdown::build_site()'

## quarto : Render quarto website landing page for thesis and package.
quarto:
	quarto render

## open : Open website landing page with firefox application.
open:
	open -a firefox _site/quarto/index.html

clean:
	rm Rplots.pdf
	rm inst/figures/*

## settings : Show build rule script pre-requisites and data targets.
settings :
	@printf "\nR Scripts\n"
	@printf "  * %s\n" $(SPECIMENS)
	@printf "\nR Data\n"
	@printf "  * %s\n" $(RDATA)

## help : Show all commands.
help :
	@grep -h -E '^##' ${MAKEFILE_LIST} \
		| sed -e 's/## //g' \
 		| column -t -s ':'
