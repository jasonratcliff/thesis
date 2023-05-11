################################################################################
#
# 1) Create `data/*.rda` files
#
# Each `.rda` file contains an exported R data object in the thesis
# NAMESPACE. Scripts and raw data are contained in `data-raw/` subdirectories.
#
# Includes:
#   - Herbarium voucher data    `herbarium_specimens`
#   - Sampled DNA specimens     `dna_specimens`
#   - SEINet occurrences        `seinet_coords`
#   - ggplot aesthetic values   `spp_(color|shape)`
#
# 2) Write figure images to `inst/figures/*(.png|.pnf)`
#
# - File stems are matched by R scripts in `inst/scripts/*.R`
#
# `data-raw/` R scripts to render binary `.Rda` files.
r_spp	:= data-raw/specimens/vouchers.R
r_dna	:= data-raw/specimens/dna.R
r_themes	:= data-raw/specimens/aesthetics.R
r_seinet	:= data-raw/SEINet/SEINet.R
rda_specimens	:= $(wildcard data/*specimens.rda)
rda_themes	:= $(wildcard data/spp*.rda)

# `inst/scripts/` R scripts to render `inst/figures/` images.
inst_scripts 	:= $(wildcard inst/scripts/*.R)
inst_figures 	:= $(inst_scripts:inst/scripts%=inst/figures%)
pdf		:= $(inst_figures:%.R=%.pdf)
png		:= $(inst_figures:%.R=%.png)

.PHONY: all
all: specimens themes seinet figures package check manuscript site # slides

# Write .rda binary `data/` files.
specimens: $(rda_specimens)
data/herbarium_specimens.rda: $(r_spp) data-raw/specimens/vouchers.xlsx
	Rscript $(r_spp)
data/dna_specimens.rda: $(r_dna) data-raw/specimens/dna.csv
	Rscript $(r_dna)

themes: $(rda_themes)
data/spp_color.rda data/spp_shape.rda: $(r_themes)
	Rscript $(r_themes)

seinet: data/seinet_coords.rda
data/seinet_coords.rda: $(r_seinet) data-raw/SEINet/P*/occurrences.csv
	Rscript $(r_seinet)

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
	Rscript scripts/render_readme.R

# R CMD INSTALL
package:
	Rscript -e 'devtools::document()'
	Rscript -e 'devtools::install_local(path = "~/Thesis", force = TRUE)'

# R CMD check
check:
	Rscript -e 'devtools::check(args = c("--no-tests", "--no-examples"))'

manuscript: figures
	Rscript scripts/render_book.R

# `pkgdown` Package Website
site: README.Rmd
	Rscript -e 'pkgdown::clean_site()'
	Rscript -e 'rmarkdown::render(input = "README.Rmd")'
	Rscript -e 'pkgdown::build_site()'

clean:
	rm Rplots.pdf
	rm inst/figures/*
