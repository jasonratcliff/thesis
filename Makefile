################################################################################
#
# 1) Create `data/*.rda` Files
#
# Each .rda file contains an exported R data object in the Thesis
# NAMESPACE. Scripts and raw data are contained in `data-raw/` subdirectories.
#
# Includes:
#   - Tidied herbarium voucher data and sampled DNA specimens
#   - Trait observations (discrete | continuous)
#   - SEINet occurrences
#
# 2) Generate README `github_document` output.
#
#   - `data-raw/`
#

# File paths to scripts, raw data, and binary .rda files.
xlsx_herbaria	:= inst/extdata/specimens.xlsx

csv_spp_dna	:= data-raw/specimens/dna_specimens.csv
csv_seinet	:= $(wildcard data-raw/SEINet/P*/occurrences.csv)
r_spp_herb	:= data-raw/specimens/herbarium_specimens.R
r_spp_dna	:= data-raw/specimens/dna_specimens.R
r_spp_themes	:= data-raw/mapping/map_themes.R
r_traits	:= $(wildcard data-raw/specimens/trait*.R)

rda_herbaria	:= data/herbarium_specimens.rda
rda_spp_dna	:= data/dna_specimens.rda
rda_traits	:= $(wildcard data/trait*.rda)

all: traits seinet specimens themes
.PHONY: all

##### SPECIMEN DATA #####

specimens: $(xlsx_herbaria) $(csv_spp_dna) $(rda_spp_dna) $(rda_herbaria) themes

# Parsed herbarium voucher specimens as: `Thesis::herbarium_specimens`
data/herbarium_specimens.rda:
	Rscript data-raw/specimens/herbarium_specimens.R

# Subset of DNA specimens as: `Thesis::dna_specimens`
data/dna_specimens.rda: $(csv_spp_dna) $(r_spp_herb) $(r_spp_dna) $(xlsx_herbaria)
	Rscript data-raw/specimens/dna_specimens.R

# ggplot aesthetic manual value specifications
themes: data/spp_color.rda data/spp_shape.rda

data/spp_color.rda data/spp_shape.rda: $(r_spp_themes)
	Rscript data-raw/mapping/map_themes.R

##### TRAIT DATA #####

# Save .Rda files for specimen discrete and continuous trait subsets.
traits: $(r_traits) $(rda_traits)

$(r_traits) $(rda_traits): data/herbarium_specimens.rda

# trait%.rda - Pattern rule for compressed .rda (R data) trait files.
data/trait%.rda: data-raw/specimens/trait%.R
	Rscript $(<D)/$(<F)

##### SEINet DATA #####

# SEINet specimen occurrence available as: `Thesis::seinet_coords`
seinet: data-raw/SEINet/SEINet.R

# Rscript is dependent on wildcard matched "occurrences.csv" files.
data/seinet_coords.rda: $(csv_seinet)
	Rscript data-raw/SEINet/SEINet.R

##### README #####

# Generate data-raw/README as `github_document` output.
data-raw/README.md: data-raw/README.Rmd data-raw/mapping/map-dna.R
	Rscript -e 'rmarkdown::render(input = "data-raw/README.Rmd", output_format = "github_document", output_file = "README.md")';\
	rm data-raw/README.html
