################################################################################
#
# 1) Create `data/*.rda` Files
#
# Each .rda file contains an exported R data object in the ThesisPackage
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
spp_herbarium	:= inst/extdata/specimens.xlsx
spp_dna_samples	:= data-raw/specimens/dna_specimens.csv
spp_themes := data-raw/mapping/map_themes.R
trait_scripts	:= $(wildcard data-raw/specimens/trait*.R)
trait_rda	:= $(wildcard data/trait*.rda)
seinet_csv	:= $(wildcard data-raw/SEINet/P*/occurrences.csv)

all: traits seinet specimens
.PHONY: all

##### SPECIMEN DATA #####

specimens: $(spp_herbarium) $(spp_dna_samples) themes

# Parsed herbarium voucher specimens as: `ThesisPackage::herbarium_specimens`
data/herbarium_specimens.rda: $(spp_herbarium)
	Rscript data-raw/specimens/herbarium_specimens.R

# Subset of DNA specimens as: `ThesisPackage::dna_specimens`
data/dna_specimens.rda: $(spp_dna_samples)
	Rscript data-raw/specimens/herbarium_specimens.R

# ggplot aesthetic manual value specifications
themes: data/spp_color.rda data/spp_shape.rda

data/spp_color.rda data/spp_shape.rda: $(spp_themes)
	Rscript data-raw/mapping/map_themes.R

##### TRAIT DATA #####

# Save .Rda files for specimen discrete and continuous trait subsets.
traits: $(trait_scripts) $(trait_rda)

$(trait_scripts) $(trait_rda): data/herbarium_specimens.rda

# trait%.rda - Pattern rule for compressed .rda (R data) trait files.
data/trait%.rda: data-raw/specimens/trait%.R
	Rscript $(<D)/$(<F)

##### SEINet DATA #####

# SEINet specimen occurrence available as: `ThesisPackage::seinet_coords`
seinet: data-raw/SEINet/SEINet.R

# Rscript is dependent on wildcard matched "occurrences.csv" files.
data/seinet_coords.rda: $(seinet_csv)
	Rscript data-raw/SEINet/SEINet.R

##### README #####

# Generate data-raw/README as `github_document` output.
data-raw/README.md: data-raw/README.Rmd data-raw/mapping/map-dna.R
	Rscript -e 'rmarkdown::render(input = "data-raw/README.Rmd", output_format = "github_document", output_file = "README.md")';\
	rm data-raw/README.html
