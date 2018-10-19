
# Set global options and source libraries for individual markdown files

# Load library dependencies
library(tinytex)
library(bookdown)
library(rmarkdown)
library(pander)
# options(knitr.table.format = "latex")
library(kableExtra)
library(grid)
library(gridExtra)
library(xtable)
library(ggplot2)
library(openxlsx)
library(plyr)
library(dplyr)
library(magrittr)
library(maps)
library(elevatr)
library(raster)
library(rgdal)
library(caTools)
library(mapproj)
library(grDevices)


# Load bioconductor packages 'ggtree' and 'msa'
if ("ggtree" %in% rownames(installed.packages()) == FALSE) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("ggtree")
}
library(ggtree)

if ("msa" %in% rownames(installed.packages()) == FALSE) {
  biocLite("msa")
}
library(msa)

# Due to issue with ggmap call, developer branch of ggmap
# addressing issue from https://github.com/dkahle/ggmap/issues/85
# with fix at https://github.com/dkahle/ggmap/pull/88
# used for ggmap build.  See citation here:
# https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
if ("ggmap" %in% rownames(installed.packages()) == FALSE) {
  # Install from author github repo master branch
  library(devtools)
  devtools::install_github("dkahle/ggmap")
}
library(ggmap)

# Read specimen data and ggplot aesthetics to Global Environment
source("R/load_species.R")
source("R/ggplot_aes_vars.R")

# Source R functions
source("R/find_spp.R")  # Source find_spp()
source("R/subset_spp.R")  # Source subset_spp()
source("R/topo_spp.R")  # Source topo_spp()
source("R/map_spp.R")  # Source map_spp()
source("R/morph_spp.R")  # Source morph_spp()
source("R/cont_spp.R")  # Source con_trait()
source("R/elev_spp.R")  # Source elev_spp()
source("R/disc_viz.R")  # Source disc_viz()
source("R/knitr_helper.R") # knitr figure caption and chunk label functions

# Setup for Phylogenetic Tree Annotation
source("R/herb_tree.R") # Source phylogenetic annotation functions
source("R/phylo_spp.R") # Source phylo_spp()
source("R/phylo_text.R") # Source phylo_text()

# automatically create a bib database for R packages
knitr::write_bib(c(
  .packages(), 'bookdown', 'knitr', 'rmarkdown'
), 'packages.bib')

# Establish character vector with elements as State strings.
phys_states <- c("Montana", "Wyoming", "Idaho", "Colorado", "Utah", 
                 "New Mexico", "South Dakota", "North Dakota", "Nebraska")


# Set colour brewer global variable containing palette options named by trait. 
colour_brewer_palette <- c(Ovule_number = "PiYG", Replum_shape = "RdYlBu",
                           Basal_leaf_margins = "Spectral")

# Replace taxa name for Physaria medicinae specimens.
phys_medi <- gsub("Physaria acutifolia - vitulifera-like", 
                  "Physaria medicinae", total_physaria$Taxon_a_posteriori)

phys_medi <- gsub("Physaria vitulifera - carbon", 
                  "Physaria medicinae", phys_medi)

total_phys_medi <- total_physaria
total_phys_medi$Taxon_a_posteriori <- phys_medi

# Subset Carbon County, Wyoming specimens within bounding box.
carbon_wyo <- subset_spp(taxa_frame = total_physaria, 
                         state = c("Wyoming", "Colorado"),
                         latitude = c(39.6, 42.3), longitude = c(-108, -105),
                         set_name = "carbon_wyo", save_set = FALSE)

