
# Set global options and source libraries for individual markdown files

# Load library dependencies
library(tinytex)
library(bookdown)
library(rmarkdown)
library(pander)
# options(knitr.table.format = "latex")
library(kableExtra)
library(gridExtra)
library(xtable)
library(ggplot2)
library(openxlsx)
library(plyr)
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

# Read specimen data to Global Environment
source("R/load_species.R")

# Source R functions
source("R/find_spp.R")  # Source find_spp()
source("R/subset_spp.R")  # Source subset_spp()
source("R/topo_spp.R")  # Source topo_spp()
source("R/map_spp.R")  # Source map_spp()
source("R/morph_spp.R")  # Source morph_spp()
source("R/cont_spp.R")  # Source con_trait()
source("R/elev_spp.R")  # Source elev_spp()

# Setup for Phylogenetic Tree Annotation
source("Phys_DNA/DNA_species.R")
source("R/phylo_spp.R") # Source phylo_spp()
source("R/phylo_text.R") # Source phylo_text()

# automatically create a bib database for R packages
knitr::write_bib(c(
  .packages(), 'bookdown', 'knitr', 'rmarkdown'
), 'packages.bib')

# Establish character vector with elements as State strings.
phys_states <- c("Montana", "Wyoming", "Idaho", "Colorado", "Utah", 
                 "New Mexico", "South Dakota", "North Dakota", "Nebraska")

# Named character vector for ggplot scale color manual call.
spp_color <- c("Physaria acutifolia" = "yellow",
               "Physaria vitulifera" = "plum",
               "Physaria nova" = "purple2", 
               "Physaria acutifolia - vitulifera-like" = "turquoise",
               "Physaria vitulifera - carbon" = "steelblue",
               "Physaria floribunda" = "gold",
               "Physaria floribunda ssp. floribunda" = "goldenrod",
               "Physaria floribunda ssp. osterhoutii" = "limegreen",
               "Physaria rollinsii" = "darkorchid1",
               "Physaria alpina" = "firebrick",
               "Physaria brassicoides" = "olivedrab2",
               "Physaria didymocarpa ssp. didymocarpa" = "skyblue",
               "Physaria didymocarpa ssp. lanata" = "goldenrod",
               "Physaria didymocarpa ssp. lyrata"= "black",
               "Physaria saximontana ssp. dentata" = "maroon",
               "Physaria saximontana ssp. saximontana" = "indianred2",
               "Physaria eburniflora" = "cyan",
               "Physaria integrifolia" =  "seagreen",
               "Physaria dornii" = "darkorange",
               "Physaria condensata"= "mediumblue",
               "Physaria" = "seashell",
               "Physaria flowering" = "seashell",
               "Lesquerella fendleri" = "black",
               "Lesquerella argyrea" = "black", 
               "na.value" = "black")

# Named character vector for ggplot scale shape manual call.
spp_shape <- c("Physaria acutifolia" = 3,
               "Physaria vitulifera"= 8,
               "Physaria nova" = 17, 
               "Physaria acutifolia - vitulifera-like" = 17,
               "Physaria vitulifera - carbon" = 17,
               "Physaria floribunda" = 15,
               "Physaria floribunda ssp. floribunda" = 18,
               "Physaria floribunda ssp. osterhoutii" = 18,
               "Physaria rollinsii" = 17,
               "Physaria alpina" = 15,
               "Physaria brassicoides" = 16,
               "Physaria didymocarpa ssp. didymocarpa" = 15,
               "Physaria didymocarpa ssp. lanata" = 25,
               "Physaria didymocarpa ssp. lyrata" = 17,
               "Physaria saximontana ssp. dentata" = 18,
               "Physaria saximontana ssp. saximontana" = 17,
               "Physaria eburniflora" = 18,
               "Physaria integrifolia" =  18,
               "Physaria dornii" = 16,
               "Physaria condensata"= 16,
               "Physaria" = 16,
               "Physaria flowering" = 16, 
               "Lesquerella fendleri" = 15,
               "Lesquerella argyrea" = 18, 
               "na.value" = 126)
