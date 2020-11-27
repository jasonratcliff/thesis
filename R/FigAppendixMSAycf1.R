library(ThesisPackage)
library(ggplot2)
library(cowplot)

# ycf1 Intron Alignment ----
ggMSAycf1 <-
  system.file("extdata/Alignments/ycf1-single.fasta",
              package = "ThesisPackage") %>%
  potentially_informative_sites(
    fasta_file = .,
    seq_name = TRUE
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 3)
  )

FigAppendixMSAycf1 <- plot_grid(ggMSAycf1)

ThesisPackage::save_plot(gg_plot = FigAppendixMSAycf1)
