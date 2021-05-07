library(ThesisPackage)
library(ggplot2)
library(cowplot)

# rps Intron Alignment ----
ggMSArpsIntron <-
  system.file("extdata/Alignments/rps-single.fasta",
              package = "ThesisPackage") %>%
  potentially_informative_sites(
    fasta_file = .,
    seq_name = TRUE,
    char_width = 0.5
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 3)
  )

AppendixMSArpsIntron <- plot_grid(ggMSArpsIntron)

purrr::walk(
  .x = c("png", "pdf"),
  .f = function(ext) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/AppendixMSArpsIntron", ext = ext),
      plot = AppendixMSArpsIntron
    )
  })

