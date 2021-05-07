library(ThesisPackage)
library(ggplot2)
library(cowplot)

# rITS Alignment ----
ggMSArITS <-
  system.file("extdata/Alignments/rITS-single.fasta",
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

AppendixMSArITS <- plot_grid(ggMSArITS)

purrr::walk(
  .x = c("png", "pdf"),
  .f = function(ext) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/AppendixMSArITS", ext = ext),
      plot = AppendixMSArITS
    )
  })

