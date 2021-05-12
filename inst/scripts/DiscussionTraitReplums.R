library(Thesis)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Replum Shape.
trait_replums <-
  separate_discrete_trait(
    specimen_tbl = Thesis::herbarium_specimens,
    trait_selection = "Replum_shape"
  ) %>%
  dplyr::filter(
    !grepl(pattern = paste(c("obtuse", "constricted"), collapse = "|"),
           x = .data$Trait)
  ) %>%
  dplyr::mutate(
    Trait = purrr::map_chr(.x = .data$Trait, function(trait) {
      trait %>%
        gsub("oblanceolate|obovate", "Oblanceolate | Obovate", x = .) %>%
        gsub("elliptic|oblong", "Elliptic | Oblong", x = .) %>%
        capitalize(character_vector = .)
    })
  ) %>% dplyr::distinct()

ggplotTraitReplums <- trait_replums %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(color = "Replum Shape")

FigDiscussionTraitReplums <-
  plot_grid(
    ggplotTraitReplums +
      theme(legend.position = "none"),
    get_legend(ggplotTraitReplums),
    nrow = 1, rel_widths = c(2, 1)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    width = c(10, 5),
    height = c(8, 4.5),
    aspect = c(.167, .167)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/DiscussionTraitReplums", ext = ext),
      plot = FigDiscussionTraitReplums,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = 1,
      ncol = 1
    )
  })

