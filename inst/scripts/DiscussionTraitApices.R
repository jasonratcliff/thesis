library(Thesis)
library(cowplot)
library(ggplot2)

# Results: ggplot distribution of specimen Basal Fruit Apices.
trait_apices <- Thesis::herbarium_specimens %>%
  dplyr::mutate(
    Mature_fruit_apices = purrr::map_chr(
      .x = .data$Mature_fruit_apices,
      function(apex) {
        gsub("-", "_", x = apex)
      })
  ) %>%
  separate_discrete_trait(
    specimen_tbl = .,
    trait_selection = "Mature_fruit_apices"
  ) %>%
  dplyr::mutate(
    Trait =  purrr::map_chr(.x = .data$Trait, function(apex) {
      split_apex <- strsplit(x = apex, split = "_") %>% unlist()
      ifelse(
        test = length(split_apex) > 1,
        yes = paste(
          c(split_apex[1], sort(split_apex[2:length(split_apex)])),
          collapse = "_"
        ),
        no = split_apex
      )
    }),
    Trait = dplyr::case_when(
      grepl("absent", x = .data$Trait) & grepl("basal", x = .data$Trait)
      ~ "Basal Absent",
      grepl("shallow", x = .data$Trait) & grepl("basal", x = .data$Trait)
      ~ "Basal Shallow",
      grepl(pattern = "[Ee]qual", x = .data$Trait) ~ "Approximately Equal"
    )
  ) %>% dplyr::filter(!is.na(.data$Trait))

ggplotTraitApices <- trait_apices %>%
  map_trait_distribution(tidy_trait = .) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(color = "Mature Fruit Apices")

FigDiscussionTraitApices <-
  plot_grid(
    ggplotTraitApices +
      theme(legend.position = "none"),
    get_legend(ggplotTraitApices),
    nrow = 1, rel_widths = c(2.25, 1)
  )

purrr::pwalk(
  .l = list(
    ext = c("png", "pdf"),
    width = c(10, 5),
    height = c(8, 5),
    aspect = c(.167, .167)
  ),
  .f = function(ext, plot, width, height, aspect, row, col) {
    cowplot::save_plot(
      filename = fs::path("inst/figures/DiscussionTraitApices", ext = ext),
      plot = FigDiscussionTraitApices,
      base_width = width,
      base_height = height,
      base_asp = aspect,
      nrow = 1,
      ncol = 1
    )
  })

