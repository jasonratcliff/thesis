# Call expression wrapper over ggrepel geoms

repel_map_labels <- function(label_nudges, label_join, init_ggplot) {
  dplyr::left_join(
    x = label_nudges,
    y = label_join,
    by = "Key"
  ) %>%
    purrr::pmap(
      .l = .,
      .f = function(
        Collector, Collection_Number,
        Longitude, Latitude, Label,
        nudge_x, nudge_y, segment.curvature, ...
      ) {
        rlang::call2(
          .fn = ggrepel::geom_label_repel,
          data = tibble::tibble(
            Longitude = Longitude,
            Latitude = Latitude,
            Label = Label
          ),
          mapping = ggplot2::aes(
            x = Longitude,
            y = Latitude,
            label = Label
          ),
          nudge_x = nudge_x,
          nudge_y = nudge_y,
          segment.curvature = segment.curvature,
          box.padding = 0.5,
          alpha = 0.66,
          segment.color = "white"
        )
      }) %>%
    purrr::reduce(
      .x = .,
      .f = ~ rlang::expr(!!.x + !!.y),
      .init = init_ggplot
    )
}

