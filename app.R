library(shiny)
require(magrittr)
require(readr)
require(ggplot2)
# require(grid)

# Source R functions
source("R/map_tools.R")
gg_borders <- map_borders(border_color = "black")

specimen_data <-
  purrr::map_dfr(list.files("output/specimens", full.names = TRUE),
    function(specimens_file) {
      spp_csv <-
        readr::read_csv(specimens_file, col_names = TRUE,
          col_types = cols(Physaria_a_priori_1 = col_character(), Physaria_a_priori_2 = col_character(), Physaria_a_priori_3 = col_character(), Physaria_a_priori_4 = col_character(), Physaria_recent = col_character(), Physaria_syn = col_character(), Taxon = col_character(), Taxon_a_posteriori = col_character(), Collector = col_character(), Collection_Number = col_character(), Date = col_character(), Herbarium = col_character(), State = col_character(), County = col_character(), Latitude = col_double(), Longitude = col_double(), ID = col_character(), App.A = col_character(), Imaged = col_character(), Elev_m = col_character(), Elev_ft = col_character(), DNA_extraction_complete = col_character(), Google_Earth = col_character(), Mature_Fruit = col_character(), Juvenile_Fruit = col_character(), TRS2 = col_character(), Notes = col_character(), Rosulate = col_character(), Caudex = col_character(), Pubescence = col_character(), Basal_leaf_trichomes = col_character(), Fruit_trichomes = col_character(), Stem_count = col_character(), Stem_shape = col_character(), Stem_length_dm = col_character(), Petiole = col_character(), Basal_leaf_length_cm = col_character(), Basal_leaf_shape = col_character(), Basal_leaf_margins = col_character(), Cauline_leaf_length_mm = col_character(), Cauline_leaf_shape = col_character(), Cauline_leaf_margins = col_character(), Racemes = col_character(), Pedicel_shape = col_character(), Pedicels_secund = col_character(), Sepal_length_mm = col_character(), Sepal_shape = col_character(), Petal_color = col_character(), Petal_length_mm = col_character(), Petal_shape = col_character(), Style_length_mm = col_character(), Mature_fruit_length_mm = col_character(), Mature_fruit_width_mm = col_character(), Fruit = col_character(), Mature_fruit_apices = col_character(), Replum_pubescence = col_character(), Inner_valve_pubescence = col_character(), Ovule_number = col_character(), Replum_shape = col_character(), Seed_color = col_character(), Seed_shape = col_character(), Mature_seed_length_mm = col_character(), `Chromosome.#` = col_character(), Date_parsed = col_date(format = "%Y-%m-%d"), Date_md = col_date(format = "%Y-%m-%d"), elev_raw_ft = col_character(), elev_raw_m = col_character(), elev_min = col_double(), elev_max = col_double()))
      dplyr::bind_rows(spp_csv)
      })

# Subset Specimens Function ----
spp_subset <- function(taxa_frame, state = NULL, county = NULL,
                       longitude = NULL, latitude = NULL,
                       spp_str = NULL, taxa_col = NULL,
                       exclude = c(FALSE, TRUE)) {

  # Helper function to parse coordinate character vector.
  parse_coordinates <- function(coordinates) {
    coordinate_range <-
      gsub(" +", "", x = coordinates) %>%
      gsub("![0-9\\.,-]", "", x = .) %>%
      strsplit(coordinates, split = ",") %>% unlist() %>%
      as.numeric() %>% sort()
    if (!(length(coordinate_range) == 2 | length(coordinate_range) == 0)) {
      stop("Ensure coordinates are two comma separated values.")
    }
    return(coordinate_range)
  }

  # Subset taxa records by coordinate ranges.
  if (!is.null(latitude) | !is.null(longitude)) {
    if (nchar(latitude) > 0 | nchar(longitude) > 0) {

      # Assign list of parsed coordinate values, where length == 2 OR NA.
      coord_values <-
        lapply(list(latitude, longitude), function(coordinate) {
          parse_coordinates(coordinate)
        })
      names(coord_values) <- c("Latitude", "Longitude")

      # Calculate range sizes of coordinate inputs.
      coord_diffs <-
        lapply(coord_values, function(coordinate) {
          if (length(coordinate) != 0) {
            coordinates <-
              seq(from = min(coordinate), to = max(coordinate), by = 1) %>%
              length()
          } else {
            coordinates <- NA
          }
        })

      # Handle cases where a single coordinate range is missing.
      if (TRUE %in% is.na(coord_diffs) &
          FALSE %in% is.na(coord_diffs)) {
        coord_missing <- coord_values[which(is.na(coord_diffs))]
        coord_present <- coord_values[which(!is.na(coord_diffs))]

        # Subset data frame to rows within non-missing coordinate range.
        coord_name <- names(coord_present)
        quo_present <- enquo(coord_name)
        taxa_present <- taxa_frame %>%
          dplyr::filter(.[[!!quo_present]] > coord_present[[1]][1] &
                          .[[!!quo_present]] < coord_present[[1]][2])

        # Estimate center of missing coordinates in present subset.
        coord_missing_mid <- taxa_present[, names(coord_missing)] %>%
          (function(x) { (min(x[[1]]) + max(x[[1]])) / 2 })
        coord_diff <- coord_diffs[which(!is.na(coord_diffs))][[1]]
        coord_missing_range <- c(coord_missing_mid + coord_diff / 2,
                                 coord_missing_mid - coord_diff / 2)

        # Subset data frame to mid-point of missing coordinate range.
        coord_name <- names(coord_missing)
        quo_present <- enquo(coord_name)
        taxa_frame <- taxa_present %>%
          dplyr::filter(.[[!!quo_present]] > min(coord_missing_range) &
                          .[[!!quo_present]] <  max(coord_missing_range))
      }

      # Handle cases where both coordinate ranges are present.
      if (length(coord_values[["Latitude"]]) == 2 &
          length(coord_values[["Longitude"]]) == 2) {
        taxa_frame <- taxa_frame %>%
          dplyr::filter(Latitude > min(coord_values[["Latitude"]]) &
                          Latitude < max(coord_values[["Latitude"]]) &
                          Longitude > min(coord_values[["Longitude"]]) &
                          Longitude < max(coord_values[["Longitude"]]))
      }
    }
  }

  # Subset specimen records by species string.
  if (!is.null(spp_str)) {
    if (is.null(taxa_col)) {
      stop("Set `taxa_col` argument for column subset.")
    } else {
      taxa_frame <-
        taxa_frame[grep(pattern = paste(spp_str, collapse = "|"),
                        x = taxa_frame[, taxa_col][[1]], invert = exclude), ]
      }
    }
  return(taxa_frame)
}

# Brushed Specimens Function ----
brushed_specimens <- function(spp_df, map_cols) {
  for (i in 1:nrow(spp_df)) {

    if (grepl("ssp.", spp_df[i, map_cols[1]])) {
      taxa_char <- unlist(strsplit(spp_df[i, map_cols[1]],
                                   split = " ssp. ", fixed = TRUE))
      taxon <- paste(taxa_char[1], taxa_char[2], sep = "\n\t\t  ssp. ")
    } else { taxon <- spp_df[i, map_cols[1]]}

    cat(spp_df[i, "Collector"], "\n",
        "  ", spp_df[i, "Collection_Number"], spp_df[i, "County"], "\n",
        "\t", taxon, '\n')
    cat("\n")
  }
}

# Distribution Plotting UI ----
ui <- fluidPage(
  titlePanel("Physaria Spatial Analysis"),

  # Show a plot of the generated map and allow brushing geom points.
  mainPanel(
    sidebarPanel(
      actionButton("map_button", "Generate map")
      ),
    tabsetPanel(
      tabPanel("Distribution",
               plotOutput("mapPlot", height = "1000px",
                 brush = brushOpts(id = "map_brush", resetOnNew = TRUE))),
      tabPanel("Parameters",
        br(),
        h3("Subsetting and Mapping Columns"),
        br(),
        fluidRow(
          # Select column for specimen subsetting and ggplot color aesthetic.
          selectInput(inputId = "subset_taxa_id", label = "Subset Taxa ID",
            choices = as.list(unique(names(specimen_data)[grep(
              "Physaria|Taxon_a_posteriori", names(specimen_data))])),
            selected = "Physaria_syn"),
          selectInput(inputId = "map_color_aes", label = "Mapped Taxa ID",
            choices = names(specimen_data), selected = "Physaria_syn")
          ),
        br(),
        h4("Optional Parameters"),
        hr(),
        fluidRow(
          column(4,
            # Optional choice to subset specimens by coordinate data.
            checkboxInput(inputId = "coordinates", value = FALSE,
                          label = "Subset by Latitude and Longitude?"),
            conditionalPanel(condition = "input.coordinates == true",
                             uiOutput("Latitude"), uiOutput("Longitude"))
            ),
          column(4,
            # Optional choice to subset specimens RegEx (w/ | w/o exlusion).
            checkboxInput(inputId = "spp_filter", value = FALSE,
                          label = "Subset by species?"),
            conditionalPanel(condition = "input.spp_filter == true",
              textInput(inputId = "taxa_filter", label = "Species",
                        value = NULL, placeholder = "Species RegEx"),
              checkboxInput(inputId = "exclude", value = FALSE,
                            label = "Exclude species subset?"))
            ),
          column(4,
            # Conditional panel for finding individual voucher.
            checkboxInput(inputId = "spp_find", value = FALSE,
                          label = "Find individual specimen?"),
            conditionalPanel(condition = "input.spp_find == true",
              textInput(inputId = "collector_id", label = "Collector"),
              textInput(inputId = "collection_id", label = "Collection Number"))
            )
          ),
          hr()
        )
      )
    )
  )

# Shiny App Server ----
server <- function(input, output, session) {

  # Set coordinate range reactive conductors.
  react_lat <- reactive({ input$latitude })
  react_long <- reactive({ input$longitude })

  # Reactive tibble data frame of subset specimens by user input.
  specimen_subset <- reactive({
    input$map_button
    species_string <- isolate(input$taxa_filter)
    spp_subset(taxa_frame = specimen_data,
               latitude = isolate(react_lat()),
               longitude = isolate(react_long()),
               taxa_col = input$subset_taxa_id,
               spp_str = species_string,
               exclude = input$exclude)
  })
  # Render map from reactive subset ----
  output$mapPlot <- renderPlot({
    if (input$map_button == 0) {
      return()
    } else {
      map_plot <-
        map_specimens(map_df = specimen_subset(),
                    map_gg_base = gg_borders,
                    map_col = input$map_color_aes) +
        theme(legend.direction = "vertical", legend.position = "bottom",
              legend.text.align = 0, legend.title.align = 0.5,
              legend.text = element_text(size = 12),
              legend.title = element_text(face = "bold", size = 15)) +
        guides(colour = guide_legend(ncol = 2, byrow = FALSE))
      map_plot
      # lemon::reposition_legend(map_plot, position = "bottom")
      # if () {
      #   map_plot <- map_plot +
      # }
      }
    })
      })
  })
}

shinyApp(ui, server)
