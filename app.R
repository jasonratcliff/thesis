require(shiny)
require(magrittr)
require(readr)
require(knitr)
require(kableExtra)
require(ggplot2)
require(lemon)
require(gridExtra)

# Source R functions, build ggplot map borders and specimen data.
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

# Select input options for subsetting / mapping aesthetics.
select_names <-
  as.list(unique(names(specimen_data)[grep(
    "Physaria|Taxon_a_posteriori", names(specimen_data))]))

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

# Find Specimen Function ----
spp_find <- function(gg_map_obj, taxa_frame, collector, collection_number) {

  # Find collection by collector and collection number.
  if (is.na(collector) | is.na(collection_number)) {
    warning("Enter arguments for collector and collection number...")
  } else {
    spp_id <- taxa_frame %>%
      dplyr::filter(grepl(Collector, pattern = collector) &
                      grepl(Collection_Number, pattern = collection_number)) %>%
      dplyr::select(Collector, Collection_Number,
                    Latitude, Longitude)
  }
  
  # Calculate label offset for plotting.
  x_adjust <- (dist(gg_map_obj$coordinates$limits$x)[1]) * .15
  y_adjust <- (dist(gg_map_obj$coordinates$limits$y)[1]) * -.2
    
  # Plot additional map layer to include the individual specimen.
  gg_map_obj +
    geom_point(data = spp_id, inherit.aes = FALSE,
               mapping = aes(x = Longitude, y = Latitude),
               show.legend = FALSE, size = 6, shape = 5, fill = NA) +
    geom_label(data = spp_id, inherit.aes = FALSE,
               nudge_x = 0.25, nudge_y = -0.15,
               label.padding = unit(0.1, "lines"), size = 4,
               mapping = aes(x = Longitude, y = Latitude,
                             label = paste(spp_id$Collector,
                                           spp_id$Collection_Number,
                                           sep = "\n")), alpha = 0.5)
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
        fluidRow(
          br(),
          column(6,
                 tableOutput("mapRange")
                 ),
          column(6,
                 tableOutput("brushedArea")
                 )
          ),
        fluidRow(
          plotOutput("mapPlot",
                     brush = brushOpts(id = "map_brush", resetOnNew = TRUE))
          ),
        fluidRow(
          plotOutput("mapLegend")
          )
        ),
      tabPanel("Parameters",
        br(),
        h3("Subsetting and Mapping Columns"),
        br(),
        fluidRow(
          # Select column for specimen subsetting and ggplot color aesthetic.
          selectInput(inputId = "subset_taxa_id", label = "Subset Taxa ID",
            choices = select_names, selected = "Physaria_syn"),
          selectInput(inputId = "map_color_aes", label = "Mapped Taxa ID",
            choices = select_names, selected = "Physaria_syn")
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
              textInput(inputId = "collector_id", value = NA,
                        label = "Collector"),
              textInput(inputId = "collection", value = NA,
                        label = "Collection Number"))
            )
          ),
          hr()
        ),
      tabPanel("Brushed Specimens", tableOutput("brushedPlants"))
      )
    )
  )

# Shiny App Server ----
server <- function(input, output, session) {

  # Reactive Specimen Subset ----
  react_lat <- reactive({ input$latitude })
  react_long <- reactive({ input$longitude })
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

  # Render Map Plot ----
  map_ggplot <- reactive({
    input$map_button
    spp_distribution <-
      map_specimens(map_df = specimen_subset(),
                    map_gg_base = gg_borders,
                    map_col = input$map_color_aes,
                    jitter_pos = c(0.035, 0.035)) +
      theme(legend.direction = "vertical", legend.position = "right",
            legend.text.align = 0, legend.title.align = 0.5,
            legend.text = element_text(size = 12),
            legend.title = element_text(face = "bold", size = 15))

    # Optionally plot specimen identification.
    if (isolate(input$spp_find) == TRUE) {
      req(input$collector_id)
      req(input$collection)
      spp_distribution <-
        spp_find(gg_map_obj = spp_distribution,
                 taxa_frame = specimen_subset(),
                 collector = isolate(input$collector_id),
                 collection_number = isolate(input$collection))
    }
      spp_distribution
  })

  # Output map ggplot.
  output$mapPlot <- renderPlot({
    req(input$map_button)
    map_ggplot() + theme(legend.position = "none")
    }, height = function() {
      session$clientData$output_mapPlot_height
      })

  # Ouptut extracted ggplot legend.
  output$mapLegend <- renderPlot({
    req(input$map_button)
    map_legend <- lemon::g_legend(map_ggplot())
    gridExtra::grid.arrange(map_legend)
    }, height = function() {
      session$clientData$output_mapLegend_height
      })

  # Coordinate Range UI ----
  output$Latitude <- renderUI({
    input$map_button
    range_lat <- isolate({
      specimen_subset() %>% dplyr::select(Latitude) %>%
        range(na.rm = TRUE) %>% paste(collapse = ", ")
      })
    textInput(inputId = "latitude", label = "Latitude", value = range_lat)
    })
  output$Longitude <- renderUI({
    input$map_button
    range_long <- isolate({
      specimen_subset() %>% dplyr::select(Longitude) %>%
        range(na.rm = TRUE) %>% paste(collapse = ", ")
      })
    textInput(inputId = "longitude", label = "Longitude", value = range_long)
    })

  # Coordinate Range Kable ----
  output$mapRange <- function() {
    req(input$map_button)
    map_latitude <- map_ggplot()$coordinates$limits$y %>% sort()
    map_longitude <- map_ggplot()$coordinates$limits$x %>% sort
    dplyr::bind_cols(Latitude = map_latitude,
                     Longitude = map_longitude) %>% t() %>%
      knitr::kable("html", col.names = c("Minimum", "Maximum")) %>%
      kableExtra::kable_styling(bootstrap_options = c("bordered"))
  }

  # Map Brush ----
  observeEvent(input$map_brush, {
    brush_dim <- reactive({
      req(input$map_brush)
      brushOpts(input$map_brush)$id[c("xmin", "xmax", "ymin", "ymax")]
      })

    # Output brush dimensions to kable.
    output$brushedArea <- function() {
      dplyr::bind_cols(`Brushed x` = unlist(brush_dim()[c("xmin", "xmax")]),
                       `Brushed y` = unlist(brush_dim()[c("ymin", "ymax")])) %>%
        t() %>% knitr::kable("html", col.names = c("Minimum", "Maximum")) %>%
        kableExtra::kable_styling(bootstrap_options = c("bordered"))
      }

    # Output brushed specimens to kable.
    output$brushedPlants <- function() {
      brushedPoints(df = specimen_subset(), brush = input$map_brush,
                    xvar = "Longitude", yvar = "Latitude") %>%
        dplyr::select(Collector, Collection_Number,
                      Physaria_syn, Taxon_a_posteriori) %>%
        dplyr::arrange(Taxon_a_posteriori, Collector) %>%
        dplyr::distinct() %>%
        dplyr::rename(`Collection Number` = Collection_Number,
                      `Recent Annotation` = Physaria_syn,
                      `Reviewed Annotation` = Taxon_a_posteriori) %>%
        knitr::kable("html") %>%
        kableExtra::kable_styling(bootstrap_options = c("bordered"))
      }
    })
}

# Run app ----
shinyApp(ui, server)

