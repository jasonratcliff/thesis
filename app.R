library(shiny)
library(magrittr)
library(ggplot2)
library(thesis)
options(tigris_use_cache = TRUE)

# Distribution Plotting UI ----
ui <- fluidPage(
  titlePanel("Physaria Spatial Analysis"),
  mainPanel(
    sidebarPanel(actionButton("resetButton", "Reset Map")),

    tabsetPanel(

      # Map plot of specimen distribution.
      tabPanel(title = "Distribution",
        fluidRow(column(width = 6, tableOutput(outputId = "mapRange"))),
        fluidRow(
          column(12,
            plotOutput(
              outputId = "mapPlot",
              brush = brushOpts(
                id = "mapBrush", delay = 500,
                resetOnNew = TRUE)
            )
          )
        ),
        fluidRow(
          column(6,
            fluidRow(
              selectInput(
                inputId = "map_color_aes", label = "Mapped Taxa ID",
                choices = list("prior_id", "prior_1", "prior_2", "prior_3",
                               "prior_4", "Taxon_a_posteriori"),
                selected = "prior_id")),
            hr(),
            fluidRow(
              checkboxInput(inputId = "spp_find", value = FALSE,
                            label = "Find individual specimen?"),
              textInput(inputId = "collector_id", value = NA,
                        label = "Collector"),
              textInput(inputId = "collection", value = NA,
                        label = "Collection Number")
            )
          ),
          column(6,
            plotOutput("mapLegend")
          )
          )
        ),

      # Reactive table of brushed specimens.
      tabPanel("Specimens", tableOutput("specimens"))

      )
    )
  )

# Shiny App Server ----
server <- function(input, output, session) {

  # Reactive Specimen Subset
  specimens <- reactiveValues(data = NULL)

  # Plot Reset ----
  observeEvent(input$resetButton, {
    specimens$data <- thesis::herbarium_specimens %>%
      # Filter specimens without manual aesthetic value specification.
      # TODO Report excluded specimens in `Specimens` tab
      dplyr::filter(.data = ., .data[[input$map_color_aes]] %in%
                      names(thesis::spp_color))
  })

  # Brush Subset ----
  observeEvent(input$mapBrush, {

    # List of brush coordinates.
    brush_dim <- reactive({
      brushOpts(input$mapBrush)$id[c("xmin", "xmax", "ymin", "ymax")]
    })

    # Subset of specimens by brushed dimensions.
    specimens$data <- specimens$data %>%
      dplyr::filter(
        .data$Longitude > brush_dim()$xmin, .data$Longitude < brush_dim()$xmax,
        .data$Latitude > brush_dim()$ymin, .data$Latitude < brush_dim()$ymax
      )

    # Render table of brushed specimens.
    output$specimens <- renderTable({
      specimens$data %>%
        dplyr::select("prior_id", "Taxon_a_posteriori",
                      "Collector", "Collection_Number")
    })
  })

  # ggplot Reactive ----
  plot_map <- reactive({

    # ggplot tidy eval in aes_()
    legend_id <- input$map_color_aes

    ggplot2::ggplot() +
      layer_borders(
        spl_extent = spl_bbox(specimens$data),
        sf_county_color = "black") +
      layer_specimens(
        specimen_tbl = specimens$data,
        id_column = legend_id, shape_aes = TRUE) +
      layer_themes(
        specimen_tbl = specimens$data,
        id_column = legend_id, legend_title = legend_id) +
      coord_sf(
        xlim = range(spl_bbox(specimens$data)[["Longitude"]]),
        ylim = range(spl_bbox(specimens$data)[["Latitude"]])
        )
  })

  # Map Output ----
  output$mapPlot <- renderPlot({
    req(input$resetButton)
    if (input$spp_find == TRUE) {
      req(input$collector_id)
      req(input$collection)
      plot_map() +
        spl_id(specimen_tbl = specimens$data,
               id_column = input$map_color_aes, shape_aes = input$map_color_aes,
               collector = isolate(input$collector_id),
               collection = isolate(input$collection)) +
            theme(legend.position = "none")
    } else {
      plot_map() +
        theme(legend.position = "none")
      }
    },
    height = function() session$clientData$output_mapPlot_height,
    res = 96
  )

  # Legend Output ----
  output$mapLegend <- renderPlot({
    if (!is.null(specimens$data)) {
      cowplot::plot_grid(
        cowplot::get_legend(
          plot_map() + guides(col = guide_legend(ncol = 2))
        ), vjust = 1
      )
    }
  })

}

# Run app ----
shinyApp(ui, server)
