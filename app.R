library(shiny)
library(magrittr)
library(ggplot2)
library(ThesisPackage)
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
          plotOutput(
            outputId = "mapPlot",
            brush = brushOpts(
              id = "mapBrush", delay = 500,
              resetOnNew = TRUE)
            )
          ),
        fluidRow(plotOutput("mapLegend"))
        ),

      # Select parameter for geom aesthetic layer.
      tabPanel(title = "Parameters", br(),
               fluidRow(
                 selectInput(
                   inputId = "map_color_aes", label = "Mapped Taxa ID",
                   choices = list("prior_id", "prior_1", "prior_2", "prior_3",
                                  "prior_4", "Taxon_a_posteriori"),
                   selected = "prior_id")), hr(),
               fluidRow(
                 # TODO fix implementation for finding individual voucher.
                 checkboxInput(inputId = "spp_find", value = FALSE,
                               label = "Find individual specimen?"),
                 textInput(inputId = "collector_id", value = NA,
                           label = "Collector"),
                 textInput(inputId = "collection", value = NA,
                           label = "Collection Number")
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
    specimens$data <- ThesisPackage::herbarium_specimens
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

    # Filter specimens without shape value specification
    # TODO Report excluded specimens in `Specimens` tab
    filtered_specimens <- specimens$data %>%
      dplyr::filter(.data = ., .data[[legend_id]] %in% names(spp_color))

    ggplot2::ggplot() +
      layer_borders(spl_extent = spl_bbox(filtered_specimens),
                    sf_county_color = "black") +
      layer_specimens(specimen_tbl = filtered_specimens,
                      id_column = legend_id, shape_aes = TRUE) +
      layer_themes(specimen_tbl = filtered_specimens,
                   id_column = legend_id, legend_title = legend_id) +
      coord_sf(xlim = range(spl_bbox(filtered_specimens)[["Longitude"]]),
               ylim = range(spl_bbox(filtered_specimens)[["Latitude"]]))

  })

  # Map Output ----
  output$mapPlot <- renderPlot(
    height = function() session$clientData$output_mapPlot_height,
    res = 96,
    {
      req(input$resetButton)
      plot_map() + theme(legend.position = "none")
    }
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
