library(shiny)
library(magrittr)
library(knitr)
library(ggplot2)
library(ThesisPackage)

# Distribution Plotting UI ----
ui <- fluidPage(
  titlePanel("Physaria Spatial Analysis"),
  mainPanel(
    sidebarPanel(actionButton("resetButton", "Reset Map")),
    tabsetPanel(
      
      # Map plot of specimen distribution.
      tabPanel("Distribution",
        fluidRow(br(),
                 column(6, tableOutput("mapRange"))),
        fluidRow(plotOutput("mapPlot",
                            brush = brushOpts(id = "mapBrush", delay = 500,
                                              resetOnNew = TRUE))),
        fluidRow(plotOutput("mapLegend"))),
      
      # Select parameter for geom aesthetic layer.
      tabPanel("Parameters", br(),
        fluidRow(
          selectInput(inputId = "map_color_aes", label = "Mapped Taxa ID",
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
                    label = "Collection Number"))),

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
    specimens$data <-
      ThesisPackage::subset_coords(specimen_tbl = specimen_data,
        Longitude = c(brush_dim()$xmin, brush_dim()$xmax),
        Latitude = c(brush_dim()$ymin, brush_dim()$ymax))

    # Render table of brushed specimens.
    output$specimens <- renderTable({
      specimens$data %>%
        dplyr::select("prior_id", "Taxon_a_posteriori",
                      "Collector", "Collection_Number")
      })
    })

  # ggplot Reactive ----
  plot_map <- reactive({
    map_specimens(specimen_tbl = specimens$data,
                  id_column = input$map_color_aes,
                  jitter_pos = c(0.035, 0.035), f_adj = 0.075) +
      guides(color = guide_legend(ncol = 2, byrow = FALSE))
  })

  # Map Output ----
  output$mapPlot <- renderPlot({
    req(input$resetButton)
      plot_map() + theme(legend.position = "none")
    }, height = function() {
      session$clientData$output_mapPlot_height
      })

  # Legend Output ----
  output$mapLegend <- renderPlot({
    if (!is.null(specimens$data)) {
      cowplot::plot_grid(cowplot::get_legend(plot_map()))
      }
    }, height = function() {
      session$clientData$output_mapLegend_height
      })

  # TODO Individual Specimen ID
  # if (isolate(input$spp_find) == TRUE) {
  #   req(input$collector_id)
  #   req(input$collection)
  #   spp_distribution <-
  #     spp_find(gg_map_obj = spp_distribution,
  #              taxa_frame = specimen_subset(),
  #              collector = isolate(input$collector_id),
  #              collection_number = isolate(input$collection))
  # }

}

# Run app ----
shinyApp(ui, server)

