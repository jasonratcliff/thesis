library(shiny)
library(knitr)

library(ThesisPackage)
library(magrittr)
library(dplyr)

ui <- fluidPage(
  titlePanel("Morphology Check"),
  mainPanel(
    fluidRow(
      column(3,
        textInput("collector", label = "Collector", value = NA),
        textInput("collection", label = "Collection", value = NA),
        actionButton("search", "Search!")
      ),
    ),
    "<br>",
    tabPanel("Specimens", tableOutput("morphologyTable"))
  )
)

server <- function(input, output, session) {
  search <- reactive({
    req(input$search)
    ThesisPackage::herbarium_specimens %>%
      dplyr::filter(.data = .,
        grepl(pattern = input$collector, x = .data$Collector),
        grepl(pattern = input$collection, x = .data$Collection_Number)
      )
  })
  
  output$morphologyTable <- function() {
     search() %>%
      dplyr::select(.data = ., Collector, Collection_Number,
                    prior_id, Taxon_a_posteriori, ID, Herbarium, App.A, Imaged,
                    Fruit_trichomes, Ovule_number, Replum_shape,
                    Basal_leaf_margins, Mature_fruit_apices) %>%
      kable(x = .) %>%
      kableExtra::kable_styling(kable_input = ., 
                                bootstrap_options = "bordered")
  }
}

# Run app ----
shinyApp(ui, server)
