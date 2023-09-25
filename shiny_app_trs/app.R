#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)


prompts_csv <- "data/estimulos_todos.csv"
prompts <- read.csv(prompts_csv)
demographic_csv <- "data/sociodemograficos_todos.csv"
demographic <- read.csv(demographic_csv)
words_csv <- "data/terminos_todos.csv"
words <- read.csv(words_csv)


df <- prompts %>%
  inner_join(demographic, by = "id") %>%
  inner_join(words, by = "id")
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("slate"),

    # Application title
    titlePanel("Qué piensan las personas de las nuevas tecnologías"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("carrera",
                        "Seleccione una carrera:",
                        choices = unique(df$carrera)),
            selectInput("estimulo",
                        "Seleccione un estímulo:",
                        choices = unique(df$estimulo)
                        ),
            sliderInput("edad",
                        "Seleccione un rango de edad:",
                        min = 18, max = 99, value = c(18, 99)),
            radioButtons("sexo", "Seleccione un sexo:",
                                     choices = c("varon", "mujer", "otro"),
                                     selected = "varon")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("filteredData")
        )
    )
)


server <- function(input, output) {
  filteredData <- reactive({
    filtered_df <- df %>%
      filter(carrera == input$carrera,
             estimulo == input$estimulo,
             edad >= input$edad[1] & edad <= input$edad[2],
             sexo == input$sexo)
    return(filtered_df)
  })
  
  output$filteredData <- renderTable({
    filteredData()
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
