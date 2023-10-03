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
    titlePanel(
      h1("Representación social de las nuevas tecnologías", align = "center")
      ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("carrera",
                        "Seleccione una carrera:",
                        choices = unique(df$carrera)),
            selectInput("nivel_estudios",
                        "Seleccione un nivel de estudios:",
                        choices = unique(df$nivel_estudios)),
            selectInput("estimulo",
                        "Seleccione un estímulo:",
                        choices = unique(df$estimulo)
                        ),
            selectInput("sexo", "Seleccione un sexo:",
                         choices = c("Todos", "varon", "mujer", "otro"),
                         selected = "Todos")
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barplot")
        )
    )
)


server <- function(input, output) {
  filteredData <- reactive({
    selected_sex <- input$sexo
    
    if (selected_sex == "Todos") {
      filtered_df <- df
    } else {
      filtered_df <- df[df$sexo %in% selected_sex, ]
    }
    
    filtered_df <- filtered_df %>%
      filter(carrera == input$carrera,
             estimulo == input$estimulo)
    
    return(filtered_df)
  })
  

  
  
  output$barplot <- renderPlot({
    data <- filteredData()
    frecuencia_palabra <- table(data$palabra)
    
      if (length(frecuencia_palabra) == 0) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
        text(0.5, 0.5, "No hay datos para 'otro'", col = "red")
        } else
          {term_freq_df <- as.data.frame(frecuencia_palabra)
          colnames(term_freq_df) <- c("palabra", "frecuencia")
          limite_minimo <- 2
          palabras_filtradas <- term_freq_df %>% 
          filter(frecuencia >= limite_minimo)
          
          ggplot(palabras_filtradas, aes(x = palabra, y = frecuencia)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    
    
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
