#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
total_match <- read.csv("total_match.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    selectInput("Name", "Name",
                choices = c("All", unique(as.character(total_match$Name)))),
    selectInput("ID", "ID",
                choices = c("All", unique(as.character(total_match$Car_ID)))),
    selectInput("Employment_Type","Employment_Type",
                choices = c("All",unique(as.character(total_match$CurrentEmploymentType)))),
    selectInput("Employment_Title","Employment_Title",
                choices = c("All",unique(as.character(total_match$CurrentEmploymentTitle)))),
    DT::dataTableOutput("table")
)

server <- function(input, output) {
    filtered_data <- reactive({
        data <- total_match
        if (input$Name != "All") {
            data <- subset(
                data,
                Name == input$Name
            )
        }
        if (input$ID != "All") {
            data <- subset(
                data,
                Car_ID == input$ID
            )
        }
        if (input$Employment_Type != "All") {
            data <- subset(
                data,
                CurrentEmploymentType == input$Employment_Type
            )
        }
        if (input$Employment_Title != "All") {
            data <- subset(
                data,
                CurrentEmploymentTitle == input$Employment_Title
            )
        }
        data
    })
    
    # Replace the renderTable() with DT's version
    output$table <- DT::renderDataTable({
        data <- filtered_data()
        data
    })
    
}

shinyApp(ui, server)