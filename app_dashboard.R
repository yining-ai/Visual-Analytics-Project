library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

packages = c('igraph', 'tidygraph', 'ggraph', 'visNetwork', 'lubridate', 'clock', 'tidyverse','dplyr', 'tidyr','raster','sf','sp','tmap', 'gifski', 'writexl', 'mapview', "ggplot2", 'data.table')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

# Load the credit card dataset in the environment.
credit_card <- read.csv("Group_Project/cc_data.csv")

# Load the loyalty card dataset in the environment.

loyalty_card <- read.csv("Group_Project/loyalty_data.csv")

# Since some payment might not use loyalty card to record, the price will automatically goes to another column. Therefore, we need to put them back to the correct column.
loyalty_card$loyaltynum <- ifelse(is.na(loyalty_card$loyaltynum), loyalty_card$price, loyalty_card$loyaltynum)
loyalty_card <- loyalty_card %>% mutate(price = ifelse(price %like% 'L', 0, price))
loyalty_card$price <- as.numeric(loyalty_card$price)


# Change the datatype of variable timestamp from character to date-time format.
credit_card$timestamp <- date_time_parse(credit_card$timestamp,
                                         zone = "",
                                         format = "%m/%d/%Y %H:%M")

# Change datatype of variable timestamp from character to date format.
loyalty_card$timestamp <- date_time_parse(loyalty_card$timestamp,
                                          zone = "",
                                          format = "%m/%d/%Y")


# Create a new column "Date" in the credit card dateset
credit_card$Date <- format(credit_card$timestamp, format="%Y-%m-%d")
credit_card$Date <- date_time_parse(credit_card$Date,
                                    zone = "",
                                    format = "%Y-%m-%d")

# Join the credit card and loyalty card data
card_joined <- credit_card %>%
  full_join(loyalty_card, by = c("Date" = "timestamp", "location", "price"))


# Frequency of visit
popular_credit_card <- credit_card %>%
  group_by(location, Date) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
  



# Build shiny app

ui <- dashboardPage(
    dashboardHeader(title = "GAStech Investigation"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("EDA", tabName = "eda", icon = icon("chart-bar"),
                     menuItem("Heatmap",
                              tabName = "heatmap"),
                     menuItem("Ridge Plot",
                              tabName = "ridgeplot"),
                     menuItem("Bar Chart",
                              tabName = "barchart"),
                     menuItem("One-to-One Matching",
                              tabName = "matching1")),
            menuItem("MAP", tabName = "map", icon = icon("map"))
        )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "eda",
                "text"),
        tabItem(tabName = "heatmap", 
                fluidRow( 
                  box(dateInput(
                    inputId = "date" ,
                    label = "Date",
                    value = NULL,
                    min = "2014-01-05",
                    max = "2014-01-20",
                    format = "yyyy-mm-dd",
                    startview = "month",
                    language = "en",
                    width = NULL,
                    autoclose = TRUE,
                    datesdisabled = NULL,
                    daysofweekdisabled = NULL),
                    plotOutput("heatmap1"),
                    width = NULL)
                  ) 
                ),
        tabItem(tabName = "ridgeplot"),
        tabItem(tabName = "barchart",
                fluidRow(
                  box(dateInput(inputId = "date_bar",
                                     label = "Select date:",
                                     min = "2014-01-06",
                                     max = "2014-01-19", 
                                     format = "yyyy-mm-dd", startview = "month",
                                     weekstart = 0,
                                     language = "en",
                                     width = NULL)),
                  box(selectInput(inputId = "n_rank",
                                label = "Choose a number",
                                choices = c(
                                  "Top 5" = 5,
                                  "Top 6" = 6,
                                  "Top 7" = 7,
                                  "Top 8" = 8,
                                  "Top 9" = 9,
                                  "Top 10" = 10),
                                selected = "5")),
                  box(width = 12,
                      plotOutput("barchart1")),
        tabItem(tabName = "matching1")
        ) 
      )
     
      ),
 
  )
)

server <- function(input, output) {
  output$heatmap1 <- renderPlot({
    ggplot(data=popular_credit_card,
           aes(x = popular_credit_card$Date,
               y = popular_credit_card$location)) +
      geom_tile()
  })
  
  
  
  # error
  filt_df <- reactive({
    popular_credit_card %>%
      filter(Date == input$date_bar) %>%
      arrange(desc(count)) %>%
      slice(1:input$n_rank)
  })
  
  output$barchart1 <- renderPlot({
    ggplot(filt_df(),
           aes(x = filt_df()$count,
               y = filt_df()$location)) +
      geom_col()
  })
}


shinyApp(ui, server)




