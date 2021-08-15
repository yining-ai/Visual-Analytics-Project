


packages = c('shinydashboard', 'igraph', 'tidygraph', 'ggjoy', 'clock', 'ggraph', 'visNetwork', 'lubridate', 'tidyverse','dplyr', 'tidyr','raster','sf','sp','tmap', 'gifski', 'writexl', 'mapview', 'ggplot2', 'data.table')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

# Load the credit card dataset in the environment.
credit_card <- read.csv("data_MC2/cc_data.csv")

# Load the loyalty card dataset in the environment.

loyalty_card <- read.csv("data_MC2/loyalty_data.csv")

# Load the dataset including all employees' information

credit_card_category <- read.csv("data_MC2/cc_data_category.csv")

# Load the data of employee information

total_match <- read.csv("data_MC2/total_match.csv")

# Since some payment might not use loyalty card to record, the price will automatically goes to another column. Therefore, we need to put them back to the correct column.
loyalty_card$loyaltynum <- ifelse(is.na(loyalty_card$loyaltynum), loyalty_card$price, loyalty_card$loyaltynum)
loyalty_card <- loyalty_card %>% mutate(price = ifelse(price %like% 'L', 0, price))
loyalty_card$price <- as.numeric(loyalty_card$price)


# Change the datatype of variable timestamp from character to date-time format.
credit_card$timestamp <- date_time_parse(credit_card$timestamp,
                                         zone = "",
                                         format = "%m/%d/%Y %H:%M")

credit_card_category$timestamp <- date_time_parse(credit_card_category$timestamp,
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

# Create a new column "Hour" in the credit card category dataset
credit_card_category$Hour <- format(credit_card_category$timestamp,
                                    format = "%H")




# Join the credit card and loyalty card data
card_joined <- credit_card %>%
  full_join(loyalty_card, by = c("Date" = "timestamp", "location", "price"))


# Frequency of visit
popular_credit_card_shiny <- credit_card %>%
  group_by(location, Date) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


  
# Load the map data
bgmap <- raster("data_MC2/MC2-tourist.tif")

# Plot raster layer




# Load gps data
gps2 <- read_csv("data_MC2/gps2.csv")


# Change the datatype of variable Timestamp.
gps2$Timestamp <- date_time_parse(gps2$Timestamp,
                                  zone = "",
                                  format = "%m/%d/%Y %H:%M")
gps2$id <- as_factor(gps2$id)


# Combine longtitude and latitude as a coordination point
gps_sf <- st_as_sf(gps2, 
                   coords = c("long", "lat"),
                   crs= 4326)

# To facilitate filtering, variables "day", "hour" and "minute" can be extracted.
gps_sf$day <- format(gps_sf$Timestamp, format="%d")
gps_sf$day <- as.numeric(gps_sf$day)
gps_sf$hour <- format(gps_sf$Timestamp, format="%H")
gps_sf$hour <- as.numeric(gps_sf$hour)
gps_sf$minute <- format(gps_sf$Timestamp, format="%M")
gps_sf$minute <- as.numeric(gps_sf$minute)

# select those time intervals are longer than 3 mins
more_than_3mins <- gps_sf %>%
  filter(Seconds >180)







# Build shiny app

ui <- dashboardPage(
    dashboardHeader(title = "GAStech Investigation"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("EDA", tabName = "eda", icon = icon("chart-bar"),
                     
                     menuItem("Ridge Plot",
                              tabName = "ridgeplot"),
                     menuItem("Bar Chart",
                              tabName = "barchart"),
                     menuItem("Employee Info",
                              tabName = "datatable")),
            menuItem("MAP", tabName = "map", icon = icon("map"),
                     menuItem("Line Graph", 
                              tabName = "linegraph"),
                     menuItem("Dot Graph",
                              tabName = "dotgraph"))
        )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "eda",
                "text"),
       
        tabItem(tabName = "ridgeplot",
                fluidRow(
                  box(selectInput(inputId = "category",
                                  label = "Choose a category:",
                                  choices = c("Restaurant",
                                              "Coffee shop",
                                              "Company",
                                              "Public Place",
                                              "Shops"))),
                  box(selectInput(inputId = "unknown_shops",
                                  label = "Choose a location:",
                                  choices = c("Abila Scrapyard",
                                              "Abila Zacharo",
                                              "Hippokampos",
                                              "Kalami Kafenion",
                                              "Kronos Pipe and Irrigation",
                                              "Octavio's Office Supplies",
                                              "Shoppers' Delight",
                                              "Stewart and Sons Fabrication"
                                              ))),
                  box(width=6,
                      plotOutput("ridge1")),
                  box(width=6,
                      plotOutput("ridge2"))
                )),
        tabItem(tabName = "barchart",
                fluidRow(
                  box(dateInput(inputId = "date_bar",
                                  label = "Select date:",
                                  value = "2014-01-06",
                                  min = "2014-01-06",
                                  max = "2014-01-19", 
                                  format = "yyyy-mm-dd", startview = "month",
                                  weekstart = 0,
                                  language = "en",
                                  width = NULL
                                  
                                     )),
                  box(numericInput(inputId = "n_rank",
                                label = "Choose a number",
                                6,
                                min = 5,
                                max = 10,
                                step = 1)
                                ),
                  
                  
                  box(width = 12,
                      plotOutput("barchart1")))),
        
        tabItem(tabName = "datatable",
                fluidRow(
                  box(
                      title = "Inputs",
                      selectInput("Name", "Name",
                                  choices = c("All", unique(as.character(total_match$Name)))),
                      selectInput("ID", "ID",
                                  choices = c("All", unique(as.character(total_match$Car_ID)))),
                      selectInput("Employment_Type","Employment_Type",
                                  choices = c("All",unique(as.character(total_match$CurrentEmploymentType)))),
                      selectInput("Employment_Title","Employment_Title",
                                  choices = c("All",
                                              unique(as.character(total_match$CurrentEmploymentTitle))))
                      
                  )),
                fluidRow(
                  box(width=12,
                      DT::dataTableOutput("table")))
                  ),
        tabItem(tabName = "map"),
        tabItem(tabName = "linegraph",
                fluidRow(
                  box(
                    title = "Inputs",
                    selectInput("id","Choose ID:",
                                choices = c(1,2,3,4,5,6,7,9,11,12,13,
                                            14,15,16,17,18,19,20,21,
                                            23,24,25,26,27,28,29,30,
                                            31,32,33,35)),
                    sliderInput("day","Choose day:",
                                min = 6,
                                max = 19,
                                value = 6,
                                step = 1))),
                fluidRow(
                  box(width = NULL,
                      plotOutput("mapPlot1"))
               
                )),
        tabItem(tabName = "dotgraph",
                fluidRow(
                  box(
                    title = "Inputs",
                    selectInput("id2","Choose ID:",
                                choices = c(1,2,3,4,5,6,7,9,11,12,13,
                                            14,15,16,17,18,19,20,21,
                                            23,24,25,26,27,28,29,30,
                                            31,32,33,35)),
                    sliderInput("day2","Choose day:",
                                min = 6,
                                max = 19,
                                value = 6,
                                step = 1),
                    sliderInput("hour2", "Choose hour:",
                                min = 1,
                                max = 24,
                                value = 8,
                                step = 1),
                    sliderInput("minute2", "Choose minute:",
                                min = 0,
                                max = 59,
                                value = 3,
                                step = 1))),
                fluidRow(
                  box(width = NULL,
                    plotOutput("mapPlot2"))
                  
                  
                ))
        
        ) 
      
     
      )
 
    )


server <- function(input, output,session){
  # reactive dataset1
  filt_ridge1 <- reactive({
    
    rdg1 <- credit_card_category %>%
      filter(Category == input$category)
    
  })
  
  
  # build ridge plot1
  output$ridge1 <- renderPlot({
    ggplot(filt_ridge1(),
           aes(x = as.numeric(Hour),
               y = location,
               fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
      scale_x_continuous(breaks = 0:24) +
      scale_y_discrete(limits=rev) +
      scale_fill_viridis_c(name = "ABC", option = "A") +
      theme_ridges(font_size = 7, grid = TRUE) +
      theme(legend.position = "none") +
      labs(title = "Visiting Frequency of Different Location Types")
    
  })
  
  # reactive dataset1
  filt_ridge2 <- reactive({
    
    rdg2 <- credit_card_category %>%
      filter(location == input$unknown_shops)})
  
  # build ridge plot2
  output$ridge2 <- renderPlot({
    ggplot(filt_ridge2(),
           aes(x = as.numeric(Hour),
               y = location,
               fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
      scale_x_continuous(breaks = 0:24) +
      scale_y_discrete(limits=rev) +
      scale_fill_viridis_c(name = "ABC", option = "A") +
      theme_ridges(font_size = 7, grid = TRUE) +
      theme(legend.position = "none") +
      labs(title = "Uncovering Location Category")
  })
  
  
  #build tab barchart
  filt_df <- reactive({
    
    fdf <- popular_credit_card_shiny %>%
      filter(Date == input$date_bar) %>%
      arrange(desc(count))
    
    head(fdf, input$n_rank)
    
  })
  
  
  output$barchart1 <- renderPlot({
    ggplot(filt_df(),
           aes(x = filt_df()$count,
               y = filt_df()$location)) +
      geom_text(aes(label = filt_df()$count), hjust = -1, colour = "white") +
      geom_col(fill="lightblue") +
      labs(x = "count",y = "location")+
      theme_dark()
  })
  
  # build the datatable
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
  
  
  #build the tmap1
  output$mapPlot1 <- renderPlot({
    mapdata1 <- gps_sf %>%
      filter(id == input$id,
             day == input$day)
    
    tmap_mode("plot")
    tm_shape(bgmap) +
      tm_rgb(bgmap, r = 1,g = 2,b = 3,
             alpha = NA,
             saturation = 1,
             interpolate = TRUE,
             max.value = 255) +
      tm_shape(mapdata1) +
      tm_dots()
  })
  
  #build the tmap2
  output$mapPlot2 <- renderPlot({
    mapdata2 <- more_than_3mins %>%
      filter(day == input$day2,
             hour == input$hour2)
    
    tmap_mode("plot")
    tm_shape(bgmap) +
      tm_rgb(bgmap, r = 1,g = 2,b = 3,
             alpha = NA,
             saturation = 1,
             interpolate = TRUE,
             max.value = 255) +
      tm_shape(mapdata2) +
      tm_dots()
  })

 
}




shinyApp(ui, server)




