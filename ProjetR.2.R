if (!require(shinythemes)) {
  install.packages("shinythemes")
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}
if (!require(leaflet)) {
  install.packages("leaflet")
}
if (!require(leaflet.extras)) {
  install.packages("leaflet.extras")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}


library(shiny)
library(leaflet)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(leaflet.extras)
library(httr)
library(jsonlite)
library(RMySQL)
library(tidygeocoder)
library(tidyr)
library(dplyr)


#Obtention des données

base<-'https://api.jcdecaux.com/vls/v1/stations?contract='
contract<-'Lyon'
key<-'&apiKey=b59e18c899829b0903cc948041c9286aecc68768'

API_URL<-paste0(base, contract, key)

data <- read.csv("Excel.csv")
adresse_station <-read.csv("Excel.csv")

ui <- dashboardPage(skin="red",
                    
                    #Titre du dashboard  
                    
                    dashboardHeader(title = "Vélo'v"),
                    
                    #Menus sur le côté
                    
                    dashboardSidebar(
                      sidebarMenu(
                        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                          label = "Search..."),
                        menuItem("Tableau de bord", tabName = "Dashboard", icon = icon("dashboard")),
                        menuItem("Table", tabName = "Table", icon = icon("th")),
                        actionButton("reload_button", "Reload Data"),
                        
                        
                        selectInput("postcode_filter", "Select Postcode:",
                                    choices = c("", unique(data$postcode)), # Use unique_postcodes
                                    selected = "",
                                    multiple = TRUE)
                         
                      )
                    ),
                    
                    #Contenue du menus
                    
                    dashboardBody(
                      
                      #Script CSS pour le titre du dashboard
                      
                      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                      
                      tabItems(
                        
                        #Premier menus      
                        
                        tabItem(tabName = "Dashboard",
                                
                                fluidRow(
                                  # Nombre de vélos disponible
                                  valueBox(
                                    value = sum(data$available_bikes),
                                    subtitle = "Nombre total de vélos disponibles",
                                    width=3,
                                    icon = icon("bicycle"),
                                    color = "blue"                      # You can adjust the color as needed
                                  ),
                                  
                                  #Nombre total de vélos
                                  valueBox(
                                    value = sum(data$available_bike_stands),  # Calculate the total number of bikes
                                    subtitle = "Nombre de places disponibles",
                                    width=3,
                                    icon = icon("bicycle"),
                                    color = "red"  # You can adjust the color as needed
                                  ),
                                  valueBox(
                                    value=length(data$number),
                                    subtitle = "Nombre de bornes",
                                    width=3,
                                    icon=icon("square-parking"),
                                    color="blue"
                                  ),
                                  valueBox(
                                    value=nrow(data[data$status=="CLOSED",]),
                                    subtitle = "Nombre de bornes fermés",
                                    width=3,
                                    icon=icon("square-parking"),
                                    color="red"
                                  )
                                  
                                  
                                  
                                ),
                                
                                
                                fluidRow(
                                  #Carte
                                  box(title = "Carte", width = 6, solidHeader = TRUE, leafletOutput("map"), color="red"),
                                  #Top 10 stations avec le plus de vélos
                                  box(title = "Top 10 des stations avec le plus de vélos", width = 6, plotOutput("top_10_stations_chart")),
                                  
                                ),
                                
                                fluidRow(
                                  box(title = "Nombre de stations à bonus", width = 4, plotOutput("bonus_vs_no_bonus_plot")),
                                  box(title = "Status de la station", width = 4, plotOutput("graphique2")),
                                  box(title = "Dispersion des emplacements de vélos disponibles par rapport aux emplacements de vélos totaux", width = 4, plotOutput("scatter_plot")),
                                ),
                                fluidRow(
                                  box(
                                  title = "Reload Status",
                                  width = 5,
                                  verbatimTextOutput("reload_status")) )                   
                        ),
                        
                        #Deuxième menus
                        
                        tabItem(tabName = "Table",
                                tableOutput("table")
                        )
                      )
                    )
)

server <- function(input, output, session) {
  
  d <- reactive({
    data
  })
  
  output$graphique1 <- renderPlot({
    data_filtered <- data %>%
      arrange(desc(available_bikes)) %>%
      head(10)
    
    data_filtered$modified_name <- substr(data_filtered$name, 8, nchar(data_filtered$name))
    
    ggplot(data_filtered, aes(x = modified_name, y = available_bikes)) +
      geom_bar(stat = "identity") +
      labs(x = "Name", y = "Nombre de vélos disponibles", title = "Nombre de vélo disponible par station")
  })
  
  
  output$graphique2 <- renderPlot({
    status_counts <- table(data$status)
    data <- data.frame(status = names(status_counts), count = as.vector(status_counts))
    
    ggplot(data, aes(x = "", y = count, fill = status)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Distribution des Status des Station") +
      theme_void()
  })
  
  output$bonus_vs_no_bonus_plot <- renderPlot({
    ggplot(data, aes(x = bonus, fill = bonus)) +
      geom_bar() +
      labs(title = "Bonus vs. Stations sans bonus", x = "Bonus activé", y = "Nombre de stations") +
      scale_fill_manual(values = c("yes" = "blue", "no" = "red")) +
      theme_minimal()
  })
  output$top_10_stations_chart <- renderPlot({
    top_10_stations <- data %>%
      arrange(desc(available_bikes)) %>%
      head(10)
    
    ggplot(top_10_stations, aes(x = reorder(name...3, -available_bikes), y = available_bikes)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 des stations les plus utilisées", x = "Nom de la station", y = "Nombre de vélos disponibles") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_x_discrete(name = "Nom de le station")
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(data, aes(x = bike_stands, y = available_bike_stands)) +
      geom_point() +
      labs(title = "Vélos disponibles par rapport aux emplacements de vélos totaux",
           x = "Emplacements de vélos totaux",
           y = "Emplacements de vélos disponibles")
  })
  
  df <- data
  df$unavailable_bike_stands <- df$bike_stands - df$available_bike_stands
  

  
  output$map <- renderLeaflet({
    filtered_data <- data
    if (!is.null(input$postcode_filter) && !("" %in% input$postcode_filter)) {
      filtered_data <- filtered_data[filtered_data$postcode %in% input$postcode_filter, ]
    }
    
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = filtered_data,
        lat = ~`position.lat`,
        lng = ~`position.lng`,
        popup = ~paste("Code Postal: ", postcode, "<br>",
                       "Vélos diponibles: ", available_bikes, "<br>",
                       "Emplacements de vélos diponibles: ", available_bike_stands)
      )
  })
  
  last_reload_time <- reactiveVal(NULL)
  
  # Reload button action
  observeEvent(input$reload_button, {
    # Perform data reload here
    # Example: Reload your data
    data <- read.csv("Excel.csv")
    
    # Update the last reload time
    last_reload_time(Sys.time())
  })
  
  # Display the reload status including the last update time
  output$reload_status <- renderPrint({
    reload_time <- last_reload_time()
    if (!is.null(reload_time)) {
      paste("Last Data Reload:", format(reload_time, format = "%Y-%m-%d %H:%M:%S"))
    } else {
      "Data has not been reloaded yet."
    }
  })

  # Observer to handle data reload
  observeEvent(input$reload_button, {
    # Reload data from the API using the API URL
    raw_data <- GET(API_URL)
    updated_data <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
    
    # join the two dataframes
    merged_df <- bind_cols(updated_data, adresse_station %>%
                             select(-one_of(names(updated_data)))
    )
    # Update the 'data' variable with the newly loaded data
    data <- updated_data
  })
  
  output$table <- renderTable({
    d()
  })
  
  output$summary <- renderPrint({
    summary(d())
  })
  
  output$plot <- renderPlot({
    data$available_bikes
    hist(data$available_bikes,
         main = "histo",
         col = "#75AADB", border = "white", breaks = input$n)
  })
}

shinyApp(ui, server)