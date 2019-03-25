library(shiny)
library(RODBC)
library(dplyr)
library(plotly)
library(tidyverse)
library(leaflet)

#Query the database
connString <-
  paste0(
    "Driver=SQL Server;Server=kephalienterprises.database.windows.net;",
    "Database=LatinListings;",
    "Uid=kephalienterprises;Pwd=Dmichelle2015.;",
    "Encrypt=yes;"
  )


#This connection string was suggested for the Shiny Server; hasn't worked yet though
connString2 <- paste0(
  'Driver=FreeTDS;TDS_Version=7.0;",
  "Server=kephalienterprises.database.windows.net;Port=<port>;",
  "Database=LatinListings;Uid=kephalienterprises;Pwd=Dmichelle2015.;",
  "Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;'
)

conn <- odbcDriverConnect(connString)

#Maaaayyyy want to eliminate conditionals on this query; Even though some of the data is dirty, could potentially use it.

query <- paste(
  "Select * from dbo.v_Listings",
  "where [Price (USD)] <= 5000000",
  "and [Price (USD)] > 1000 and",
  "[Square Feet] < 20000 and",
  "[Square Feet] > 500 and",
  "Latitude != '' and",
  "Longitude != '';"
)

listings <- sqlQuery(conn, query)
close(conn)

listings$Bedrooms <- as.integer(listings$Bedrooms)
listings$Latitude <- as.numeric(listings$Latitude)
listings$Longitude <- as.numeric(listings$Longitude)

#Remove coordinate outliers
removeCoordsOutliers <- function(listings) {
  lats <- as.data.frame.numeric(listings$Latitude)
  longs <- as.data.frame.numeric(listings$Longitude)
  latFirstQ <- as.numeric(quantile(lats$`listings$Latitude`, 0.25))
  latThirdQ <- as.numeric(quantile(lats$`listings$Latitude`, 0.75))
  latIQR <- latThirdQ - latFirstQ
  longFirstQ <-
    as.numeric(quantile(longs$`listings$Longitude`, 0.25))
  longThirdQ <-
    as.numeric(quantile(longs$`listings$Longitude`, 0.75))
  longIQR <- longThirdQ - longFirstQ
  #on typical boxplot, m would be 1.5; I am opening it up some to prevent legitimate coords from being considered outliers
  m <- 6
  latll <- latFirstQ - m * latIQR
  latul <- latThirdQ + m * latIQR
  longll <- longFirstQ - m * longIQR
  longul <- longThirdQ + m * longIQR
  latOuts <-
    lats[lats$`listings$Latitude` < latll |
           lats$`listings$Latitude` > latul, ]
  longOuts <-
    longs[longs$`listings$Longitude` < longll |
            longs$`listings$Longitude` > longul, ]
  listings <-
    listings[!(listings$Latitude %in% latOuts) &
               !(listings$Longitude %in% longOuts), ]
  return(listings)
}

listings <- removeCoordsOutliers(listings)

countries <- sort(unique(listings$Country))

ui <- fluidPage(tags$head((tags$style(
  HTML(
    '#inputPanel {background-color: rgba(255,255,255,0.8);padding:20px;}'
  )
))),

titlePanel(
  HTML(
    "<h1>Latin Listings&trade;</h1><h4>by Kephali Enterprises, LLC</h4>"
  )
),

#absolutePanel(id = "inputPanel", style="z-index:500;", draggable = TRUE,
sidebarLayout(
  sidebarPanel(
    checkboxGroupInput(
      "countries",
      label = "Countries",
      choices = countries,
      selected = countries[1]
    ),
    
    sliderInput(
      "price",
      label = "Price Range (USD): ",
      value = c(50000, 500000),
      min = 10000,
      max = 5000000,
      step = 5000
    ),
    sliderInput(
      "beds",
      label = "Bedrooms: ",
      value = c(2, 4),
      min = min(listings$Bedrooms),
      max = max(listings$Bedrooms),
      step = 1
    ),
    checkboxInput("includeApproximations", "Include homes with approximated location", value = FALSE)
  ),
  mainPanel(tabsetPanel(
    tabPanel(
      "Map",
      leafletOutput("map", height = 500),
      verbatimTextOutput("count")
    ),
    tabPanel(
      "Charts",
      plotlyOutput("priceDist"),
      plotlyOutput("priceVSqFt")
    ),
    tabPanel("Summary", verbatimTextOutput("stats")),
    tabPanel("Raw Data")
  ))
))


#SERVER
server <- function(input, output) {
  
  mapData <- reactive({
    df <- listings[listings$Country %in% input$countries &
        listings$Bedrooms >= min(input$beds) & listings$Bedrooms <= max(input$beds) &
        listings$`Price (USD)` >= min(input$price) & listings$`Price (USD)` <= max(input$price),]
    if (!input$includeApproximations)
    {df <- df[df$CoordsApproximated == 0,]}
    df
  })
  
  #Draw map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery)
  })
  
  #Update UI when dataframe changes
  observeEvent(mapData(), {
    
    lngs <- c(min(mapData()$Longitude), max(mapData()$Longitude))
    lats <- c(min(mapData()$Latitude), max(mapData()$Latitude))
    
    #Update map markers
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lat = jitter(mapData()$Latitude, 2), lng = jitter(mapData()$Longitude, 2), 
                 color = ~mapData()$CoordsApproximated,
                 popup = paste(
                           mapData()$City,
                           "<br>",
                           "<a href =",
                           mapData()$URL,
                           ">",
                           mapData()$Header,
                           "</a>",
                           "<br>",
                           "<b>$",
                           mapData()$'Price (USD)',
                           "</b>"
                         )
                       ) %>%
                       setView(lng = median(lngs),
                               lat = median(lats),
                               zoom = 5)
    #Caption on map
    output$count <- renderPrint({
      cat(paste(as.character(nrow(mapData())), "homes waiting for you!"))
    })
    
    #Draw Price Distribution histogram
    output$priceDist <- renderPlotly({
      plot_ly(x = ~mapData()$'Price (USD)', type = "histogram") %>%
        layout(
          title = "Price Distribution",
          xaxis = list(title = "Price USD"),
          yaxis = list(title = "Properties"),
          margin = 30
        )
    })
    
    #Draw Price vs Sq Feet scatter chart
    output$priceVSqFt <- renderPlotly({
      plot_ly(mapData(),
              x =  ~ mapData()$'Price (USD)',
              y = ~ mapData()$'Square Feet') %>%
        add_markers() %>%
        layout(
          title = "Price vs Square Feet",
          xaxis = list(title = "Price USD"),
          yaxis = list(title = "Square Feet"),
          margin = 30
        ) %>%
        hide_colorbar()
    })
    
    #Write out summary of selected data
    output$stats <- renderPrint({
      summary(mapData() %>% select("Price (USD)", "Square Feet", "Bedrooms"))
    })
    
  }, ignoreNULL = FALSE)
  
  
}

shinyApp(ui = ui, server = server)
