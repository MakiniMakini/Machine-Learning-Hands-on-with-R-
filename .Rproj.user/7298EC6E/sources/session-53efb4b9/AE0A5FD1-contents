# Load the libraries
# install.packages("dygraphs")
library(shiny)
library(sf)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(dplyr)

# Set working directory
setwd("~/MAKINI CODES/CEMA/MAP-AMR/Dashboard/appdir/appdir")

data <- read.csv("data/data.csv")
map <- st_read("data/fe_2007_39_county/fe_2007_39_county.shp")

# UI object
ui <- fluidPage(
  titlePanel(p("MAP-AMR KENYA", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      p("Made by", a("CEMA",
                     href = "http://cema-africa.uonbi.ac.ke/"
      ), "."),
      img(
        src = "kenya-logo.jpg",
        width = "70px", height = "70px"),
      width = 3),
    mainPanel(
      fluidRow(
        column(12,
               leafletOutput(outputId = "map")
        ),
        fluidRow(
          column(6,
                 dygraphOutput(outputId = "timetrend")
          ),
          column(6,
                 DTOutput(outputId = "table")
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output) {
  output$table <- renderDT(data)
  
  output$timetrend <- renderDygraph({
    dataxts <- NULL
    counties <- unique(data$county)
    for (l in 1:length(counties)) {
      datacounty <- data[data$county == counties[l], ]
      dd <- xts(
        datacounty[, "cases"],
        as.Date(paste0(datacounty$year, "-01-01"))
      )
      dataxts <- cbind(dataxts, dd)
    }
    colnames(dataxts) <- counties
    
    dygraph(dataxts) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
    
    d1$x$css <- "
      .dygraph-legend > span {display:none;}
      .dygraph-legend > span.highlight { display: inline; }
    "
    d1
  })
  
  output$map <- renderLeaflet({
    # Add data to map
    datafiltered <- data[data$year == 1980, ]
    map <- left_join(map, datafiltered, by = c("NAME" = "county"))
    
    # Create leaflet
    pal <- colorBin("YlOrRd", domain = map$cases, bins = 7)
    
    labels <- sprintf("%s: %g", map$NAME, map$cases) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(cases),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~cases,
        opacity = 0.7, title = NULL
      )
    l
  })
}

# Shiny app
shinyApp(ui = ui, server = server)
