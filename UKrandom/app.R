library(shiny)
library(dplyr)
library(leaflet)
library(raster)

# downloads map of great britain if it has not alread been downloaded
gbr = getData("GADM", country="GBR", level=0)

# Checking whether a random sample point is in Great Britain is really slow
# Therefore we do this initially for a (very) large sample. Later on we will
# then take small samples from these points which is much faster as we do not 
# need to check again whether they are in Great Britain. 
# The large sample is only generated on the first run and saved to disk. This
# takes a few minutes but only has to be done once.
if(!file.exists("sample_data.rds")) {
   spsample(gbr, 10000, type="random") %>% saveRDS("sample_data.rds")
}
all_points <- readRDS("sample_data.rds") 

# Set the user interface
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput(inputId = "my_n", label="Number", min=0, max=200,
                            value = 10, step = 10),
                actionButton("new_sample", "New Sample"))
)

server <- function(input, output, session) {
  
  # function to generate new sample of size my_n from all points calculated
  # initially
  new_sample <- function() {sample(all_points, input$my_n)}
  
  # We want the sample to react to changes in my_n and when the button is 
  # pressed so it needs to be a reactive value
  v <- reactiveValues(my_sample = NULL)
  
  # generate new sample data if my_n changes
  observe(v$my_sample <- new_sample())
  
  # generate new sample data if button is pressed
  observeEvent(input$new_sample, 
               {v$my_sample = new_sample()})
  
  output$map <- renderLeaflet({ #"map" here refers to the "map" in the ui above
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = gbr) %>% 
      addTiles() %>% addPolygons(stroke=FALSE) 
  })
  
  # Change the map everytime the sample changes
  observe({
    leafletProxy("map") %>%
      clearShapes() %>% 
      addCircles(data=v$my_sample, 
                 radius=10000, color = "#000000", stroke=FALSE, fill=TRUE,
                 fillColor = "#000000", fillOpacity = 1)
    })
}

shinyApp(ui, server)



