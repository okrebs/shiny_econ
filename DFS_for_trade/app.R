#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyr)
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Ordered relative productivities"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("theta",
                     "Set the value of theta:",
                     min = 1,
                     max = 20,
                     step = 0.5,
                     value = 5),
         sliderInput("t_ratio",
                     "Set the realtive value T/T*:",
                     min = 0,
                     max = 10,
                     step = 0.25,
                     value = 1),
         numericInput("ymax",
                      "Y-axis maximum:",
                      value = 2.5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("MyPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$MyPlot <- renderPlot({
      
     # generate v-curve
     z <- seq(0, 1, 1/1000)
     A <- (input$t_ratio * ((1 - z) / z)) ^ (1 / input$theta) 
      mydf <- data.frame("x" = z, "y" = A) %>% na.omit
      ggplot(mydf,
        aes(y = y, x = x)) + geom_line() + ylim(0, input$ymax) + 
        xlim(0, 1)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

