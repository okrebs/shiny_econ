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
   titlePanel("CES Indifference Curve with 2-goods"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("sigma",
                     "Set the value of sigma:",
                     min = 0,
                     max = 10,
                     step = 0.25,
                     value = 2),
         checkboxInput("infsigma",
                       "Let sigma approach infinity",
                       value = FALSE),
         sliderInput("utility",
                     "Set the shown level of utility:",
                     min = 0,
                     max = 10,
                     step = 0.1,
                     value = 2.5),
         numericInput("xmin",
                      "X-axis minimum:",
                      value = 0),
         numericInput("xmax",
                      "X-axis maximum:",
                      value = 15),
         numericInput("ymin",
                      "Y-axis minimum:",
                      value = 0),
         numericInput("ymax",
                      "Y-axis maximum:",
                      value = 15)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("CESPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$CESPlot <- renderPlot({
      
     # generate sigma based on input$sigma from ui.R
      sigma <- input$sigma
      if(input$infsigma == TRUE) {sigma <- Inf}
      utility <- input$utility
      x_1 <- seq(0, input$xmax, by = 0.001)
      tmp <- 2*utility^((sigma - 1) / sigma) - x_1^((sigma - 1) / sigma)
      if(sigma == 2) {
        x_2 <- ifelse(tmp < 0, NA, tmp^(sigma / (sigma - 1)))
      } else {
        x_2 <- tmp^(sigma / (sigma - 1))
      }
      x_2 <- round(x_2, 10)
      if (sigma == 0) {
        x_2 <- ifelse(x_1 == utility, max(x_1), utility)
        x_2 <- ifelse(x_1 < utility, NA, x_2)
      }
      if (sigma == 1) {x_2 <- utility^2 / x_1}
      if (sigma == Inf) {x_2 <- 2*utility - x_1}
      # draw the histogram with the specified number of bins
      mydf <- data.frame("x" = x_1, "y" = x_2) %>% na.omit
      ggplot(mydf,
        aes(y = y, x = x)) + geom_line() + ylim(input$ymin, input$ymax) + 
        xlim(input$xmin, input$xmax)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

