#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(evd)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("DFS from random draws"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("theta",
                  "Set the value of theta:",
                  min = 1,
                  max = 10,
                  step = 0.1,
                  value = 5),
      sliderInput("t",
                  "Set T:",
                  min = 0,
                  max = 10,
                  step = 0.25,
                  value = 1),
      sliderInput("t_star",
                  "Set T*:",
                  min = 0,
                  max = 10,
                  step = 0.25,
                  value = 1),
      numericInput("n",
                   "Number of products:",
                   value = 10),
      numericInput("ymax",
                   "Y-axis maximum:",
                   value = 2.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow( verticalLayout(splitLayout(cellWidths = c("50%", "50%"), plotOutput("random_home_d"), plotOutput("random_foreign_d")), 
                               splitLayout(cellWidths = c("50%", "50%"), plotOutput("random_home"), plotOutput("random_foreign")),
                               splitLayout(cellWidths = c("50%", "50%"), plotOutput("relative"), plotOutput("A_curve"))))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  values <- reactive({
    data.frame(j = seq(0 + 1/input$n, 1, 1/input$n)) %>%
      mutate(z = rfrechet(input$n, loc=0,  scale=(input$t)^(1/input$theta), shape = input$theta),
             z_star = rfrechet(input$n, loc=0, scale=(input$t_star)^(1/input$theta), shape = input$theta),
             A_uns = z/z_star,
             A = sort(A_uns, decreasing = T))
  })
  
  output$random_home <- renderPlot({
    my_data <- values()
    mydf <- data.frame("x" = my_data$j, "y" = my_data$z)
    ggplot(mydf,
           aes(y = y, x = x)) + geom_point() + ylim(0, input$ymax) + 
      xlim(0, 1) + ggtitle("Realized/Drawn productivities Home") + xlab("j") + ylab("z(j)") +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
  })
  
  output$random_foreign <- renderPlot({
    my_data <- values()
    mydf <- data.frame("x" = my_data$j, "y" = my_data$z_star)
    ggplot(mydf,
           aes(y = y, x = x)) + geom_point() + ylim(0, input$ymax) + 
      xlim(0, 1) + ggtitle("Realized/Drawn productivities Foreign") + xlab("j") + ylab("z(j)") +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
  })
  
  output$random_home_d <- renderPlot({
    j = seq(0,10,1/100)
    mydf <- data.frame("x" = j, "y" = pfrechet(j, loc=0,  scale=(input$t)^(1/input$theta), shape = input$theta))
    ggplot(mydf,
           aes(y = y, x = x)) + geom_line() + ylim(0, 1) + 
      xlim(0, 10) + ggtitle("Frechet Distribution Home") + xlab("Z") +
      theme(axis.title.y=element_blank(), plot.title = element_text(size = 16, hjust = 0.5))
  })
  
  output$random_foreign_d <- renderPlot({
    j = seq(0,10,1/100)
    mydf <- data.frame("x" = j, "y" = pfrechet(j, loc=0,  scale=(input$t_star)^(1/input$theta), shape = input$theta))
    ggplot(mydf,
           aes(y = y, x = x)) + geom_line() + ylim(0, 1) + 
      xlim(0, 10) + ggtitle("Frechet Distribution Foreign") + xlab("Z") +
      theme(axis.title.y=element_blank(), plot.title = element_text(size = 16, hjust = 0.5))
  })
  
  output$relative <- renderPlot({
    my_data <- values()
    mydf <- data.frame("x" = my_data$j, "y" = my_data$A_uns)
    ggplot(mydf,
           aes(y = y, x = x)) + geom_point() + ylim(0, input$ymax) + 
      xlim(0, 1) + ggtitle("Relative productivities unsorted") + xlab("j") + ylab("A") +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
  })
  
  output$A_curve <- renderPlot({
    my_data <- values()
    mydf <- data.frame("x" = my_data$j, "y" = my_data$A)
    ggplot(mydf,
           aes(y = y, x = x)) + geom_line() + ylim(0, input$ymax) + 
      xlim(0, 1) + ggtitle("Relative productivities sorted") + xlab("j") + ylab("A") +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


