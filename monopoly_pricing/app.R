#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)

global_df <- data.frame(Runde = numeric(), Preis = numeric())
max_rounds <- 5
gk <- 3 # Grenzkosten
pass <- "password" # reset password

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Show notifications in the middle of the screen
   tags$head(
     tags$style(
       HTML(".shiny-notification {
              position:fixed;
              top: calc(50%);;
              left: calc(50%);;
              }
              ")
    )
  ),
  
   # Application title
   titlePanel("Monopolspiel"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "output.showresults == false",
          p(paste0("Nehmen Sie an, Sie expandieren mit Ihrer Hallig-Kneipe auf die Nachbarhallig Nordstrandischmoor. Dort eröffnen Sie die erste und damit einzige Kneipe. Sie kennen die Nachfragesituation auf der Insel nicht, auch wenn Sie wissen, dass die Hallig mit 18 Einwohnern doppelt so groß ist, wie die Ihnen bekannte Insel Gröde."), 
            style = "text-align:justify"),
          p(paste0("Auch auf Nordstrandischmoor können Sie Bier zu konstanten Grenzkosten von drei Euro pro Liter anbieten. Die Fixkosten des Angebots sind 5 Euro pro Tag. Sie haben 5 Tage Zeit, Erfahrungen über die Nachfrage zu sammeln und können dabei den Bierpreis jeden Tag neu festlegen."), 
            style = "text-align:justify"),
          sliderInput("price",
                      "Zu welchem Preis pro Liter bieten Sie Ihr Bier an?",
                      min = 0,
                      max = 20,
                      value = 0,
                      step = 0.5),
          actionButton("next_round", "Gewinne berechnen")
        ),
        conditionalPanel(
          condition = "output.showresults == true",
          actionButton("results", "Auswertung/Update"),
          actionButton("reset", "Reset"),
          hr(),
          passwordInput("reset_pw", "Passwort für reset:")
        )
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput('txt'),
        tableOutput('tbl'),
        conditionalPanel(
          condition = "output.showresults == true",
          plotOutput('results_plot'),
          tableOutput('results_tbl')
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Set reactive values
  values <- reactiveValues(profits = data_frame(Preis = numeric(), 
                                                Menge = numeric(), 
                                                Kosten = numeric(), 
                                                Gewinn = numeric()),
                           txt = character(),
                           n_round = 1)
  
  # Calculate new profits, set the demand function and marginal costs here
  new_profits <- function(price) {
    data_frame(Preis = price, Menge = max(0,15 - price)) %>% 
      mutate(Kosten = Menge * gk + 5,
             Gewinn = Preis * Menge - Kosten)
  }
  
  # On button push
  observeEvent(input$next_round, {
    if (values$n_round < (max_rounds + 1)) {
      # calculate new profits
      values$profits <- rbind(values$profits, new_profits(input$price))
      # add choosen price to global dataframe
      global_df <<- rbind(global_df, data_frame(Runde = values$n_round, Preis = input$price))
      # produce some text to output
      values$txt <- 
        case_when(input$price < 3 ~ paste("Sie haben einen Preis unter den variablen Kosten pro Liter gesetzt. Zu diesem niedrigen Preis können Sie zwar sehr viel Bier verkaufen, machen aber mit jedem verkauften Liter Verluste."),
                  input$price >= 15 ~ paste("Sie haben einen sehr hohen Preis gewählt. Zu diesem Preis finden sich auf der Insel keine Kunden. Sie können heute kein Bier verkaufen und Ihre Fixkosten nicht decken."),
                  values$profits$Gewinn[values$n_round] < 0 ~ paste("Beim Preis von", input$price, "Euro werden in Ihrer Kneipe", round(values$profits$Menge[values$n_round],2), "Liter getrunken.  Ihre Umsätze betragen",  input$price, "Euro *",  round(values$profits$Menge[values$n_round],2), "=",  round(input$price * values$profits$Menge[values$n_round],2), "Euro und Ihre Kosten" , gk, "Euro *", round(values$profits$Menge[values$n_round],2), "+ 5 Euro =", round(values$profits$Kosten[values$n_round],2), "Euro. Sie können Ihre Fixkosten nicht decken."),
                  TRUE ~ paste("Beim Preis von", input$price, "Euro werden in Ihrer Kneipe", round(values$profits$Menge[values$n_round],2), "Liter getrunken.  Ihre Umsätze betragen",  input$price, "Euro *",  round(values$profits$Menge[values$n_round],2), "=",  round(input$price * values$profits$Menge[values$n_round],2), "Euro und Ihre Kosten" , gk, "Euro *", round(values$profits$Menge[values$n_round],2), "+ 5 Euro =", round(values$profits$Kosten[values$n_round],2), "Euro.")
                              )
      # go to next round
      values$n_round = values$n_round + 1
      }
  })
  
  # Display text
  output$txt <- renderText({values$txt})
    
  # Display profits table
  output$tbl <- renderTable({values$profits}) 
  
  # show "results" and reset button after max_rounds
  output$showresults <- reactive({values$n_round > max_rounds})
  outputOptions(output, "showresults", suspendWhenHidden = FALSE)
  
  # Calculate/Update results on button press
  observeEvent(input$results, {
    if (values$n_round > max_rounds) {
      output$results_plot <- renderPlot({
        ggplot(global_df %>% na.omit, 
               aes(Preis, fill = as.factor(Runde), colour = as.factor(Runde))) +
          geom_density(alpha = 0.1) + labs(colour="Runde", fill = "Runde")})
      output$results_tbl <- renderTable({global_df %>% group_by(Runde) %>% 
          summarise(Durchschnitt = mean(Preis), Minimum=min(Preis), Maximum = max(Preis))})}
    })
   
  # Reset results
  observeEvent(input$reset, {
    if( input$reset_pw == pass) {
      global_df <<- data.frame(Runde = numeric(), Preis = numeric()) } else {
        showNotification("Wrong password", duration = 5, closeButton = TRUE,
                         type = "error")
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

