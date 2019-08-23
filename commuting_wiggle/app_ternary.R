#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggtern)
library(plotly)
library(dplyr)
library(tidyr)
library(grDevices)
library(viridis)
library(DT)
library(shiny)

# Functions ####################################################################

Z <- function(w, l11, l12, l21, l22, a, s, phi, i) {
  ((l12 + l11 * w + i * (1-a) * (1-w) * (l11 * l22 - l12 * l21)) /
     (phi*(l12 + l22) + (l11 + l21) * w^(1-s)) +
     phi *
     (l21 * w + l22 - i * (1-a) * (1-w) * (l11 * l22 - l12 * l21)) /
     ((l12 + l22) + phi * (l11 + l21) * w^(1-s)))^(1/s) - w
}

calc_vars <- function(w, l11, l12, l21, l22, a, s, phi, i, k, f, h) {
  # prices
  P_1 <- ((s / (s - 1))^(1 - s) * 1/ ( f * s) *
            (phi * (l12 + l22) + (l11 + l21) * w^(1-s))
  )^(1 / (1 - s))
  
  P_2 <- ((s / (s - 1))^(1 - s) * 1/ ( f * s) *
            ((l12 + l22) + phi * (l11 + l21) * w^(1-s))
  )^(1 / (1 - s))
  
  # incomes
  I_1 <- (l12 + l11 * w + i * (1-a) * (1 - w) * (l11 * l22 - l12 * l21)) /
    (1 - (1 - a) * i)
  
  I_2 <- (l21 * w + l22 - i * (1-a) * (1 - w) * (l11 * l22 - l12 * l21)) /
    (1 - (1 - a) * i)
  
  # rents
  Q_1 <- (1 - a) * I_1 / h
  Q_2 <- (1 - a) * I_2 / h
  
  # total price indices
  Pind_1 <- P_1^a * Q_1^(1-a)
  Pind_2 <- P_2^a * Q_2^(1-a)
  
  # Utilities
  V_11 <- (w + i * (1 - a)*(I_1 + I_2)) / Pind_1
  V_22 <- (1 + i * (1 - a)*(I_1 + I_2)) / Pind_2
  V_12 <- 1 / k * (1 + i * (1 - a)*(I_1 + I_2)) / Pind_1
  V_21 <- 1 / k * (w + i * (1 - a)*(I_1 + I_2)) / Pind_2
  
  return(list(V_11 = V_11, V_12 = V_12, V_21 = V_21, V_22 = V_22))
}

find_V <- function(l11, l12, l21, l22, a, s, phi, i, k, f, h) {
  w <- uniroot(Z, interval=c(0.0001,1000), tol = 1e-14, l11, l12, l21, l22, a, s, phi, i)$root
  calc_vars(w, l11, l12, l21, l22, a, s, phi, i, k, f, h)
}

partial_agg_w <- function(l11, a, i, k) {
  (1 - (k - (k - 1) * l11) * i * (1 - a)) /
    (k - (k - (k - 1) * l11) * i * (1 - a))
}

partial_agg_eq1 <- function(l22, l11, a, s, phi, i, k) {
  w <- partial_agg_w(l11, a, i, k)
  (k * (1 - l22) - (k - 1) * l11) /
    (((1 - l11) * w^s + phi * l11 * w) *
       (k - (k - (k - 1) * l11) * i * (1 - a)) / (1 - i * (1 - a)) -
       phi * l22 * k
    ) - 
    (w^(1 - s) + phi * (1 - l11) / l11) /
    (phi * w^(1 - s) + (1 - l11) / l11)
}

partial_agg_eq2 <- function(l22, l11, a, s, phi, i, k) {
  w <- partial_agg_w(l11, a, i, k)
  k^(s - 1) * (l11 / l22 + k * (1 - l11 - l22) / l22)^((1 - a) * (s - 1) / a) - 
    (w^(1 - s) + phi * (1 - l11) / l11) /
    (phi * w^(1 - s) + (1 - l11) / l11)
}


partial_agg_eq <- function(l11, a, s, phi, i, k) {
  uniroot(partial_agg_eq1, interval=c(0.0001,1-l11), tol = 1e-14, l11, a, s, phi, i, k)$root -
    uniroot(partial_agg_eq2, interval=c(0.0001,1-l11), tol = 1e-14, l11, a, s, phi, i, k)$root
}

limit_range <- function(x, n) {
  x[x > sort(x, decreasing = TRUE)[n]] <- NA
  x
}

scale_to_1 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Commuting and Stability"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(width=3,
           numericInput("s", "sigma", 4, min = 1, max = NA, step = 0.5),
           sliderInput("a",
                       "Goods share in consumption",
                       min = 0,
                       max = 1,
                       value = 0.8),
           sliderInput("i",
                       "Rent share to portfolio",
                       min = 0,
                       max = 1,
                       value = 0.8),
           sliderInput("phi",
                       "Trade freeness phi",
                       min = 0,
                       max = 1,
                       value = 0.8),
           numericInput("k", "commuting costs", 1.1, min = 1, max = NA, step = 0.1),
           numericInput("h", "housing stock", 1, min = 0, max = NA, step = 0.5),
           numericInput("f", "fixed costs", 1, min = 0, max = NA, step = 0.5),
           textInput("filterstring", "Filter command for table",
                     value = "filter(abs(del_V_11) < 0.0001 | lambda_11 == 0, abs(del_V_12) < 0.0001 | lambda_12 == 0, abs(del_V_21) < 0.0001 | lambda_21 == 0, abs(del_V_22) < 0.0001 | lambda_22 == 0)"),
           actionButton("show_tbl", "Filter Table"),
           textInput("drawstring", "Draw command for color",
                     value = "del_V_11"),
           actionButton("redraw", "Redraw ternary diagramm")
    ),
    column(width=6,
           textOutput("mytext"),
           tableOutput("mytable"),
           plotOutput("motion")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  motion_df <- data_frame(lambda_11 = seq(0, 1, 0.01), 
                          lambda_12 = seq(0, 1, 0.01)) %>% 
    complete(lambda_11, lambda_12) %>% 
    mutate(lambda_21 = 0,
           lambda_22 = 1 - lambda_11 - lambda_12 - lambda_21,
           # there seems to be some problem with how exact numbers are here?!
           lambda_11 = round(lambda_11,2),
           lambda_12 = round(lambda_12,2),
           lambda_21 = round(lambda_21,2),
           lambda_22 = round(lambda_22,2)) %>% 
    filter(lambda_22 >= 0)
  
  
  mydata <- reactiveVal()
  observe({
    mydata(apply(motion_df, 1,
                 function(x) unlist(find_V(x[1], x[2], x[3], x[4], input$a, input$s, input$phi,
                                           input$i, input$k, input$f, input$h))) %>% t %>% 
             cbind(motion_df) %>%
             rename(V_11 = V_11.lambda_12, V_12 = V_12.lambda_12, V_21 = V_21.lambda_12,
                    V_22 = V_22.lambda_12) %>%
             rowwise() %>% 
             mutate(bar_V = sum(V_11 * lambda_11, V_12 * lambda_12, V_21 * lambda_21, 
                                V_22 * lambda_22, na.rm = TRUE)) %>% ungroup %>% 
             mutate(del_V_11 =  (V_11 - bar_V),
                    del_V_12 =  (V_12 - bar_V),
                    del_V_21 =  (V_21 - bar_V),
                    del_V_22 =  (V_22 - bar_V)))
  }) 
  
  motion_data <- eventReactive(input$redraw, {
    mydata() %>% 
      eval(parse(text = paste0("tmp %>% mutate(z = ", 
                               input$drawstring, ")"))) %>%
      mutate(z = limit_range(z, 5))
    })
  
  motion_data <- motion_data %>% 
    mutate(col = rgb(ifelse(is.na(scale_to_1(del_V_11)), 0, scale_to_1(del_V_11)),
                     ifelse(is.na(scale_to_1(del_V_12)), 0, scale_to_1(del_V_12)),
                     ifelse(is.na(scale_to_1(del_V_22)), 0, scale_to_1(del_V_22)),
                     maxColorValue = 1))
  
  output$motion <- renderPlot({  
    p <- motion_data() %>% 
      ggtern(aes(x = lambda_11, y = lambda_12, z = lambda_22)) +
      geom_point(color = ~z)
    
    
    print(p) # possible work around for ggtern issues

  })
  
  filtered_data <- eventReactive(input$show_tbl, {
    mydata()
    eval(parse(text = paste0("mydata() %>% ", input$filterstring))) 
  })  
  output$mytable <- renderTable({
    filtered_data()    
  }, digits = 5) 
  
  output$mytext <- renderText({
    agg_eq_l22 <- 1 / (1 + input$k^(-1 / (1 - input$a)) * 
                         input$phi^(input$a / ((input$s-1)*(1-input$a))))
    agg_eq_l12 <- 1 - agg_eq_l22
    agg_eq_k <- (1/ input$phi + agg_eq_l22 * (input$phi^2 - 1) / input$phi
    )^(-1 / input$s)
    agg_Vs <- find_V(0,1-agg_eq_l22,0,agg_eq_l22,input$a,input$s,input$phi,input$i,input$k,input$f,input$h)
    paste0("With lambda_22 at ", round(agg_eq_l22,5), 
           " we must have kappa smaller or equal to ", round(agg_eq_k,5),
           " for an equilibrium with commuting to exist and be stable. ",
           " The resulting utility levels would be ", round(agg_Vs[[1]],5), " ",
           round(agg_Vs[[2]],5), " ", round(agg_Vs[[3]],5), " ",
           round(agg_Vs[[4]],5))
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
