library(shiny)
library(dplyr)
library(nleqslv)
library(ggplot2)

# Setting up the equilibrium equations
price_index_ni <- function(lambda_n, w_n, w_i, sigma, d_ni, A_n, A_i, f_n, f_i, L_bar) {
  ((sigma - 1)^(sigma - 1) / sigma^sigma * L_bar *
     (A_n^sigma / f_n * lambda_n * w_n^(1 - sigma) + 
      A_i^sigma / f_i * (1 - lambda_n) * (d_ni * w_i)^(1 - sigma))
  )^(1 / (1 - sigma))
}

wage_n <- function(P_n, P_i, sigma, alpha_n, alpha_i, d_in, A_n, f_n, Y_n, Y_i, 
                   landlords) {
  if(landlords == TRUE) {
    Y_goods_n <- Y_n
    Y_goods_i <- Y_i
  } else {
    Y_goods_n <- alpha_n * Y_n
    Y_goods_i <- alpha_i * Y_i
  }
  
  A_n * ((sigma - 1)^(sigma - 1) / sigma^sigma * 1 / f_n *
           (Y_goods_n / P_n^(1 - sigma) + 
            Y_goods_i * d_in^(1 - sigma) / P_i^(1 - sigma))
  )^(1 / sigma)
}

expenditure_n <- function(L_bar, lambda_n, w_n, w_i, alpha_n, alpha_i,
                          landlords) {
  if(landlords == TRUE) {
      Y_n <- L_bar * lambda_n * w_n
    } else {
      Y_n <- (alpha_i * L_bar * lambda_n * w_n + 
              lambda_n * (1 - alpha_i) * L_bar * 
              (lambda_n * w_n + (1 - lambda_n) * w_i)
             ) / (alpha_n * lambda_n + alpha_i * (1 - lambda_n))
    }
  return(Y_n)
}

# Solve the short term equilibrium system
solve_st_eq <- function(lambda_D, params) {
  # numeraire
  w_F <- 1
  w_D_diff <- 1
  
  # initial guess
  w_D <- 1
  
  first_run = TRUE
  
  # solve system
  while(w_D_diff > 1e-5) {
    Y_D <-  with(params, expenditure_n(L_bar, lambda_D, w_D, w_F, alpha_D, 
                                       alpha_F, landlords))
    Y_F <-  with(params, expenditure_n(L_bar, 1 - lambda_D, w_F, w_D, alpha_F,
                                       alpha_D, landlords))
    P_D <- with(params, price_index_ni(lambda_D, w_D, w_F, sigma, d_DF, A_D, A_F, 
                                       f_D, f_F, L_bar))
    P_F <- with(params, price_index_ni(1 - lambda_D, w_F, w_D, sigma, d_FD, A_F, 
                                       A_D, f_F, f_D, L_bar))
    w_D_new <-  with(params, wage_n(P_D, P_F, sigma, alpha_D, alpha_F, d_FD, A_D, 
                                    f_D, Y_D, Y_F, landlords))
  
    if(first_run == TRUE) {
      w_D_diff <- 1
      first_run <- FALSE
      w_D <- w_D_new
    } else {
      w_D_diff <- abs(w_D_new / w_D - 1) 
      w_D <- w_D_new
    }
   
  }
  return(c(w_D, w_F, P_D, P_F, Y_D, Y_F))
}

# calcualte real wages / indirect utilities
omega_n <- function(w_n, P_n, Y_n, lambda_n, L_bar, alpha_n, H_n, landlords) {
  if(alpha_n == 1) {
    # avoid 0^0
    q_n <- 1
  } else {
    q_n = (1 - alpha_n) * Y_n / H_n
  }
  
  Y_n / (lambda_n * L_bar) / (P_n^alpha_n * q_n^(1 - alpha_n))
  
}

# calculate real wage differential
omega_diff <- function(x, lambda_D, params) {
  w_D <- x[1]
  w_F <- x[2]
  P_D <- x[3]
  P_F <- x[4]
  Y_D <- x[5]
  Y_F <- x[6]
  
  omega_D <- with(params, omega_n(w_D, P_D, Y_D, lambda_D, L_bar, alpha_D, H_D,
                                  landlords))
  omega_F <- with(params, omega_n(w_F, P_F, Y_F, 1 - lambda_D, L_bar, alpha_F,
                                  H_F, landlords))
  
  if(params$relative == TRUE) {
    omega_D / omega_F 
  } else {
    omega_D - omega_F
  }
}

calc_omega_diff <- function(lambda_D, params) {
  sapply(lambda_D, function(i) {omega_diff(solve_st_eq(i, params), i, params)})
}

wiggle_data <- function(params) {
  data_frame(lambda_D = c(seq(0.001, 0.049, 0.002),# provide some extra smoothness on the borders
                          seq(0.05, 0.195, 0.01),
                          seq(0.2, 0.8, 0.01), 
                          seq(0.805, 0.95, 0.01),
                          seq(0.951, 0.999, 0.002)), 
    omega_diff = calc_omega_diff(lambda_D, params))
}

calc_longrun_omega_diff <- function(lambda_D, amenities) {
  with(amenities, (lambda_D / (1 - lambda_D) * B_D / B_F)^(1 / epsilon))
}

# Define User Interface ########################################################
ui <- fluidPage(
  title = "Wiggle Diagram",
  plotOutput('plot'),
  hr(),
  fluidRow(
    column(3,
           h4("Settings"),
           checkboxInput('symmetric', 'Symmetric parameters', value = TRUE),
           numericInput('sigma', 'Elasticity of substitution (sigma)', value = 4, min = 1),
           numericInput('epsilon', 'Amenity elasticity (epsilon)', value = 4, min = 1),
           numericInput('L_bar', 'Total population', value = 1, min = 0),
           checkboxInput('lr_lambda', 
                         'Draw long run lambda (for imperfect mobility)',
                         value = FALSE),
           checkboxInput('landlords', 
                         'Land owned by immobile landlords (who only consume goods)',
                         value = FALSE),
           checkboxInput('relative', 
                         'Use relative utility (instead of differential)', 
                         value = FALSE),
           checkboxInput('axis_fix', 
                         'Fix the y-axis minimum and maximum', 
                         value = FALSE),
           column(5, offset = 1,
             numericInput('y_min', 'Minimum', value = -1)),
           column(5, 
             numericInput('y_max', 'Maximum', value = 1)
           )
    ),
    column(4, offset = 1,
           h5("Domestic"),
           numericInput('d_DF', 'Import Barriers (d)', value = 1.5, min = 1, step = 0.1),
           numericInput('alpha_D', 'Goods share in consumption (alpha)', value = 1, min = 0, max = 1, step = 0.05),
           numericInput('B_D', 'Average amenity level (B)', value = 1, min = 0),
           numericInput('A_D', 'Technology (A)', value = 1, min = 0),
           numericInput('f_D', 'Fixed costs in production (F)', value = 1, min = 0),
           numericInput('H_D', 'Housing Stock (H)', value = 1, min = 0)
    ),
    column(4,
           conditionalPanel(
             condition = "input.symmetric == false",
             h5("Foreign"),
             numericInput('d_FD', 'Import Barriers (d)', value = 1.5, min = 1, step = 0.1),
             numericInput('alpha_F', 'Goods share in consumption (alpha)', value = 1, min = 0, max = 1, step = 0.05),
             numericInput('B_F', 'Average amenity level (B)', value = 1, min = 0),
             numericInput('A_F', 'Technology (A)', value = 1, min = 0),
             numericInput('f_F', 'Fixed costs in production (F)', value = 1, min = 0),
             numericInput('H_F', 'Housing Stock (H)', value = 1, min = 0)
           )
    )
  )
)

# Define server logic ##########################################################
server <- function(input, output) {
   
   params <- reactive({
     if(input$symmetric == TRUE) {
       list(sigma = input$sigma,
            d_DF = input$d_DF,
            d_FD = input$d_DF,
            A_D = input$A_D, 
            A_F = input$A_D,
            f_D = input$f_D,
            f_F = input$f_D, 
            alpha_D = input$alpha_D,
            alpha_F = input$alpha_D,
            L_bar = input$L_bar,
            landlords = input$landlords,
            H_D = input$H_D,
            H_F = input$H_F,
            relative = input$relative)  
     } else {
       list(sigma = input$sigma,
            d_DF = input$d_DF,
            d_FD = input$d_FD,
            A_D = input$A_D, 
            A_F = input$A_F,
            f_D = input$f_D,
            f_F = input$f_F, 
            alpha_D = input$alpha_D,
            alpha_F = input$alpha_F,
            L_bar = input$L_bar,
            landlords = input$landlords,
            H_D = input$H_D,
            H_F = input$H_F,
            relative = input$relative)
     }
   })
   
   amenities <- reactive({
     if(input$symmetric == TRUE) {
       list(B_D = input$B_D,
            B_F = input$B_D,
            epsilon = input$epsilon)  
     } else {
       list(B_D = input$B_D,
            B_F = input$B_F,
            epsilon = input$epsilon)
     }
   })
   
   wiggle_data_reac <- reactive({
     # Check parameters
     validate(
         need(params()$alpha_D > 0 && params()$alpha_D <= 1, "Alpha value out of range ]0,1]."),
         need(params()$alpha_D == params()$alpha_F || (params()$alpha_F > 0 && params()$alpha_F <= 1), 
              "Foreign alpha value out of range ]0,1]."),
         need(params()$d_DF >= 1, "Barriers out of range (d >= 1)."),
         need(params()$d_DF == params()$d_FD || params()$d_FD >= 1, 
              "Foreign import barriers out of range (d_FD >= 1)."),
         need(params()$sigma > 1, "Sigma out of range (sigma > 1)."),
         need(params()$A_D > 0, "Technology out of range (A > 0)."),
         need(params()$A_D == params()$A_F || params()$A_F > 0, 
              "Foreign technology out of range (A_F > 0)."),
         need(params()$f_D > 0, "Fixed costs out of range (F > 0)."),
         need(params()$f_D == params()$f_F || params()$f_F > 0, 
              "Foreign fixed costs out of range (f_F > 0)."),
         need(params()$H_D > 0, "Housing stock out of range (H > 0)."),
         need(params()$H_D == params()$H_F || params()$H_F > 0, 
              "Foreign housing stock out of range (H_F > 0)."),
         need(params()$L_bar > 0, "Total population out of range (L > 0).")
         )
     wiggle_data(params())
   }) 
   
   output$plot <- renderPlot({
    plot_data <- wiggle_data_reac()  %>% 
      # fix imprecision
      mutate(omega_diff = ifelse(abs(omega_diff) < 1e-15, 0, omega_diff))
  
    # find all inner equilibria
    if(any(diff(sign(plot_data$omega_diff - params()$relative)) != 0)) {
      lambda_D_roots <- mapply(uniroot, 
                               lower = plot_data$lambda_D[which(diff(sign(plot_data$omega_diff - params()$relative)) != 0)],
                               upper = plot_data$lambda_D[which(diff(sign(plot_data$omega_diff - params()$relative)) != 0) + 1],
                               MoreArgs = list(f = function(...) {calc_omega_diff(...) - params()$relative}, extendInt = c("yes"), params=params()))[1,] %>% 
        unlist %>% unique()
      # numerically determine stability
      roots <- data_frame(lambda_D = lambda_D_roots,
                          omega_diff = calc_omega_diff(lambda_D_roots, params()))
      roots$stability <- ifelse(calc_omega_diff(lambda_D_roots + 0.0001, params()) <
                                   calc_omega_diff(lambda_D_roots - 0.0001, params()),
                                 "stable", "unstable") %>% 
        factor(levels=c("stable","unstable"))
    } else {
      roots <- data_frame(lambda_D = numeric(0), omega_diff = numeric(0),
                          stability = factor(levels=c("stable", "unstable")))
    }
    # add border equilibria if they exist
    if(plot_data[1,'omega_diff'] - params()$relative < 0) {
      roots <- bind_rows(roots, 
                         plot_data[1,] %>% 
                           mutate(stability= factor("stable", 
                                                    levels=c("stable", "unstable"))))
    }
    if(plot_data[nrow(plot_data),'omega_diff'] - params()$relative > 0) {
      roots <- bind_rows(roots, 
                         plot_data[nrow(plot_data),] %>% 
                           mutate(stability= factor("stable", levels=c("stable", "unstable"))))
    }
    
    # Create the wiggle diagramm
    wiggle_plot <- ggplot(plot_data, aes(x=lambda_D, y = omega_diff)) + geom_line() +
      # mark stability of equilibria
      geom_point(data=roots, aes(color=stability), size = 4) + 
      scale_colour_discrete(drop = FALSE)
     
    # Set axis labels
    if (params()$relative == TRUE) {
      wiggle_plot <- wiggle_plot + 
        labs(x = "Domestic share of labor", y = "Relative utility (Domestic/Foreign)")
    } else {
      wiggle_plot <- wiggle_plot + 
        labs(x = "Domestic share of labor", y = "Utility differential (Domestic - Foreign)")
    }
    
    # Fix axis?
    if (input$axis_fix == TRUE) {
      wiggle_plot <- wiggle_plot + ylim(input$y_min, input$y_max)
    } 
    
    # Add long run lambda line
    if (input$lr_lambda == TRUE) {
      plot_data$lr_lambda <- calc_longrun_omega_diff(plot_data$lambda_D, amenities())
      wiggle_plot <- wiggle_plot + geom_line(aes(y=plot_data$lr_lambda), color = "red")
    }
    
    # Draw plot
    wiggle_plot
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

