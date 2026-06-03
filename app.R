## ---------------------------
##
## Script name: app.R
##
## Purpose of script: This script contains the front end of the optimal exercise calculator 
##
## Author: Niklas V. Lehmann
##
## Date Created: 2026-06-02
##
## Copyright (c) Niklas V. Lehmann, 2026
## Email: niklasl.2306@gmail.com
##
## ---------------------------
##
## Notes: 
##   
##
## ---------------------------

library(shiny)

rsconnect::writeManifest()

source("Optimal_exercise_calculator_backend.R")

intensity_choices <- c(
  "Walking (3 METs)"                        = 2,
  "Brisk hiking (4 METs)"                   = 3,
  "Weight lifting, Yoga (intense) (5 METs)" = 4,
  "Moderate cycling (6 METs)"               = 5,
  "Jogging (8 METs)"                        = 7,
  "Basketball game (10 METs)"               = 9
)

ui <- fluidPage(
  tags$style(HTML("
    .form-group { margin-bottom: 20px; }
    .help-block { color: #666; font-size: 13px; line-height: 1.5; }
    .well-result { background-color: #f0f7ff; border: 1px solid #b8d4f0;
                   padding: 20px; border-radius: 6px; margin-top: 20px; }
  ")),
  
  titlePanel("Optimal Exercise Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Your current age (years):",
                  min = 18, max = 90, value = 30, step = 1),
      
      selectInput("intensity", "Typical exercise intensity:",
                  choices = intensity_choices, selected = 4),
      
      sliderInput("utility", "Utility: How unpleasant is exercise to you?",
                  min = -1, max = 1, value = 0.9, step = 0.1, ticks = FALSE),
      helpText(
        "1 = as fun as alternatives",   br(),
        "0.5 = quite disliked",         br(),
        "0 = equivalent to unconsciousness", br(),
        "-1 = substantial suffering and misery.", br(), br(),
        "Each hour/week reduces per-period utility by this fraction."
      ),
      
      sliderInput("r", "Discount rate (per year):",
                  min = 0, max = 0.12, value = 0.02, step = 0.005),
      helpText(
        "How much less you value future years vs. today.", br(),
        "0 = future and present equal", br(),
        "0.03 = standard health economics", br(),
        "0.06 = standard adult", br(),
        "0.12 = short-term focus", br(), br(),
        tags$em("Note: discount rates reflect impatience and tech progress expectations only.
                 Mortality risk is already accounted for.")
      )
    ),
    
    mainPanel(
      div(class = "well-result",
          h3(textOutput("recommendation"))
      )
    )
  )
)

server <- function(input, output) {
  
  result <- reactive({
    optimal_exercise_max(
      u_rel     = as.numeric(input$utility),
      age       = as.numeric(input$age),
      intensity = as.numeric(input$intensity),
      r         = as.numeric(input$r)
    )
  })
  
  output$recommendation <- renderText({
    sprintf("Your optimal level of exercise is %.1f hours per week at the specified intensity.",
            result())
  })
}

shinyApp(ui = ui, server = server)