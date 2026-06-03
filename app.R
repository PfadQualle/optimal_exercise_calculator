# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


# ---- Source your backend ----
source("~/Documents/Projekte/Kurzprojekte/Optimal_Exercise_Calculator/Optimal_exercise_calculator_backend.R")   # contains optimal_exercise functions


# ---- Intensity presets ----
intensity_choices <- c(
  "Walking (3 METs)"                        = 2,
  "Brisk hiking (4 METs)"                   = 3,
  "Weight lifting, Yoga (intense) (5 METs)" = 4,
  "Moderate cycling (6 METs)"               = 6,
  "Jogging (8 METs)"                        = 7,
  "Basketball game (10 METs)"               = 9
)



# ---- UI ----
ui <- fluidPage(
  titlePanel("Optimal Exercise Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Your current age (years):",
                  min = 18, max = 90, value = 30, step = 1),
      
      selectInput("intensity", "Typical exercise intensity:",
                  choices = intensity_choices, selected = 4),
      
      sliderInput("utility", "Utility: How unpleasant is exercise to you?",
                  min = -1, max = 1, value = 0.9, step = 0.1,
                  ticks = FALSE),
      
      helpText("0 = unconsciousness is equivalent;
               0.50 = quite disliked;
               1 = as fun as alternatives;
               -1 = substantial suffering and misery.",
               "Each hour/week reduces per-period utility by this fraction."),
      
      sliderInput("r", "Discount rate (per year):",
                  min = 0, max = 0.12, value = 0.02, step = 0.005),
      helpText("How much less you value future years vs. today. ",
               "0 = future and present equal; 0.03 = standard health economics; 0.06 = standard adult ; 0.12 short-term focus",
               "Discount rates ONLY reflect impatience(time preference) and tech progress expectations. Mortality risk is already being accounted for!")
    ),
    
    mainPanel(
      h3(textOutput("recommendation")))
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

  result <- reactive({
    optimal_exercise <- optimal_exercise_max(
      u_rel    = as.numeric(input$utility),
      age   = as.numeric(input$age),
      intensity = as.numeric(input$intensity),
      r    = as.numeric(input$r)
    )
    optimal_exercise
  })

  
  output$recommendation <- renderText({
    res <- result()
    sprintf("Your optimal level of exercise is %.1f hours per week at the specified intensity.",
            res)
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
