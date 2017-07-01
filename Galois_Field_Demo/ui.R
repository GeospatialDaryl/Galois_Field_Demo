library(shiny)
shinyUI(fluidPage(
  titlePanel("Predict Horsepower from MPG"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sliderMPG", "What is the MPG of the car?", 10, 35, value = 20),
      sliderInput("sliderAlpha", "model alpha ", 1, 5, value = 2),
      sliderInput("sliderFW", "Field Width ", 1, 20, value = 8),
      checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE),
      checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE),
      submitButton("Submit")
    ),
    mainPanel(
      #fluidRow(
      #  column(4,
      #         dataTableOutput("tableValpha")),
      #  column(4,
      #         dataTableOutput("tableVcoeff"))
      #),
      #column(4,
      #       dataTableOutput("tableValpha")),
      #column(4,
      #      dataTableOutput("tableVcoeff")),
      tableOutput("tableValpha"),
      tableOutput("tableVcoeff"),
      
      plotOutput("plot1"),
      h3("Predicted Horsepower from Model 1:"),
      textOutput("pred1"),
      h3("Predicted Horsepower from Model 2:"),
      textOutput("pred2")
    )
  )
))