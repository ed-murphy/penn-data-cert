# Homework 5
# Ed Murphy
# 2025-04-22

# Load necessary libraries
library(survey)
library(shiny)
library(stargazer)

# set working directory
setwd('C:/Users/edwar/OneDrive/Ed_Outlook_acct/OneDrive/Repositories/DATA4010/src')

# load data
pm_data <- read.csv('place_data.csv')

# server object
server <- function(input, output) {
  
  regFormula <- reactive({
    as.formula(paste("Risk", " ~ ", paste(input$iv1, collapse = '+')))
  })
  
  model <- reactive({lm(regFormula(), data = pm_data)})
  
  output$regTab <- renderText({
    out <- capture.output(
      stargazer(model(), type = 'html', dep.var.labels = 'Risk Prediction')
    )
    paste(out, collapse = '\n')
  })
  
  
}

# ui object
ui <- shinyUI(fluidPage(tabPanel(
  'Analyzing ACEs',
  headerPanel('ACEs Prediction Model'),
  sidebarLayout(
    position = 'right',
    sidebarPanel(
      h2('Build your model'),
      br(),
      checkboxGroupInput(
        'iv1',
        label = 
          c('Poverty' = 'Poverty',
            'Education' = 'Education',
            'Unemployment' = 'Unemployment',
            'Crime' = 'Crime',
            'ACEs' = 'ACEs'
            ),
        selected = 'Poverty'
      )
    ),
    
    mainPanel(br(),
              tabsetPanel(
                type = 'tabs',
                tabPanel(
                  'Regression Table',
                  h3('Table of Regression Coefficients'),
                  HTML('</br>'),
                  uiOutput('regTab'),
                  HTML('</br>'),
                  helpText('Describe the model')
                )
              )
    )
  )
)
)
)

shinyApp(ui = ui, server = server)
