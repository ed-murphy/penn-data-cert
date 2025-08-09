# Final Project
# Ed Murphy
# 2025-05-04

# Load necessary libraries
library(survey)
library(sf)
library(shiny)
library(stargazer)
library(sandwich)
library(lmtest)
library(rsconnect)
library(dplyr)
library(leaflet)

# load data for model
schools <- read.csv('Philly_schools.csv')

# add variables for model
schools <- schools %>%
  mutate(
    student_turnover_count = New_student + Withdrawals,
    special_education_count = Special_education / 100 * Enrollment,
    SCHOOL_ZIP = as.character(SCHOOL_ZIP)
  )

# load data for map
# Load shapefile
zipcodes <- read_sf('Zipcodes_Poly', layer = 'Zipcodes_Poly')
zipcodes <- st_set_crs(zipcodes, 4326)
zipcodes84 <- st_transform(zipcodes, 4326)

zipcodes84 <- left_join(zipcodes84, schools %>%
                          group_by(SCHOOL_ZIP) %>%
                          summarise(
                            total_suspensions = sum(Total_suspensions, na.rm = TRUE),
                            total_enrollment = sum(Enrollment, na.rm = TRUE),
                            total_turnover = sum(student_turnover_count, na.rm = TRUE),
                            total_special = sum(special_education_count, na.rm= TRUE)
                          ),
                        by = c("CODE" = "SCHOOL_ZIP"))

zipcode_popup <- paste0('<strong>Zip Code: </strong>', zipcodes84$CODE, '<br>',
                        '<strong>Total Suspensions: </strong>', round(zipcodes84$total_suspensions, 0), '<br>',
                        '<strong>Total Enrollment: </strong>', round(zipcodes84$total_enrollment, 0), '<br>',
                        '<strong>Student Turnover: </strong>', round(zipcodes84$total_turnover, 0), '<br>',
                        '<strong>Special Education Students: </strong>', round(zipcodes84$total_special, 0))

zipcodes84$SuspBin <- cut(zipcodes84$total_suspensions,
                          breaks = c(0, 50, 100, 200, Inf),
                          labels = c('0-50', '51-100', '101-200', '201+'),
                          include.lowest = TRUE)
color_scheme <- colorFactor(c('#F6E27F', '#E69A5C', '#D35C5C', '#B22222'),
                            zipcodes84$SuspBin)



# server object
server <- function(input, output) {
  
  regFormula <- reactive({
    as.formula(paste("Total_suspensions", " ~ ", paste(input$iv1, collapse = '+')))
  })
  
  model <- reactive({
    
    model_fit <- lm(regFormula(), data = schools)
    
    robust_se <- vcovHC(model_fit, type = 'HC1')
    
    list(model = model_fit, robust_se = robust_se)
    
    })
  
  output$regTab <- renderText({
    mod <- model()$model
    robust_se <- model()$robust_se
    stargazer(mod,
              type = 'html',
              dep.var.labels = 'Suspensions Prediction',
              se = list(sqrt(diag(robust_se))),
              single.row = TRUE)
  })
  
  output$plotInfo <- renderUI({
    if (is.null(input$iv1) || length(input$iv1) == 0) {
      return(helpText("Please select at least one predictor to view the plot."))
    }
    helpText("Plotting One Suspension against each selected predictor.")
  })
  
  output$dynamicPlotUI <- renderUI({
    req(input$iv1)
    num_vars <- length(input$iv1)
    height_px <- 300 * max(num_vars, 1)  # Each plot gets 300px
    
    div(
      style = 'overflow-y: auto; max-height: 1000px;',
      plotOutput('dynamicPlot', height = paste0(height_px, "px"))
    )
  })
  
  output$dynamicPlot <- renderPlot({
    req(input$iv1)  # Only run if at least one predictor is selected
    
    num_vars <- length(input$iv1)
    par(mfrow = c(num_vars, 1), mar = c(4, 4, 2, 1))  # Adjust margins for readability
    
    for (var in input$iv1) {
      x <- schools[[var]]
      y <- schools$One_suspension
      x_label <- var
      
      plot(x, y,
           main = paste("Suspensions vs.", var),
           xlab = x_label,
           ylab = "One Suspension",
           pch = 19,
           col = "darkgreen")
      abline(lm(y ~ x), col = "blue", lwd = 2)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(zipcodes84) %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(stroke = TRUE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  color = 'gray',
                  weight = 1,
                  fillColor = ~color_scheme(SuspBin),
                  popup = zipcode_popup) %>%
      addLegend('bottomright',
                colors = c('#F6E27F', '#E69A5C', '#D35C5C', '#B22222'),
                labels = levels(zipcodes84$SuspBin),
                title = 'Total Suspensions',
                opacity = 1)
  })
  
}

# ui object
ui <- shinyUI(fluidPage(tabPanel(
  'Analyzing Suspensions',
  headerPanel('Student Suspensions in Philadelphia Schools'),
  sidebarLayout(
    position = 'right',
    sidebarPanel(
      h2('Build your model'),
      br(),
      checkboxGroupInput(
        'iv1',
        label = 'You can remove either predictor to see the other predictor in isolation. You can change your selection at any time.',
          c('Student turnover' = 'student_turnover_count',
            'Number of special education students' = 'special_education_count'
            ),
        selected = c('student_turnover_count', 'special_education_count')
      )
    ),
    
    mainPanel(br(),
              tabsetPanel(
                type = 'tabs',
                tabPanel(
                  'Regression Table',
                  h3('Table of Regression Coefficients'),
                  HTML('</br>'),
                  tableOutput('regTab'),
                  HTML('</br>'),
                  helpText('This linear model predicts the number of suspensions at a school per year using the number of low-income students, student turnover (new students + withdrawals), and number of special education students. Note that due to the presence of heteroskedasticity, robust standard errors are used to generate this table.')
                ),
                tabPanel(
                  'Plots',
                  h3('Visualizing Predictors vs. Suspensions'),
                  uiOutput('plotInfo'),
                  uiOutput('dynamicPlotUI')
                ),
                tabPanel(
                  'Map',
                  h3('Enrollment, Suspensions, and Demographics by Zip Code'),
                  leafletOutput('map', height = 600)
                )
    )
  )
)
)
)
)

shinyApp(ui = ui, server = server)
