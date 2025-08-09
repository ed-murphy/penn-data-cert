# Homework 3
# Ed Murphy
# 2025-04-08

# Load necessary libraries
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.providers)

# set working directory
setwd('C:/Users/edwar/OneDrive/Ed_Outlook_acct/OneDrive/Repositories/DATA4010/src')

# load in the data
image_data <- read.csv('place_data.csv')
head(image_data)

# load in the shapefile
zipcodes <- read_sf('Zipcodes_Poly', layer = 'Zipcodes_Poly')
head(zipcodes)

# this is the transform command that Professor Kates gave
# us in lecture - but it fails due to "missing crs"
zipcodes84 <- st_transform(zipcodes, 4326)

# so I found a solution online - which is to run this command first instead
zipcodes <- st_set_crs(zipcodes, 4326)

# and then the command that Professor Kates gave to us now works
zipcodes84 <- st_transform(zipcodes, 4326)

# now merge the two data sources using the method shown in class
zipcodes <- data.frame(zipcodes, image_data[match(zipcodes$CODE, image_data$CODE),])

# create zip code popup object
zipcode_popup <- paste0('<strong>Zip Code: </strong>', zipcodes$CODE, '<br>',
                        '<br>',
                        '<strong>Risk Index: </strong>', zipcodes$Risk, '%', '<br>',
                        '<strong>Poverty: </strong>', zipcodes$Poverty, '%', '<br>',
                        '<strong>Education: </strong>', zipcodes$Education, '%', '<br>',
                        '<strong>Unemployment: </strong>', zipcodes$Unemployment, '%', '<br>',
                        '<strong>Crime: </strong>', zipcodes$Crime, '%', '<br>',
                        '<strong>ACEs: </strong>', zipcodes$ACEs, '%', '<br>'
                        )

# create factor version of Risk variable
zipcodes$RiskFactor <- cut(
  zipcodes$Risk,
  breaks = c(0, 24, 49, 74, 98),
  labels = c('0-24', '25-49', '50-74', '75-98'),
  include.lowest = TRUE,
  right = TRUE
)

# use factor version of Risk to create color scheme object
color_scheme <- colorFactor(c('#F6E27F', '#E69A5C', '#D35C5C', '#B22222'),
                            zipcodes$RiskFactor)

# recreate the provided image
leaflet(zipcodes84) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(stroke = TRUE,
              smoothFactor = 0.2,
              fillOpacity = 0.8,
              color = 'lightgray',
              weight = 1,
              fillColor = ~color_scheme(zipcodes$RiskFactor),
              popup = zipcode_popup) %>%
  addLegend('bottomright',
            colors = c('#F6E27F', '#E69A5C', '#D35C5C', '#B22222'),
            labels = c('0 - 24', '25 - 49', '50 - 74', '75 - 98'),
            title = 'Risk: Lowest to Highest',
            opacity = 1)
