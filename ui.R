#load libraries

library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(leaflet.extras)
library(tm)
library(wordcloud2)
library(plotly)


#import data , change your path
data <- read.csv("AB_NYC_2019.csv")

#create price group
data$price_group <- ifelse(data$price <= 100, "Low", 
                           ifelse(data$price <= 200, "Mid", 
                                  ifelse(data$price > 200, "High", 
                                         "Unknown")))

# Add a column with the text you want to display for each bubble:
data$text <- paste("Name: ",data$name, "\n", "Price:", data$price, "\n", "Number of Reviews: ", data$number_of_reviews, "\n", "Neighbourhood: ", data$neighbourhood)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # Application title
  titlePanel(h1("New York City Airbnb Analysis", align = "center"), windowTitle = "New York City Airbnb Analysis"),
  
  navbarPage("",
             #Tab 1
             tabPanel("Location",
                      # tab title ----
                      titlePanel("Map chart"),
                      
                      #description of the tab
                      h5("This map show the location of each listing. Please adjust the filter at the left side accordingly to your requirement."),
                      h5("You may search the desired neighbourhood by putting the name into the search bar. You also may select the Room Type or adjust the Price Range."),
                      h5("Click on the group showing in the map or zoom in the map to see the listing in details."),
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          p(),
                          textInput("neighbour", "Neighbourhood search:",""),
                          checkboxGroupInput('roomtype', 'Room Type', unique(data$room_type), unique(data$room_type)),
                          p(),
                          #      )      
                          
                          sliderInput(inputId = "range",
                                      label = "Price:",
                                      min = 1,
                                      max = 1000,
                                      step = 10,
                                      value = c(1,1000))
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          #this will create a space for us to display our map
                          leafletOutput(outputId = "mymap")
                          
                        )
                      )
             ),
             #Tab 2
             tabPanel("Reviews",
                      # Tab title ----
                      titlePanel("Top 10 Listing With Most Reviews"),
                      
                      # Description
                      h5("This tab will be showing the top 10 listing with the most reviews. Select the room type which fit your requirement. Please hover on the lollipop to see the details of the listing."),
                      
                      # Sidebar panel for inputs ----
                      sidebarPanel(
                        
                        p(),
                        selectInput('roomtypeReview', 'Room Type', unique(data$room_type))
                        
                      ),
                      
                      # Main panel for displaying outputs ----
                      mainPanel(
                        #this will create a table of top 10 reviews
                        plotlyOutput('reviewChart')
                      )
             ),
             #Tab 3
             tabPanel("Name",
                      # Tab title
                      titlePanel("Word Frequency Based On Prices"),
                      
                      # Description
                      h5("It would be interesting to know the listing name used by each host. Please adjust the price range or number of word to show in the Word Cloud. The bigger size of the text, the more frequently used by hosts."),
                      
                      sidebarLayout(
                        sidebarPanel(
                          # Sidebar with a slider and selection inputs
                          # Although the maximum price is USD 10,000, the maximum input is limited to USD 1,000,
                          # as listings priced above USD 1,000 is only 0.4% of the data,
                          # and an upper limit that is too large will affect usability.
                          sliderInput("range2", "Select the price range:", min = 1,  max = 1000,  value = c(200,700)),
                          # Allow users to choose maximum number of words
                          sliderInput("number", "Select the maximum number of words appearing in the wordcloud:", min = 1,  max = 300,  value = 30),
                        ),
                        # Show Word Cloud
                        mainPanel(
                          # plotOutput("plot")
                          wordcloud2Output('plot', width = '100%', height = '500px')
                        )
                      )
             )              
  )
)
