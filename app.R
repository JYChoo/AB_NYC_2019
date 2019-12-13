#checking required packages
list.of.packages <- c("dplyr", "shiny", "leaflet", "leaflet.extras", "stringr", "tm", "wordcloud2","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


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

# Define server logic required to draw the charts ----
server <- function(input, output) {
  
  ################### MAP START HERE ###################
  # Reactive expression for the data subsetted to what the user selected in Location Tab
  filteredData <- reactive({
    data %>% 
      filter(price >= input$range[1] & price <= input$range[2])  %>%
      filter(room_type %in% input$roomtype) %>%
      filter(
        if(input$neighbour==""){
          TRUE
        } else {
          str_detect(neighbourhood,fixed(input$neighbour, ignore_case=TRUE))
        }
      )
  })
  
  #define the color of for the marker
  pal2 <- colorFactor(
    palette = c('red', 'green','orange'),
    domain = data$price_group
  )
  
  output$mymap <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = -74, lat = 40.7, zoom = 9)  %>% #setting the view over ~ center of North America      
      addCircleMarkers(
        lng = ~longitude, 
        lat = ~latitude,
        #add pop up content
        popup=~paste(
          "<b>", name, "</b><br/>",
          "Host Name: ", host_name, "<br/>",
          "Price: $", as.character(price)
        ),        
        clusterOptions = markerClusterOptions()
      ) 
    # %>%
    #  addLegend("bottomright", pal = pal2, values = data$price_group, title = "Price Group", opacity = 1)
    })
  
  observeEvent({
    input$range
    input$roomtype
    input$neighbour},{
    leafletProxy("mymap", data = filteredData()) %>% 
      clearMarkerClusters() %>% 
    #  clearMarkers() %>%
    #  clearShapes() %>%
      addCircleMarkers(
        lng = ~longitude, 
        lat = ~latitude,
        #add pop up content
        popup=~paste(
          "<b>", name, "</b><br/>",
          "Host Name: ", host_name, "<br/>",
          "Price: $", as.character(price)
        ),        
        clusterOptions = markerClusterOptions()
      )
  })
  
  ################### TOP 10 REVIEWS START HERE ###################
  # Reactive expression for the data subsetted to what the user selected in Review Tab
  observed=reactiveValues(
    inputReview=NULL
  )
  
  observeEvent(input$roomtypeReview,{    
    observed$inputReview <- data %>% 
      filter(
        if(input$roomtypeReview=="All"){
          TRUE
        } else {
          str_detect(room_type,fixed(input$roomtypeReview, ignore_case=TRUE))
        }
      )%>%
      arrange(desc(number_of_reviews)) %>%
      slice(1:10)
  })
  
  #Lollipop Chart
  output$reviewChart <- renderPlotly({
      lollipopC <-observed$inputReview %>%
      arrange(number_of_reviews) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
      mutate(name=factor(name, levels=name)) %>%   # This trick update the factor levels
      ggplot(aes(x=name, y=number_of_reviews, text=text)) +
      geom_segment( aes(xend=name, yend=0)) +
      geom_point( size=6, aes(color=name)) +
      coord_flip() +
      theme_bw() + 
      theme(legend.position = "none")
  
    ggplotly(lollipopC, tooltip="text") %>%
      config(displayModeBar = FALSE)
  })
  
  ################### WORD CLOUD START HERE ###################
      terms <- reactive({
      # Extract "price" column based on the upper and lower limits of the slider input
      text <- filter(data, price >= input$range2[1] & price <= input$range2[2])$name
      
      # Do stemming for certain words with significant frequency using regex
      text <- gsub("\\b(?i)apt\\b", 'apartment', text)
      text <- gsub("\\b(?i)lux\\b", 'luxury', text)
      text <- gsub("\\b(?i)luxurious\\b", 'luxury', text)
      
      # Remove all graphical characters
      text <- str_replace_all(text,"[^[:graph:]]", " ")
      
      # Remove certain words & symbols and proceed with the wordcloud
      myCorpus = Corpus(VectorSource(text))
      myCorpus = tm_map(myCorpus, content_transformer(tolower))
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords,
                        c(stopwords("SMART"), "and", "the", "with", "for", "from", "one", "only"))
      
      myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      sort(rowSums(m), decreasing = TRUE)
    })

wordcloud_rep <- repeatable(wordcloud)

# Plot the wordcloud
  output$plot <- renderWordcloud2({
    v <- terms()
    d <- data.frame(word = names(v),freq=v)
    d <- head(d, input$number)
    
    # Limit the display to 30 words with top frequency
    wordcloud2(d, size = 1)
  
  })
  
    
}

shinyApp(ui, server)