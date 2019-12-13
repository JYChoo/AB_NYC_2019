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