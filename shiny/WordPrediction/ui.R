

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predicting Current and Next Word"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("inputString", "Type some text",value = ""),
      br(),
      strong("Try typing the following:"),
      br(),
      HTML("Gossip is idle talk or rumor, especially about the personal or private affairs of others; the act is also known as dishing or tattling. Gossip has been researched in terms of its evolutionary psychology origins. Wikipedia"),
      br(),
      strong("Current Input:"),
      tags$style(type='text/css', '#text1 {color: blue;}'), 
      textOutput('text1')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("Predicted"),
      htmlOutput("predicted"),
      tags$style(type='text/css', '#predicted {background-color: rgba(255,255,0,0.40); color: blue;}'),
      br(),br()
      
    )
    
  )
))
