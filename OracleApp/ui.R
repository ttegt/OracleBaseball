library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Oracle Ratings"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("table",
                  "Select table to display",
                  choices=c("2016 Batting","2015 Batting","2016 Pitching","2015 Pitching"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("dTable")
    )
  )
))
