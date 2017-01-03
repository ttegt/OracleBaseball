library(shiny)
bt2016<-read.csv("files/batters2016.csv")
bt2015<-read.csv("files/batters2015.csv")
pt2016<-read.csv("files/pitchers2016.csv")
pt2015<-read.csv("files/pitchers2015.csv")

bt<-bt2016

shinyServer(function(input, output) {

  
  
  output$dTable <- renderDataTable({data<-switch(input$table,
                                                  "2016 Batting"=bt2016,
                                                  "2015 Batting"=bt2015,
                                                  "2016 Pitching"=pt2016,
                                                  "2015 Pitching"=pt2015)
  },options=list(pageLength=10))
    
})

