library(shiny)
library(BEST)

summary <- reactive ({
  BESTout <- BESTmcmc(input$data1, input$data2,numSavedSteps = 30000)
  
  summary(BESTout, credMass= input$g, ROPEm=c(input$d,input$c), 
          ROPEeff=c(input$f,input$e))
})

plot <- reactive({
  
  plotAll(BESTout,  ROPEm=c(input$d,input$c), 
          ROPEeff=c(input$f,input$e), compValm=input$a, showCurve=TRUE)
  
})

output$summary.out <- renderPrint({
  summary()
})

output$plot.out <- renderPlot({
  print(plot())
})



})


mainPanel(
  tabsetPanel(
    tabPanel("Summary", verbatimtextOutput("summary.out")),
    tabPanel("Plot", plotOutput("plot.out"))
     )