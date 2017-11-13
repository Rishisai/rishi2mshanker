shinyServer(
function(input ,output){
  
  output$myPlot <- renderPlot({
    
    
    distType <- input$Distribution
    size <- input$SampleSize
    
    if(distType == "Normal"){
     
       randomVec <- rnorm(size, mean = as.numeric(input$mean),sd = as.numeric(input$sd))
    }
    else{
      
      randomVec <- rexp(size ,rate = 1/as.numeric(input$lambda))
    }
    
   hist(randomVec,col = "blue")
    
  })
})
  
  
  
  
)