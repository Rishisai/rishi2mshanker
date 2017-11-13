shinyUI(
  pageWithSidebar(
    headerPanel("My first Shiny App"),
    
    sidebarPanel(
      selectInput("Distribution","Please select distribution type",
                  choices = c("Normal","Exponential")),
      sliderInput("SampleSize","Please select sample size:",
                  min = 100, max = 5000, value = 1000, step = 100),
      conditionalPanel(condition ="input.Distribution =='Normal'",
                       textInput("mean", "Please select the mean",10),
                       textInput("sd","Please select standard deviation",3)),
      conditionalPanel(condition = "input.Distribution == 'Exponential'",
                       textInput("lambda","Please select exponential lambda:",1))
      ),
    mainPanel(
      plotOutput("myPlot")
    )
    
)
)
  
  