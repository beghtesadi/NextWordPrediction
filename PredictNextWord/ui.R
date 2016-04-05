shinyUI(fluidPage(
  
  titlePanel("Predict Next Word"),
  
  sidebarLayout(
    
    sidebarPanel(textInput("text", label = h3("Text input"), value = "Hello"),
                 p("Type in a phrase above and press the PREDICT button below to view the suggested next word."),
                 submitButton("PREDICT") )  ,
  
  mainPanel(  h1("The predicted next word is:"),
      verbatimTextOutput("predictedWord")
    )
  )
))