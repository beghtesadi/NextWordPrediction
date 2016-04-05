source("./PredictNextWord.R")


shinyServer(function(input, output) {
  
  wordPrediction <- reactive({
    text <- input$text
    cleaninput <- CleanInput(text)
    wordPrediction <- NextWord(cleaninput)})
  
  output$predictedWord <- renderPrint(wordPrediction())
  #output$enteredWords <- renderText({ input$text }, quoted = FALSE)
})