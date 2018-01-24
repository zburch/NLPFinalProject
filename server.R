# Author: Zack Burch
# Email: zack.s.burch@gmail.com
#
# This is the server logic of a Shiny web application model that gives
# three predicted words when the user inputs a phrase in the UI.
#

library(shiny)

shinyServer(function(input, output) {

  bigrams <- read.csv("biSummary.csv",row.names=NULL)
  trigrams <- read.csv("triSummary.csv",row.names=NULL)
  quadgrams <- read.csv("quadSummary.csv",row.names=NULL)
  def <- read.csv("defaults.csv",row.names=NULL)
  numResults <- 3
  
  model <- function(oldest=NULL,middle=NULL,newest){
    preds <- NULL
    oldest <- if(!is.null(oldest)) tolower(oldest)
    middle <- if(!is.null(middle)) tolower(middle)
    newest <- tolower(newest)
    
    head(def)
    if (is.null(oldest) & is.null(middle)){
      preds <- bigrams[bigrams$first==newest,2]
      if(length(preds)<numResults){
        preds <- unique(c(as.character(preds),as.character(def)))
      }
      preds <- preds[1:numResults]
    } else if(is.null(oldest)){
      preds <- trigrams[trigrams$first==middle & trigrams$second==newest,3]
      if (length(preds)<numResults){
        biPreds <- bigrams[bigrams$first==newest,2]
        preds <- unique(c(as.character(preds),as.character(biPreds)))
      }
      if (length(preds)<numResults){
        preds <- unique(c(as.character(preds),as.character(def)))
      }
      preds <- preds[1:numResults]
    } else {
      preds <- quadgrams[quadgrams$first==oldest & quadgrams$second==middle &
                           quadgrams$third==newest,4]
      if (length(preds)<numResults){
        triPreds <- trigrams[trigrams$first==middle & trigrams$second==newest,3]
        preds <- unique(c(as.character(preds),as.character(triPreds)))
      }
      if (length(preds)<numResults){
        biPreds <- bigrams[bigrams$first==newest,2]
        preds <- unique(c(as.character(preds),as.character(biPreds)))
      }
      if (length(preds)<numResults){
        preds <- unique(c(as.character(preds),as.character(def)))
      }
      preds <- preds[1:numResults]
    }
    return(preds)
  }

  
  output$value1 <- renderText({
    usrInput <- reactive({ input$phrase })
    if(usrInput()=="Your phrase"){
      "Enter text in the box to the left."
    } else {
      str <- tail(strsplit(usrInput(),split=" ")[[1]],3)
      oldest <- if(length(str)==3) str[1]
      middle <- if(length(str)==3|length(str)==2) {
                    str[length(str)-1]
                }
      newest <- str[length(str)]
      retVal <- model(oldest,middle,newest)
      paste(tolower(usrInput()),retVal[1])
    }
  })
  output$value2 <- renderText({
    usrInput <- reactive({ input$phrase })
    if(usrInput()=="Your phrase"){
      "Enter text in the box to the left."
    } else {
      str <- tail(strsplit(usrInput(),split=" ")[[1]],3)
      oldest <- if(length(str)==3) str[1]
      middle <- if(length(str)==3|length(str)==2) {
        str[length(str)-1]
      }
      newest <- str[length(str)]
      retVal <- model(oldest,middle,newest)
      paste(tolower(usrInput()),retVal[2])
    }
  })
  output$value3 <- renderText({
    usrInput <- reactive({ input$phrase })
    if(usrInput()=="Your phrase"){
      "Enter text in the box to the left."
    } else {
      str <- tail(strsplit(usrInput(),split=" ")[[1]],3)
      oldest <- if(length(str)==3) str[1]
      middle <- if(length(str)==3|length(str)==2) {
        str[length(str)-1]
      }
      newest <- str[length(str)]
      retVal <- model(oldest,middle,newest)
      paste(tolower(usrInput()),retVal[3])
    }
  })
})
