# Author: Zack Burch
# Email: zack.s.burch@gmail.com
# 
# This is the UI for a predictive text application.

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Predictive Text Application"),
  
  sidebarLayout(
    sidebarPanel(
       textInput("phrase",
                 "Enter a phrase:",
                 "Your phrase"),
       submitButton("Submit")
    ),
    
    mainPanel(
      h4("Top Choice"),
      verbatimTextOutput("value1"),
      h4("Second Choice"),
      verbatimTextOutput("value2"),
      h4("Third Choice"),
      verbatimTextOutput("value3")
    )
  )
))
