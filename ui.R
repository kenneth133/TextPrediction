library(shiny)

shinyUI(fluidPage(
    titlePanel("Text Prediction"),
    sidebarLayout(
        sidebarPanel(strong("Note: "), "Algorithm will predict the next word when the last character entered is a space.  
                     Otherwise, it will predict or correct the last word entered."),
        mainPanel(
            textInput(inputId="text1", label="Enter text:", width="100%"),
            p(strong("Predictions:")),
            verbatimTextOutput("predict1")
        )
    )
))
