#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
#load models
model1<-readRDS("./lm.fitNews.rds")
model2<-readRDS("./lm.fitRadio.rds")
model3<-readRDS("./lm.fitTV.rds")
model4<-readRDS("./lm.fitSales.rds")
# Define UI
ui <- fluidPage(
  headerPanel(
    # Combine title and image using a div
    title = div(
      "How to divide investment to get that sales?"
    )),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "user_input",
        label = "Total invest:Enter a numeric value between 0 and 910:",
        value = 0,
        min = 0,
        max = 910
      )
    ),
    mainPanel(
      h4("Model Prediction:"),
      h4("Prediction Sales with that investment"),
      verbatimTextOutput("prediction_outputSales"),
      h4("Prediction media TV"),
      verbatimTextOutput("prediction_outputTV"),
      h4("Prediction media Newspaper"),
      verbatimTextOutput("prediction_outputNews"),
      h4("Prediction media Radio"),
      verbatimTextOutput("prediction_outputRadio")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive expression to generate prediction
  # Replace this with your actual model prediction logic
  #total_invest=0

  #Newspaper
  predictionNews <- reactive({
    x <- input$user_input
    x<-as.data.frame(x)
    names(x)<-"Total_invest"
    # Example: Simple linear model y = 2x + 3
    y_pred <- round(predict(model1,x),digits = 2)
    if (y_pred<0) y_pred<- -y_pred/10
    if (x$Total_invest==0) y_pred<- 0
    return(y_pred)
  })
  #Radio
  predictionRadio <- reactive({
    x <- input$user_input
    x<-as.data.frame(x)
    names(x)<-"Total_invest"
    # Example: Simple linear model y = 2x + 3
    y_pred <- round(predict(model2,x),digits = 2)
    if(y_pred<0) y_pred<--y_pred/10
    if (x$Total_invest==0) y_pred<- 0
    return(y_pred)
  })
  #TV
  predictionTV <- reactive({
    x <- input$user_input
    x<-as.data.frame(x)
    names(x)<-"Total_invest"
    # Example: Simple linear model y = 2x + 3
    y_pred <- round(predict(model3,x),digits = 2)
    if (x$Total_invest==0) y_pred<- 0
    return(y_pred)
  })
  #Sales
  predictionSales <- reactive({
    x <- input$user_input
    x<-as.data.frame(x)
    names(x)<-"Total_invest"
    # Example: Simple linear model y = 2x + 3
    y_pred <- round(predict(model4,x),digits = 2)
    if (x$Total_invest==0) y_pred<- 0
    return(y_pred)
  })

  # Output the prediction Sales
  output$prediction_outputSales <- renderPrint({
    predictionSales()})
  # Output the prediction TV
  output$prediction_outputTV <- renderPrint({
      predictionTV()})
  # Output the prediction Newspaper
  output$prediction_outputNews <- renderPrint({
      predictionNews()})
  # Output the prediction Radio
    output$prediction_outputRadio <- renderPrint({
        predictionRadio()})

}

# Run the app
shinyApp(ui = ui, server = server)
