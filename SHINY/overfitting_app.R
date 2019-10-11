#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("~/Dropbox/LAURENT/COURS/EDHEC/LEARNING/SHINY")
source("generation.R")
source("trace_front.R")
library(ggplot2)
library(shiny)
library(class)
library(reshape2)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Overfitting"),
   
   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
      sidebarPanel(
         sliderInput("size_n",
                     "Number of points:",
                     min = 1,
                     max = 1000,
                     value = 5000),
         sliderInput("lambda",
                     "Complexity:",
                     min = 0,
                     max = 20,
                     value = 10),
         tableOutput("perf"),
         submitButton("Update View", icon("refresh"))
      ),
      
      mainPanel(
         plotOutput("front",width="80%")
#         plotOutput("train",width="80%")
      ),
      fluidRow(
        column(width = 6,
               plotOutput("train",width="80%")),
        column(width = 6,
               plotOutput("test",width="80%"))
      )
#   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive(generation(input$size_n))
  K <- reactive(c(1:5,seq(8,20,by=2),seq(20,50,by=10),80,100,130,0.3*input$size_n))
  
  KK <- reactive(rev(K())[input$lambda])
  prev_app <- reactive(
    if (length(KK())==0){
#      rep(round(mean(as.numeric(data()$train[,3])-1)),nrow(data()$train))
      rep(1,nrow(data()$train))
    } else {
      knn(data()$train[,1:2],data()$train[,1:2],
                            data()$train[,3],k=KK())
    }
  )
  prev_test <- reactive(
    if (length(KK())==0){
#      rep(round(mean(as.numeric(data()$train[,3])-1)),nrow(data()$test))
      rep(1,nrow(data()$test))
    } else {
      knn(data()$train[,1:2],data()$test[,1:2],
                        data()$train[,3],k=KK())
    }
  )
#  browser()
  output$perf <- renderTable(
    data.frame(Err.app=mean(prev_app()!=data()$train[,3]),
               Err.test=mean(prev_test()!=data()$test[,3]))
  )
  
  mod <- reactive(trace_front_gg(data()$train,methode="KNN",k=KK()))
  
   output$train <- renderPlot({
     train <- data()$train
#     ggplot(train)+aes(x=X1,y=X2,color=Y)+geom_point()+theme_bw()+
#       labs(title="Train")
     mod()+geom_point(data=train,aes(x=X1,y=X2,color=Y))+labs(title="Train")
   })
   output$test <- renderPlot({
     test <- data()$test
     mod()+geom_point(data=test,aes(x=X1,y=X2,color=Y))+labs(title="Test")
#     ggplot(test)+aes(x=X1,y=X2,color=Y)+geom_point()+theme_bw()+
#       labs(title="Train")
   })
   
   output$front <- renderPlot(
     mod()+labs(title="Classification rule")
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

