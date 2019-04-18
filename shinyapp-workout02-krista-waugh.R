library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Visualizing Savings: Three Modes of Investing"),
  fluidRow(
    column(4,
           
           sliderInput("amount","Initial Amount",
                       min = 0,
                       max = 100000,
                       step = 500,
                       value = 1000),
           sliderInput("contribution","Annual Contribution",
                       min = 0,
                       max = 50000,
                       step = 500,
                       value = 2000)
    ),
    column(4,
           
           sliderInput("rate","Return Rate (in %)", 
                       min = 0,
                       max = 20,
                       step = .1,
                       value = 5),
           sliderInput("growth","Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       step = .1,
                       value = 2)
    ),
    column(4,
           
           sliderInput("years","Years", 
                       min = 1,
                       max = 50,
                       step = 1,
                       value = 20),
           selectInput("facet","Facet?",choices = c("No","Yes"))
    )
  ),
  mainPanel(
   h2("Timelines")),
   plotOutput("graph"),
    h2("Balances"),
    verbatimTextOutput("dataframe",placeholder = FALSE)
    
  )


# Define server logic required to draw a histogram

future_value <- function(amount, rate, years) {
  fv = amount*(1 + rate)^years
  
  return(fv)
}  

annuity <- function(contrib, rate, years) {
  fva = contrib*(((1 + rate)^years -1)/rate)
  
  return(fva)
}  

growing_annuity <- function(contrib, rate, growth, years) {
  fvga = contrib*((1 + rate)^years -(1 + growth)^years)/(rate - growth)
  
  return(fvga)
}

server <- function(input, output) {
    dat <- reactive({
      no_contrib <- input$amount
      fixed_contrib <- input$amount
      growing_contrib <- input$amount
      for (i in  2:(input$years +1)) {
        no_contrib[i] <- future_value(input$amount,(input$rate)/100,i-1)
      }
      for (i in  2:(input$years +1))
      {
        fixed_contrib[i] <- annuity(input$contribution,(input$rate)/100,i-1) + no_contrib[i]
      }
      for (i in  2:(input$years +1))
      {
        growing_contrib[i] <- growing_annuity(input$contribution,(input$rate)/100,(input$growth/100),i-1)+ no_contrib[i]
    }
    
    server_data <- data.frame(1:(input$years+1),no_contrib,fixed_contrib,growing_contrib)
    names(server_data) <- c("year","no_contrib","fixed_contrib","growing_contrib")
    return(server_data)
  })
  output$dataframe <- renderPrint(dat())
  
  output$graph <- renderPlot({
    server_data <- gather(dat(),key="type","amount",2:4)
    server_data$type = factor(server_data$type, levels=c("no_contrib","fixed_contrib","growing_contrib"))
    
    if (input$facet=="Yes")
    {
      ggplot(server_data,aes(x=year,y=amount, color=type)) + geom_line() + facet_wrap(~type) +
        geom_area(aes(fill=type),alpha=.7) + labs(title="Three modes of investing", x="year", y = "value") 
    }
    else
    {
      ggplot(server_data,aes(x = year,y = amount, color=type)) + geom_line() + labs(title="Three modes of investing", x="year", y = "value") 
    }
    
  })
  
}


shinyApp(ui = ui, server = server)