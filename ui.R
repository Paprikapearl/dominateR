library(shiny)
library(plotly)
# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Basketball.de #winning"),
  
  sidebarPanel(),
  
  mainPanel(
    dataTableOutput('mytable'),
    plotlyOutput('meanPlot', height = "640px"),
    plotlyOutput('lastPlot', height = "640px")
  )
))
