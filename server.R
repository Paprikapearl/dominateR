library(plotly)
library(shiny)

load("dominateR2019.RData")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  output$mytable = renderDataTable({
    stat[,c("Name", "Mean", "games", "sd", "min last 5", "min else", "p value mins", "mean last 5", "mean else", "p value means", "points total", "pos", "gehalt", "ppgl", "value", "valuation")]
  })
  
  output$meanPlot <- renderPlotly({
    
    p1 <- ggplot(stat, aes(x = Mean, y = gehalt, label = Name)) + 
      geom_point() +
      stat_smooth(data = stat[stat$games > 5 & stat$gehalt > 0.5,], method = "lm", col = "red")
    
    gg1 <- ggplotly(p1)
    
  })
  
  output$lastPlot <- renderPlotly({
    
    p2 <- ggplot() + 
      geom_point(data = stat, aes(x = stat$`mean last 5`, y = gehalt, label = Name)) +
      stat_smooth(data = stat[stat$games > 5 & stat$gehalt > 0.5,], aes(x = Mean, y = gehalt), formula = y ~ x, method = "lm", col = "red")
    
    gg2 <- ggplotly(p2)
    
  })

  
})
