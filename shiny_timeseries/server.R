library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  
  output$distPlot <- renderPlot({
    date.min <- as.Date(input$range[1], origin = "1970-01-01")
    date.max <- as.Date(input$range[2], origin = "1970-01-01")
    
    if (input$var != "all") sex = input$var
    if (input$var == "all") sex = c("male", "female")
    
    if (input$var2 == "days") incidence.title = "Daily"
    if (input$var2 == "weeks") incidence.title = "Weekly"
    if (input$var2 == "months") incidence.title = "Monthly"
    
    # draw the histogram with the specified number of bins
    hist(subset(df, (date.min<=ERU & ERU<=date.max & SEX %in% sex))$ERU, breaks = input$var2, start.on.monday=FALSE, col = 'skyblue', border = 'white',
         xlab=paste(incidence.title,"incidence",sep=" "), main="")
    #ts.plot(subset(df, (date.min<=ERU & ERU<=date.max & SEX %in% sex))$ERU, xlab=paste(incidence.title,"incidence",sep=" "), main="")
  })
})

#setwd("/Users/rolinavangaalen/Documents/Hackout/time/")
#runApp("shiny_timeseries")
#runApp("shiny_timeseries", display.mode = "showcase")
