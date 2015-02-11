library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Interactive time series"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput("var", 
                  label = "Sex:",
                  choices = c("all", "male", "female"),
                  selected = "all"),
      
      selectInput("var2", 
                  label = "Break by:",
                  choices = c("days", "weeks", "months"),
                  selected = "weeks"),
      
      sliderInput("range",
                  "Dates:",
                  min = as.numeric(min(df$ERU)),
                  max = as.numeric(max(df$ERU)),
                  value = c(as.numeric(min(df$ERU)),as.numeric(max(df$ERU))))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
