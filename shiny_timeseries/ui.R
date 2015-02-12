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
                  #format = '_locale.date.format', #'M/d/yyyy h:mm a', animate=TRUE,
                  #dateInput("ana", "Choose a date:", value = min(df$ERU)),
                  value = c(as.numeric(min(df$ERU)),as.numeric(max(df$ERU)))),
      
      dateInput("ana", "Choose a date:", value = min(df$ERU))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      singleton(tags$head(HTML(
        '
  <script type="text/javascript">
    $(document).ready(function() {
      var slider = $("#range").slider();
      // override the default "nice" function.
      var labels = slider.domNode.find(".jslider-label span");
      labels.eq(0).text(min(df$ERU));
      labels.eq(1).text(max(df$ERU));
      slider.nice = function(value) {
        var ref_date = new Date("2014-07-01");
        // each slider step is 1 day, translating to 24 * 3600 * 1000 milliseconds
        var slider_date = new Date(ref_date.getTime() + value * 24 * 3600 * 1000);
        return [slider_date.getUTCFullYear(), 
                slider_date.getUTCMonth() + 1, 
                slider_date.getUTCDate()].join("-");
      }
    })
  </script>
  '))),
      
      plotOutput("distPlot")
    )
  )
))
