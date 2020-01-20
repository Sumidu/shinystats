#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Histogramme für Normalverteilte Daten"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "Anzahl Werte", min = 5, max = 1000, value = 25),
            sliderInput("mean_v", "Mittelwert", min = -50, max = 50, value = 0, step = 0.1),
            sliderInput("sd_v", "Standardabweichung", min = 10, max = 50, value = 20, step = 1),
            sliderInput("bins",
                        "Anzahl bins:",
                        min = 1,
                        max = 200,
                        value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- rnorm(input$n, input$mean_v, input$sd_v)
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        
        df <- data.frame(x)
        df %>% ggplot() + aes(x = x) + geom_histogram(bins = input$bins) + theme_minimal(base_size = 20) +
            labs(x = "Variable", y = "Anzahl", caption = paste("Bins:",input$bins),
                 title = "X-Achse ist von -100 bis 100 fest eingestellt.") +
            lims(x = c(-100, 100))
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
