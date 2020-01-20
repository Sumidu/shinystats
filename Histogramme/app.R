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
            sliderInput("n", "Anzahl Werte", min = 5, max = 1000, value = 100),
            sliderInput("mean_v", "Mittelwert", min = -50, max = 50, value = 0, step = 1),
            sliderInput("sd_v", "Standardabweichung", min = 10, max = 50, value = 20, step = 1),
            sliderInput("bins",
                        "Anzahl bins:",
                        min = 1,
                        max = 200,
                        value = 50),
            checkboxInput("help", "Hilfslinien einzeichnen", value = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
            h3("Mögliche Übungsfragen"),
            p("1.) Können Sie anhand des Histogramms (ohne Hilfslinien) erkennen, wo Mittelwert und Standardabweichung liegen?"),
            p("2.) Können Sie anhand des Histogramms (mit etwa 10 Bins) erkennen, wieviele Werte im Datensatz enthalten sind?"),
            p("3.) Können Sie die Standardabweichung (ohne Hilfslinien) schätzen?"),
           plotOutput("distPlot"),
           h4("Interpretationshilfe"),
           p("Grüne Linie = Mittelwert, rote Linie = eine Standardabweichung zum Mittelwert")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    dataf <- reactive({
        x <- rnorm(input$n, input$mean_v, input$sd_v)
        data.frame(x)
    })
   
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        df <- dataf()
        p <- df %>% ggplot() + aes(x = x) + geom_histogram(bins = input$bins) + theme_minimal(base_size = 20) +
            labs(x = "Variable", y = "Anzahl", caption = paste("Bins:",input$bins),
                 title = "X-Achse ist von -100 bis 100 fest eingestellt.") +
            scale_x_continuous(breaks = c(-100, -50, 0, 50, 100), limits = c(-100, 100))
        if(input$help){
            p <- p + geom_vline(xintercept = input$mean_v, color = "green")+
                geom_vline(xintercept = input$mean_v + input$sd_v, color = "red")+
                geom_vline(xintercept = input$mean_v - input$sd_v, color = "red")+
                scale_x_continuous(breaks = c(-100, -50, 0, 
                                              input$mean_v - input$sd_v, 
                                              input$mean_v + input$sd_v, 
                                              input$mean_v,  50,
                                              100), 
                                   limits = c(-100, 100))
        }
            p
            #lims(x = c(-100, 100),)
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
