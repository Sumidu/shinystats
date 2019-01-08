# ---- load libraries and source functions
library(shiny)
library(tidyverse)
source("CreateData.R")
source("CreatePlot.R")
source("TestQuestions.R")
introtext <- "Stell Dir vor, Du hast den Bierkonsum von Studenten in Aachen erhoben. 
              Die Ergebnisse hast Du in einem Datensatz festgehalten, der zwei Variablen
              enthält: Bierkonsum (in l/Jahr) und Studiengang (Maschinenbau oder anderer
              Studiengang). In dieser App kannst Du aus diesem Datensatz mehrere Arten von
              Plots erstellen (Boxplots, Violinplots und Scatterplots). Spiel ein bisschen
              mit den Parametern herum, um ein Gefühl dafür zu bekommen, was jeder der Plots
              anzeigt und was die Lage- und Streuungsmaße, die Du angeben kannst, verändern."

# ---------------------------------------------------------------------------------------------- #

# ---- define ui
ui <- navbarPage(
  
  # ---- title (upper left corner)
  title = "Lage- und Streuungsmaße",
  
  # ---------------------------------------- #
  # ---- first panel with dropdown menu ---- #
  # ---------------------------------------- #
  navbarMenu(
    title = "Verteilungen",
    # ---- normal distribution
    tabPanel(
      # ---- title and introduction text
      title = "Normalverteilung",
      h2("Normalverteilung"),
      p(introtext), 
      # ---- functional panel contents
      sidebarLayout(
        sidebarPanel(
          # ---- set parameters for dataset and plot creation
          sliderInput(
            inputId = "norm_samplesize", label = "Stichprobengröße",
            min = 2, max = 300, value = 50
          ),
          sliderInput(
            inputId = "norm_mean", label = "Mittelwert",
            min = 75, max = 250, value = 150
          ),
          sliderInput(
            inputId = "norm_stddev", label = "Standardabweichung",
            min = 0, max = 125, value = 25
          ),
          radioButtons(
            inputId = "norm_comparison", label = "Anzeige",
            choices = c("gesamter Datensatz", "Aufteilung nach Studiengang")
          ),
          checkboxGroupInput(
            inputId = "norm_plottype", label = "Plot",
            choices = c("Scatterplot", "Boxplot", "Violinplot")
          ),
          actionButton(
            inputId = "norm_generate", label = "neuen Datensatz generieren"
          )
        ),
        # ---- plot output
        mainPanel(
          plotOutput("norm_plot")
        )
      )
    ),
    
    # ----------------------------- #
    # ---- poisson distribution---- #
    # ----------------------------- #
    tabPanel(
      # ---- title and introduction text
      title = "Poissonverteilung",
      h2("Poissonverteilung"),
      p(introtext),
      # ---- functional panel contents
      sidebarLayout(
        sidebarPanel(
          # ---- set parameters for dataset and plot creation
          sliderInput(
            inputId = "pois_samplesize", label = "Stichprobengröße",
            min = 2, max = 300, value = 50
          ),
          sliderInput(
            inputId = "pois_lambda", label = "Lambda",
            min = 0, max = 250, value = 150
          ),
          radioButtons(
            inputId = "pois_comparison", label = "Anzeige",
            choices = c("gesamter Datensatz", "Aufteilung nach Studiengang")
          ),
          checkboxGroupInput(
            inputId = "pois_plottype", label = "Plot",
            choices = c("Scatterplot", "Boxplot", "Violinplot")
          ),
          actionButton(
            inputId = "pois_generate", label = "neuen Datensatz generieren"
          )
        ),
        # ---- plot output
        mainPanel(
          plotOutput("pois_plot")
        )
      )
    ),
    
    # ------------------------------ #
    # ---- uniform distribution ---- #
    # ------------------------------ #
    tabPanel(
      # ---- title and introduction text
      title = "Gleichverteilung",
      h2("Gleichverteilung"),
      p(introtext),
      # ---- functional panel contents
      sidebarLayout(
        sidebarPanel(
          # ---- set parameters for dataset and plot creation
          sliderInput(
            inputId = "unif_samplesize", label = "Stichprobengröße",
            min = 2, max = 300, value = 50
          ),
          sliderInput(
            inputId = "unif_mean", label = "Mittelwert",
            min = 75, max = 250, value = 150
          ),
          radioButtons(
            inputId = "unif_comparison", label = "Anzeige",
            choices = c("gesamter Datensatz", "Aufteilung nach Studiengang")
          ),
          checkboxGroupInput(
            inputId = "unif_plottype", label = "Plot",
            choices = c("Scatterplot", "Boxplot", "Violinplot")
          ),
          actionButton(
            inputId = "unif_generate", label = "neuen Datensatz generieren"
          )
        ),
        # ---- plot output
        mainPanel(
          plotOutput("unif_plot")
        )
      )
    )
  )

  # # -------------------- #
  # # ---- test panel ---- #
  # # -------------------- #
  # tabPanel(
  #   title = "Selbsttest",
  #   p("In diesem Modul kannst Du Dich selbst testen."),
  #   radioButtons(
  #     inputId = "q1",
  #     label = "Schau dir den folgenden Plot an.
  #              Welche der folgenden Einschätzungen ist richtig?",
  #     choices = output$choices_q1
  #   ),
  #   actionButton(
  #     inputId = "commit1",
  #     label = "Antwort bestätigen"
  #   ),
  #   actionButton(
  #     inputId = "regenerate",
  #     label = "neue Testfrage generieren"
  #   ),
  #   plotOutput("test_plot1"),
  #   textOutput("test1")
  #   
  # )
  
)

# ---------------------------------------------------------------------------------------------- #

# ---- server logic
server <- function(input, output){
  
  # ---- create plot from inputs on first panel (normal distribution)
  # ---- run when action button is triggered
  observeEvent(input$norm_generate, {
  
    # ---- plot from input
    output$norm_plot <- renderPlot({
      
      # ---- create dataset
      beer <- CreateData(input_samplesize = input$norm_samplesize, 
                         input_mean = input$norm_mean, 
                         input_sd = input$norm_stddev, 
                         distribution = c(T, F, F))

      # ---- create plot
      CreatePlot(data = beer,
                 input_comparison = input$norm_comparison,
                 input_plottype = input$norm_plottype,
                 distribution = "norm")
      
    })
  
  })
  
  # ---- create plot from inputs on second panel (poisson distribution)
  # ---- run when action button is triggered
  observeEvent(input$pois_generate, {

    # ---- plot from input
    output$pois_plot <- renderPlot({

      # ---- create dataset
      beer <- CreateData(input_samplesize = input$pois_samplesize, 
                         input_lambda = input$pois_lambda, 
                         distribution = c(F, T, F))

      # ---- create plot
      CreatePlot(data = beer,
                 input_comparison = input$pois_comparison,
                 input_plottype = input$pois_plottype,
                 distribution = "pois")

    })

  })
  
  # ---- create plot from inputs on third panel (uniform distribution)
  # ---- run when action button is triggered
  observeEvent(input$unif_generate, {
    
    # ---- plot from input
    output$unif_plot <- renderPlot({
      
      # ---- create dataset
      beer <- CreateData(input_samplesize = input$unif_samplesize, 
                         input_mean = input$unif_mean, 
                         distribution = c(F, F, T))
      
      # ---- create plot
      CreatePlot(data = beer,
                 input_comparison = input$unif_comparison,
                 input_plottype = input$unif_plottype,
                 distribution = "unif")
      
    })
    
  })
  
  
  # # --------------------- #
  # # ---- test module ---- #
  # # --------------------- #
  # 
  # # ---- build up test result outputs
  # # ---- run when action button is triggered
  # observeEvent(input$regenerate, {
  #   
  #   q1 <- GenerateQuestion()
  #   output$choices_q1 <- q1[[2]]
  #   output$indices_q1 <- q1[[3]]
  #   
  #   output$test_plot1 <- renderPlot({
  #     
  #     q1[[1]]
  #   
  #   })
  #   
  # })
  # 
  # # TO DO: adjust answers to randomly generated plot
  # observeEvent(input$commit1, {
  # 
  #   # ---- test if answer is correct
  #   if(input$q1 == output$choices_q1[[not(intersect(c(1, 2, 3, 4), output$indices_q1))]]){
  #     output$test1 <- renderText(
  #       "Richtig! :)"
  #     )
  #   }else{
  #     output$test1 <- renderText(
  #       "Leider falsch :("
  #     )
  #   }
  # 
  # })
  
}

# ---------------------------------------------------------------------------------------------- #

# ---- create shiny app
shinyApp(ui, server)