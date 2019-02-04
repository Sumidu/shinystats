library(shiny)
library(ggplot2)

introtext <- "Pearson's r ist ein Maß für den Zusammenhang zwischen zwei Zufallsvariablen X und Y. Im 
Reiter (Hinführung) findest Du eine kurze Hinführung zur Pearson-Korrelation, im zweiten
Reiter (Zum Ausprobieren) kannst Du testen, wie sich bestimmte Parameter auf den 
Pearson-Korrelationskoeffizienten auswirken."

ui <- fluidPage(img(src = "logo.png", width = "150px"),
                h1("Pearson-Korrelation"),
                p(introtext),
                tabsetPanel(
                  tabPanel(
                    "Hinführung",
                    withMathJax(),  # MathJax display only works in Browser so far
                    p("Die Varianz ist wie folgt definiert:"),
                    br(),
                    helpText(
                      "$$var(X) = \\frac{\\sum_{} (x_i-\\bar{x})^2}{N-1} =
                      \\frac{\\sum_{} (x_i-\\bar{x})(x_i-\\bar{x})}{N-1}$$"),
                    p("Sie ist ein Maß dafür, wie stark eine Variable durchschnittlich von ihrem
                      Mittelwert abweicht."),
                    p("Bei der Covarianz schauen wir uns an, wie stark zwei Variablen zusammen
                      variieren. Sie ist daher wie folgt definiert:"),
                    br(),
                    helpText(
                      "$$cov(X, Y) = \\frac{\\sum_{} (x_i-\\bar{x})(y_i-\\bar{y})}{N-1}$$"),
                    p("Das Problem an der Covarianz ist, dass sie von der Einheit der betrachteten 
                      Variablen abhängt. Daher sind Covarianzwerte nicht gut vergleichbar, da wir
                      nicht wissen, wie sie jeweils zu interpretieren sind. Pearson's 
                      Korrelationskoeffizient schafft hier Abhilfe. Wir drücken die Covarianz einfach
                      in Standardabweichungen anstatt in der Einheit der zugrundeliegenden Variablen
                      aus. Damit erhalten wir folgende Formel:"),
                    helpText(
                      "$$r = \\frac{cov(X, Y)}{s_x s_y} = 
                      \\frac{\\sum_{} (x_i-\\bar{x})(y_i-\\bar{y})}{(N-1) s_x s_y}$$"),                    
                    NULL
                    ),
                  tabPanel(
                    "Zum Ausprobieren",
                    sliderInput(
                      inputId = "n",
                      "Stichprobengröße",
                      min = 1,
                      value = 250,
                      max = 5000
                    ),
                    numericInput(inputId = "seed", "Stichprobennummer", value = 1),
                    sliderInput(
                      inputId = "r",
                      "Korrelationskoeffizient",
                      value = 0,
                      min = -1,
                      max = 1,
                      step = 0.05
                    ),
                    plotOutput(outputId = "hist")
                  ),
                  tabPanel(
                    "Zum Üben",
                    p(strong("Korrelationen raten")),
                    br(),
                    p("Folge diesem Link um zu Prüfen, wie gut Du Korrelationen erkennen kannst. Du solltest nach einiger Übung etwa 9 von 10 Korrelationen richtig raten können."),
                    tagList("Diesen Link klicken:", a(" Korrelationen raten", href="http://guessthecorrelation.com/")),
                    br(),
                    br(),
                    br(),
                    br(),
                    
                    NULL
                    )
                  # tabPanel("Experiment", "contents"),
                  # tabPanel("tab 3", "contents")
                    ))


complement <- function(y, rho, x) {
  if (missing(x))
    x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho ^ 2)
}

server <- function(input, output) {
  output$hist <- renderPlot({
    n     <- input$n                    # length of vector
    rho   <- input$r                    # desired correlation = cos(angle)
    set.seed(input$seed)
    x <- rnorm(n)
    y <- complement(x, rho)
    df <- data.frame(x, y)
    ggplot(df, aes(x = x, y = y)) + geom_point(alpha = 0.5, color = "dark blue") +
      labs(title = "Pearson-Korrelation", x = "Variable x", y = "Variable y") +
      lims(x = c(-4, 4), y = c(-4, 4)) +
      theme_minimal() +
      coord_fixed()
    
  })
  output$correlation <- renderText({
    cor.test(dista, distb)
  })
}

shinyApp(ui, server)
