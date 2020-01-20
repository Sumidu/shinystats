# shinystats
Shiny applications that help understand statistics 

# How to use these apps
- Clone the repository into RStudio
- Go to one of the subdirectories (e.g. "Verteilungen", "Korrelationen")
- Open the "app.R" and click on Run App

Tip: Regularly pull current changes for new Apps.


Alternatively you can run one of the following commands in your r console. This requires that you have the `shiny` package installed.

```r
shiny::runGitHub("shinystats", "sumidu", subdir = "Korrelationen")

shiny::runGitHub("shinystats", "sumidu", subdir = "Verteilungen")

shiny::runGitHub("shinystats", "sumidu", subdir = "Histogramme")

```
