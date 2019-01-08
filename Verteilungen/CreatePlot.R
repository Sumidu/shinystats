# ---- CreatePlot:
# ------ returns plot created from input parameters

CreatePlot <- function(data, input_comparison, input_plottype, distribution){

  beer <- data
  
  if(distribution == "pois"){
    y_limit <- 300
  }else{
    y_limit <- 500
  }
  
  # ---- build up plot from input parameters
  if(input_comparison == "gesamter Datensatz"){
    p <- ggplot(data = beer, aes(x = 0, y = Bierkonsum)) +
      scale_x_discrete()
  }else{
    p <- ggplot(data = beer, aes(x = Studiengang, y = Bierkonsum))
  }
  p <- p +
    scale_y_continuous("Bierkonsum [l/Jahr]", limits = c(0, y_limit))
  
  if(is.element("Scatterplot", input_plottype)){
    p <- p + geom_jitter(shape = 1, col = "red", alpha = 0.5, width = 0.1)
  }
  if(is.element("Violinplot", input_plottype)){
    p <- p + geom_violin(alpha = 0.5, fill = "yellow")
  }
  if(is.element("Boxplot", input_plottype)){
    p <- p + geom_boxplot(alpha = 0.5, fill = "blue")
  }
  
  # add theme and return p
  p <- p + theme_bw()
  return(p)

}