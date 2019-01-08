# plot generieren: boxplot und scatterplot
# Frage: Welcher der folgenden Aussagen würdest Du zustimmen?
# Antwortmöglichkeiten: zufällig verteilt aus Fundus von Antwortmöglichkeiten
# Pool von mehreren TRUE-Items und FALSE-Items
# return Vektor mit einem TRUE-Item und drei FALSE-Items
# return Vektor mit TRUE- und FALSE-Werten entsprechend der Antwortmöglichkeiten



GenerateQuestion <- function(){
  random_mean <- 50 + round(runif(1) * 100)
  random_sd <- round(runif(1) * 50)
  beer <- CreateData(input_samplesize = 300, 
                     input_mean = random_mean,
                     input_sd = random_sd,
                     distribution = c(T, F, F))
  plot <- CreatePlot(data = beer,
                     input_comparison = "gesamter Datensatz",
                     input_plottype = c("Scatterplot", "Boxplot"),
                     distribution = "norm")
  base_q <- "Der Median liegt bei"
  answers <- c(
    toString(random_mean + round(runif(1) * 50)),
    toString(random_mean - round(runif(1) * 21)),
    toString(random_mean + round(runif(1) * 104)),
    toString(random_mean - round(runif(1) * 13)),
    toString(random_mean)
  )
  indices <- sample.int(4, 3)
  choices <- sample(c(
    paste(base_q, answers[5]),
    paste(base_q, answers[indices[1]]),
    paste(base_q, answers[indices[2]]),
    paste(base_q, answers[indices[3]])
  ))
  return(list(plot, choices, indices))
}



