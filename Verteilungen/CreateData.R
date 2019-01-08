# ---- CreateData:
# ------ returns data as a tibble
# ------ simulates beer consumption per capita and year of students
# ------ normal / poisson / uniform distribution

CreateData <- function(input_samplesize, 
                       input_mean = 150, 
                       input_sd = 25, 
                       input_lambda = 150, 
                       distribution = c(F, F, F)){
  
  # ---- create temporary beer dataframe
  if(distribution[1]){
    beer_temp <- data.frame(rnorm(n = input_samplesize, mean = input_mean, sd = input_sd))
  }else if(distribution[2]){
    beer_temp <- data.frame(rpois(n = input_samplesize, lambda = input_lambda))
  }else{
    beer_temp <- data.frame(runif(n = input_samplesize, min = 0, max = input_mean * 2))
  }
  names(beer_temp) <- "Bierkonsum"
  
  # ---- initialize study subject column
  stud_temp <- c()
  
  # ---- create column with study subject
  # ---- make it more likely that mechanical engineers drink more beer
  for(i in 1:input_samplesize){
    if(beer_temp$Bierkonsum[i] > mean(beer_temp$Bierkonsum)){
      tmp <- runif(1)
      if(tmp > 0.3){
        stud_temp <- rbind(stud_temp, c("Maschinenbau"))
      }else{
        stud_temp <- rbind(stud_temp, c("anderer Studiengang"))
      }
    }else if(beer_temp$Bierkonsum[i] <= mean(beer_temp$Bierkonsum)){
      tmp <- runif(1)
      if(tmp > 0.3){
        stud_temp <- rbind(stud_temp, c("anderer Studiengang"))
      }else{
        stud_temp <- rbind(stud_temp, c("Maschinenbau"))
      }
    }
  }
  
  # ---- create final tibble beer
  stud_temp <- as.data.frame(stud_temp)
  temp_frame <- cbind(beer_temp$Bierkonsum, stud_temp)
  beer <- as.tibble(temp_frame)
  names(beer) <- c("Bierkonsum", "Studiengang")
  
  # ---- clean up environment
  rm(beer_temp, stud_temp, i, tmp, temp_frame)
  
  return(beer)
}