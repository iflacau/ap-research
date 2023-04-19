#Process and analyze results from 
#packages
#install.packages("tidyr")
#install.packages("readr")
#install.packages("ggplot2")  # Install & load ggplot2 package

library("readr")
library("tidyr")
library("ggplot2")


#creating storage matrix 
filler_data <- rep(NA, 9 * 4 * length(IDs))
results <-array(filler_data, c(9, 4, length(IDs)))
dim(results) <- c(9, 4, length(IDs))
col_names <- c("energy", "rmsd lb", "rmsd ub", "kD")
row_names <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
matrix_names <- c(IDs)
dimnames(results) <- list(row_names, col_names, matrix_names)


count = 0 
#populating storage matrix 
for(i in 1:length(file_names)) {
  #get files
  curr_name <- paste("/Users/ilincaflacau/APResearch/Results/", matrix_names[i], "_log.txt", sep = "")
  try({curr_file <- readLines(curr_name)
  
  #identify target lines
  mode_line <- grep("mode", curr_file)
  start_line <- mode_line + 3
  end_line <- start_line + 8

  #extract data from target lines
  for (j in start_line:end_line){
    k <- 1
    while (substr(curr_file[j], k, k)  == " "){
      k <- k+1 
    }
    
    m <- k
    while (substr(curr_file[j], m, m) != " ") {
      m <- m + 1
    }
   
    n <- m
    while (substr(curr_file[j], n, n)  == " "){
      n <- n+1 
    }
    
    p <- n
    while (substr(curr_file[j], p, p) != " ") {
      p <- p + 1
    }
    results[j - start_line + 1,1, i] <- as.numeric(substr(curr_file[j], n, p-1)) #binding energy
    
    q <- p
    while (substr(curr_file[j], q, q)  == " "){
      q <- q + 1 
    }
   
    r <- q
    while (substr(curr_file[j], r, r) != " ") {
      r <- r + 1
    }
    
    results[j - start_line + 1,2, i] <- as.numeric(substr(curr_file[j], q, r-1)) #rmsd lb
    
    s <- r
    while (substr(curr_file[j], s, s)  == " "){
      s <- s + 1 
    }
    
    t <- s
    while (substr(curr_file[j], t, t) != " " && t <= nchar(curr_file[j])) {
      t <- t + 1
    }
    
    
    results[j - start_line + 1, 3, i] <- as.numeric(substr(curr_file[j], s, t-1)) #rmsd ub
  
  }
  
  count = count + 1
  }, silent = TRUE)
  # Calculating kD values using --> G = RTln(kD), 1 kcal = 4184
  results[,4,i] <- exp(results[,1,i] * 4184 /(8.314 * 298.15) )
}

