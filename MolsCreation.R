#imports SDF file with multiple molecule and produces individual molecule files labeled by name

#install packages and write libraries
#if (!require("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
#BiocManager::install(version = "3.16")
#BiocManager::install("ChemmineR")
library("ChemmineR")

#import files
sdfset <- read.SDFset("/Users/ilincaflacau/APResearch/Inputs/appDrugs.sdf")
valid <- validSDF(sdfset); sdfset <- sdfset[valid]

setwd("/Users/ilincaflacau/APResearch/Inputs/appMolsSDF")
block <-toString(datablock(sdfset[[3]]))

file_names <- c()
IDs <- c()
molNames <- c()
#export individual SDFs
count = 0 
for (i in 1:length(sdfset)) {
  #get file name
  block <-toString(datablock(sdfset[[i]]))
  bound <- unlist(gregexpr("," , block))
  molName <- substr(block, bound + 2, nchar(block))
  molNames[i] <- molName
  
  ID <- substr(block, 1, bound-1)
  IDs[i] <- ID
  
  file_name <- paste(ID, ".sdf", sep = "")
  file_names[i] <- file_name
  
  #get individual files and export
  #write.SDF(sdfset[[i]], file = file_name, cid=file_name)
  count = count+1
}

idNamesMatch <- data.frame(IDs, molNames)
count #verify all are done