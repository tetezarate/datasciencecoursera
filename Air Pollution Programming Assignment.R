#install.packages("readr")
#install.packages("pastecs")
pollutantmean <- function(directory, pollutant, id=1:332) {
  folderPath <- file.path(getwd(),directory)
  fileList <- list.files(folderPath)
  for (file in fileList) {
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read.csv(file.path(folderPath,file), header=TRUE, sep=",")
    } else{
      temp_dataset <- read.csv(file.path(folderPath,file), header=TRUE, sep=",")
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  vectorToAnalyze <- dataset[[pollutant]]
  mean(vectorToAnalyze, na.rm = TRUE)
}









