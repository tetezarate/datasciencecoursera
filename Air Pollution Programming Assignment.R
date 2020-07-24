#install.packages("readr")
#install.packages("pastecs")
pollutantmean <- function(directory, pollutant, id=1:332) {
  #Set the required directory
  folderPath <- file.path(getwd(),directory)
  fileList <- list.files(folderPath)
  idName<-id
  for(i in id){
    if(nchar(i) ==1){
      idName[i]<-paste("00",i,".csv",sep = "")
    } else if(nchar(i) ==2){
      idName[i]<-paste("0",i,".csv",sep = "")
    } else{
      idName[i]<-paste(i,".csv",sep = "")
    }
  }
  
  for (file in fileList) {
    if (file %in% idName){
      # if the merged dataset doesn't exist, create it
      if (!exists("dataset")){
        dataset <- read.csv(file.path(folderPath,file), header=TRUE, sep=",")
      } else{
        temp_dataset <- read.csv(file.path(folderPath,file), header=TRUE, sep=",")
        dataset<-rbind(dataset, temp_dataset)
        rm(temp_dataset)
      }
    }
  }
  vectorToAnalyze <- dataset[[pollutant]]
  mean(vectorToAnalyze, na.rm = TRUE)
}


complete <- function(directory, id=1:332){
  dataset <- data.frame(id=integer(),nobs=integer())
  folderPath <- file.path(getwd(),directory)
  fileList <- list.files(folderPath)
  idName<-id
  for(i in id){
    if(nchar(i) ==1){
      idName[i]<-paste("00",i,".csv",sep = "")
    } else if(nchar(i) ==2){
      idName[i]<-paste("0",i,".csv",sep = "")
    } else{
      idName[i]<-paste(i,".csv",sep = "")
    }
  }
  cont <- 0
  for (file in fileList) {
    if (file %in% idName){
      cont <- cont + 1
      dataset2 <- read.csv(file.path(folderPath,file), header=TRUE, sep=",")
      nobsTemp <- is.na(dataset2)
      nobsTemp <- nobsTemp[,2:3]
      nobsTemp<-nobsTemp[,1]+nobsTemp[,2]
      nobsTemp <- nobsTemp == 0
      nobsTemp <- sum(nobsTemp)
      id <- substr(file,1,3)
      dataset[cont,] <- c(as.numeric(id),nobsTemp)
    }
  }
  dataset
}


corr <- function(directory,threshold=0){
  folderPath <- file.path(getwd(),directory)
  fileList <- list.files(folderPath)
  cont <- 0
  corVector <- data.frame(corr=double())
  for (file in fileList) {
    if(complete(directory,as.numeric(substr(file,1,3)))[2]>=threshold){
      cont <- cont+1
      dataset <- na.omit(read.csv(file.path(folderPath,file), header=TRUE, sep=","))
      corVector[cont,] <- c(cor(dataset$nitrate,dataset$sulfate))
    }
  }
  if (!exists("dataset")){
    cr <- numeric()
  }else{
    cr <- corVector
  }
  cr
}










