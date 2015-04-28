# ==========================================================================
# This program is developed to diagnosis skin status of human face.
#                                       Created by Ph.D./Prof. Jaeho H. BAE
#                           Dept. of Industrial Management, Osan University
#                                                                Sep., 2014.
#                                                     knowhow.bae@gmail.com
# ==========================================================================

lib.call <- function() {
  # Load required libraries
  require("XLConnect")
  require("EBImage")
  require("colorscience")
  #library("extrafont")
  # Load required custom build libraries
  source("usedFunctions.r")
  source("transColorSpace.r")
  source("transScale.r")
  source("ellipsoid_test.r")
}

checkDirectory <- function() {
  currentWD <- paste(getwd(), "/srcImage", sep="")
  cat("Your DATA files must be at ", currentWD, "\n", sep="")
  ANS <- readline("Is it right? (Y/y or N/n): ")
  
  if ((substr(ANS, 1, 1)=="n") || (substr(ANS, 1, 1)=="N")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

getFileName <- function(type,...,fileID="") {
  currentWD <- paste(getwd(), "/srcImage", sep="")
  if (type==1) {
    fileName <- getwd()
  } else if(type==2) {
    fileName <- fileID
    fileName <- paste(currentWD, "/", fileName, ".png", sep="")
  } else if(type==3) {
    fileName <- readline("What is the Coordination file Name (without extension)? ")
    fileName <- paste(currentWD, "/", fileName, ".txt", sep="")
  } else if(type==4) {
    fileName <- readline("What is the source image file Name (without extension)? ")
  } 
  return(fileName)
}

transLoc <- function(locFile) {
  tmplocData <- read.table(locFile, header = FALSE, sep = ",")
  veclocData <- as.numeric(as.vector(tmplocData[1:length(tmplocData[,1])-1,1]))
  locData <- matrix(veclocData, ncol=2)
  return(locData)
}

saveXlsx <- function(dataWB, nameSheet, setData) {
  options(java.parameters = "-Xmx4096m")
  
  dataWS   <- createSheet(dataWB, name = nameSheet)
  writeWorksheet(dataWB, setData, sheet = nameSheet)
  #  saveWorkbook(dataWB)
  
  xlcFreeMemory()
  return(1)
}

saveImage   <- function(srcImage, prefix, seq, timeTag, save, imageType=1) {
  if (imageType == 1) {
    if (save=="Y") {
      dir.create(paste("./srcImage/", prefix, sep=""), showWarnings=FALSE)
      saveFileName   <- paste("./srcImage/", prefix, "/", seq, "-", timeTag, ".png", sep="")
      writeImage(srcImage, saveFileName, quality=100)
    }
  } else if (imageType == 2) {
    
  }
  return(seq+10)
}
