library("lattice")
library("XLConnect")
red      <- c(1:255)
green    <- c(1:255)

imgRed   <- array(0, dim=c(length(red),3))
imgRed[,1] <- red
imgGreen <- array(0, dim=c(length(red)*length(green), 4))
imgGreen[,1] <- rep(red, each=length(green))
imgGreen[,2] <- rep(green, times=length(red))


refRed   <- 255
refGreen <- 180
refBlue  <- 165

valRed1   <- imgRed[,1] + 150
#calResult1 <- 100 * log10(valRed1/refRed)
calResult1 <- 90+ 100 * log10(refRed/valRed1)
imgRed[,2] <- calResult1
calResult12 <- 80 - ((valRed1 - refRed)/(255 - refRed) - (green - refGreen) / (255-refGreen)) * 20
imgRed[,3] <- calResult12
plot(imgRed[,1], imgRed[,2], xlab = "Red Value", ylab = "Erythema Index")

plot(imgRed[,1], imgRed[,3], xlab = "Red Value", ylab = "Erythema Index")



valRed2 <- imgGreen[,1] + 100
valGreen2 <- imgGreen[,2] + 60
calResult2 <- 90 + 50*(log10(refGreen/valGreen2) + 1.44 * log10(refRed/valRed2))
imgGreen[,3] <- calResult2

calResult21 <- 80 - ((valRed2 - refRed)/(255-refRed) - (valGreen2-refGreen)/(255-refGreen)) * 10

imgGreen[,4] <- calResult21

wireframe(imgGreen[,3]~imgGreen[,1]*imgGreen[,2],
          xlab = list(label = "Red Value", font =1),
          ylab = list(label = "Green Value", font = 1),
          zlab = list(label = "Melanin Index", font =1))
          zlim = range(seq(14, 16),by=0.5), drape = TRUE, colorkey = TRUE)

wireframe(imgGreen[,4]~imgGreen[,1]*imgGreen[,2],
          xlab = list(label = "Red Value", font =1),
          ylab = list(label = "Green Value", font = 1),
          zlab = list(label = "Melanin Index", font =1),
          zlim = range(seq(14, 16),by=0.5), drape = TRUE, colorkey = TRUE)

fileName    <- paste(getwd(), "/result.xlsx", sep="")
resultWB <- loadWorkbook(fileName, create = TRUE)
resultWS <- "MelaninIndex"
createSheet(resultWB, resultWS)
writeWorksheet(resultWB, imgGreen, resultWS, header=TRUE)
saveWorkbook(resultWB)  