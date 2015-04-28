remapColorLab   <- function(rawLab, targetL) {
  modiLab       <- rawLab
  modiLab[,,1] <- rawLab[,,1] - 50
  targetL       <- targetL     - 50
  oriRadiusL    <- 50
  oriRadiusa    <- 127
  oriRadiusb    <- 127
  oriRadius     <- c(oriRadiusL, oriRadiusa, oriRadiusb)
  # original ellipsoid is
  # L^2/oriRadiusL^2 + a^2/oriRadiusa^2 + b^2/oriRadiusb^2 = 1
  
  targetVal     <- array(0, dim=c(nrow(rawLab), ncol(rawLab), 3))
  targetVal[,,1]<- targetL
  
  ratioLa       <- sqrt(modiLab[,,1]^2/oriRadiusL^2 + modiLab[,,2]^2/oriRadiusa^2)
  ratioLab      <- sqrt(modiLab[,,1]^2/oriRadiusL^2 + modiLab[,,2]^2/oriRadiusa^2 + modiLab[,,3]^2/oriRadiusb^2)
  
  radiusLab     <- sqrt(oriRadiusL^2 + oriRadiusa^2 + oriRadiusb^2)    #r_o
  radiusLa      <- sqrt(oriRadiusL^2 + oriRadiusa^2)                   #r_o'
  
#  targetVal[which(targetVal[,,1] > (modiLab[,,1]*ratioLa))] = (modiLab[,,1]*ratioLa)
  
  for (i in 1:nrow(targetVal[,,1]))   {
    for (j in 1:ncol(targetVal[,,1])) {
      if (targetVal[i, j, 1] > (modiLab[i,j,1]*ratioLa[i,j])) {
        targetVal[i,j,1] = modiLab[i,j,1]*ratioLa[i,j]
      }
    }
  }
  
  targetVal[,,2]<- sqrt((1-(targetVal[,,1]^2/(oriRadiusL * ratioLa)^2))*(oriRadiusa*ratioLa)^2)
  targetVal[,,3]<- sqrt((1-(targetVal[,,1]^2/(oriRadiusL * ratioLab)^2) - (targetVal[,,2]^2/(oriRadiusa * ratioLab)^2)) * (oriRadiusb * ratioLab)^2)
  targetVal[,,1]<- targetVal[,,1]+50
  
return(targetVal)
}


#   #ratioLab      <- modiLab[,,1]^2/oriRadiusL^2 + modiLab[,,2]^2/oriRadiusa^2 + modiLab[,,3]^2/oriRadiusb^2
#   #ratioLa       <- modiLab[,,1]^2/oriRadiusL^2 + modiLab[,,2]^2/oriRadiusa^2
#   
# 
#   
#   # -----------------------
#   rawLab <- c(82., 8., 4.)
#   maxLab <- c(100, 127, 127)
#   modiL  <- 83
#   moveLab <- c(maxLab[1]-50, maxLab[2:3])
#   testLab <- c(rawLab[1]-50, rawLab[2:3])
#   modiL  <- modiL - 50
#   
#   ratio_La<- testLab[1]^2/moveLab[1]^2 + testLab[2]^2/moveLab[2]^2
#   ratio_Lab<- testLab[1]^2/moveLab[1]^2 + testLab[2]^2/moveLab[2]^2 + testLab[3]^2/moveLab[3]^2
#   
#   if (modiL > (moveLab[1] * sqrt(ratio_La))) {modiL = (moveLab[1] * sqrt(ratio_La))} else (modiL = modiL)
#   
#   # if the result of test1 is 1, this test was passed
#   test1 <- (testLab[1]^2 / (moveLab[1] * sqrt(ratio_La))^2) + (testLab[2]^2 / (moveLab[2]*sqrt(ratio_La))^2)
#   test2 <- (testLab[1]^2 / (moveLab[1] * sqrt(ratio_Lab))^2) + (testLab[2]^2 / (moveLab[2]*sqrt(ratio_Lab))^2) + (testLab[3]^2 / (moveLab[3]*sqrt(ratio_Lab))^2)
#   
#   
#   # if the value of L will be changed into modiL, we can fine a value of a in the ellipse plane
#   modia <- sqrt((1-(modiL^2/(moveLab[1] * sqrt(ratio_La))^2))*(moveLab[2]*sqrt(ratio_La))^2)
#   
#   
#   ratio_Lb<- testLab[1]^2/moveLab[1]^2 + testLab[3]^2/moveLab[3]^2
#   
#   # if the result of test1 is 1, this test was passed
#   test2 <- (testLab[1]^2 / (moveLab[1] * sqrt(ratio_Lb))^2) + (testLab[3]^2 / (moveLab[3]*sqrt(ratio_Lb))^2)
#   
#   # if the value of L will be changed into modiL, we can fine a value of a in the ellipse plane
#   modib <- sqrt((1-(modiL^2/(moveLab[1] * sqrt(ratio_Lb))^2))*(moveLab[3]*sqrt(ratio_Lb))^2)
#   
#   
#   rawXYZ <- myLab2XYZ(rawLab)
#   rawRGB <- myXYZ2RGB(rawXYZ)
#   rawRGB255 <- rawRGB*255
#   
#   rstLab <- c(modiL+50, modia, modib)
#   rstXYZ<- myLab2XYZ(rstLab)
#   rstRGB <- myXYZ2RGB(rstXYZ)
#   rstRGB255 <- rstRGB*255
#   
#   rawRGB255-rstRGB255
#   
#   # Verify the result
#   rawImage <- array(0, c(200,200,3))
#   rstImage <- array(0, c(200,200,3))
#   rawImage[,,1] <- matrix(rawRGB[1], nc=200, nr=200)
#   rawImage[,,2] <- matrix(rawRGB[2], nc=200, nr=200)
#   rawImage[,,3] <- matrix(rawRGB[3], nc=200, nr=200)
#   rawImage <- as.Image(rawImage)
#   drawRawImage <- rgbImage(red=rawImage[,,1], green=rawImage[,,2], blue=rawImage[,,3])
#   display(drawRawImage, method="raster")
#   
#   rstImage[,,1] <- matrix(rstRGB[1], nc=200, nr=200)
#   rstImage[,,2] <- matrix(rstRGB[2], nc=200, nr=200)
#   rstImage[,,3] <- matrix(rstRGB[3], nc=200, nr=200)
#   rstImage <- as.Image(rstImage)
#   drawRstImage <- rgbImage(red=rstImage[,,1], green=rstImage[,,2], blue=rstImage[,,3])
#   display(drawRstImage, method="raster")
# }