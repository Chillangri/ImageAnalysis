# ==========================================================================
# This program is developed to diagnosis skin status of human face.
#                                       Created by Ph.D./Prof. Jaeho H. BAE
#                           Dept. of Industrial Management, Osan University
#                                                                Sep., 2014.
#                                                     knowhow.bae@gmail.com
# ==========================================================================

# Color conversion rules from following site
# http://www.cs.rit.edu/~ncs/color/t_convert.html#XYZ%20to%20CIE%20L*a*b*%20(CIELAB)%20&%20CIELAB%20to%20XYZ
# http://en.wikipedia.org/wiki/Lab_color_space


# This function transfroms color values in RGB color space to another ones in CIE XYZ.
# matRGB is color values in (n x 3) matrix
myRGB2XYZ <- function(matRGB) {
  k <- c(0.412453, 0.212671, 0.019334, 0.357580, 0.715160, 0.119193, 0.180423, 0.072169, 0.950227)
  k <- matrix(k, ncol=3)  
  
  valXYZ <- k %*% t(matRGB)
  return(t(valXYZ))  
}


myXYZ2RGB <- function(matXYZ) {
  k <- c(3.240479, -0.969256, 0.055648, -1.537150, 1.875992, -0.204043, -0.498535, 0.041556, 1.057311)
  k <- matrix(k, ncol=3)
  
  valRGB <- k %*% t(matXYZ)
  return(t(valRGB))
}


myXYZ2Lab <- function(matXYZ) {
  if (is.null(nrow(matXYZ))) {
    matXYZ <- matrix(matXYZ, nrow=1)
    valLab <- matrix(0, nrow = nrow(matXYZ), ncol = ncol(matXYZ))    
  } else {
    valLab <- matrix(0, nrow = nrow(matXYZ), ncol = ncol(matXYZ))
  }
  refX <- 0.950456
  refY <- 1.000000
  refZ <- 1.088754
  refVal <- c(refX, refY, refZ)
  funcX <- matrix(0, nrow=nrow(matXYZ), ncol=1)
  funcY <- matrix(0, nrow=nrow(matXYZ), ncol=1)
  funcZ <- matrix(0, nrow=nrow(matXYZ), ncol=1)
  
  # 첨자 내부의 값들이 제대로 반영되도록... which 함수 사용 
  #   if(matXYZ[,1]/refX > (6/29)^3) {
  #     funcX <- (matXYZ[,1]/refX)^(1/3)
  #   } else {
  #     funcX <- (1/3) * (29/6)^2 * (matXYZ[,1]/refX) + (4/29)
  #   }
  #   if(matXYZ[,2]/refY > (6/29)^3) {
  #     funcY <- (matXYZ[,2]/refY)^(1/3)    
  #   } else {
  #     funcY <- (1/3) * (29/6)^2 * (matXYZ[,2]/refY) + (4/29)
  #   }
  #   if(matXYZ[,3]/refZ > (6/29)^3) {
  #     funcZ <- (matXYZ[,3]/refZ)^(1/3)
  #   } else {
  #     funcZ <- (1/3) * (29/6)^2 * (matXYZ[,3]/refZ) + (4/29)
  #   }
  
  for (i in 1:nrow(matXYZ)) {
    if (matXYZ[i,1]/refX > (6/29)^3) {
      funcX[i] <- (matXYZ[i,1]/refX)^(1/3)
    } else {
      funcX[i] <- (1/3) * (29/6)^2 * (matXYZ[i,1]/refX) + (4/29)
    }
    
    if (matXYZ[i,2]/refY > (6/29)^3) {
      funcY[i] <- (matXYZ[i,2]/refY)^(1/3)
    } else {
      funcY[i] <- (1/3) * (29/6)^2 * (matXYZ[i,2]/refY) + (4/29)
    }
    
    if (matXYZ[i,3]/refZ > (6/29)^3) {
      funcZ[i] <- (matXYZ[i,3]/refZ)^(1/3)
    } else {
      funcZ[i] <- (1/3) * (29/6)^2 * (matXYZ[i,3]/refZ) + (4/29)
    }
  }
  
  valLab[,1] <- 116 * funcY -16
  valLab[,2] <- 500 * (funcX - funcY)
  valLab[,3] <- 200 * (funcY - funcZ)
  return(valLab)
}

myLab2XYZ <- function(matLab) {
  if (is.null(nrow(matLab))) {
    matLab <- matrix(matLab, nrow=1)
    valXYZ <- matrix(0, nrow = nrow(matLab), ncol = ncol(matLab))    
  } else {
    valXYZ <- matrix(0, nrow = nrow(matLab), ncol = ncol(matLab))
  }
  refX <- 0.950456  
  refY <- 1.000000
  refZ <- 1.088754
  refVal <- c(refX, refY, refZ)
  revX <- matrix(0, nrow=nrow(matLab), ncol=1)
  revY <- matrix(0, nrow=nrow(matLab), ncol=1)
  revZ <- matrix(0, nrow=nrow(matLab), ncol=1)
  

  for (i in 1:nrow(matLab)) {
    t_x <- (1/116) * (matLab[i,1]+16) + (1/500) * matLab[i,2]
    t_y <- (1/116) * (matLab[i,1]+16)
    t_z <- (1/116) * (matLab[i,1]+16) - (1/200) * matLab[i,3]
    
    if (t_x > (6/29)) {
      revX[i] <- t_x^3
    } else {
      revX[i] <- 3 * (6/29)^2 * (t_x - (4/29))
    }
    
    if (t_y > (6/29)) {
      revY[i] <- t_y^3
    } else {
      revY[i] <- 3 * (6/29)^2 * (t_y - (4/29))
    }
    
    if (t_z > (6/29)) {
      revZ[i] <- t_z^3
    } else {
      revZ[i] <- 3 * (6/29)^2 * (t_z - (4/29))
    }
  }
  
  valXYZ[,1] <- refX * revX
  valXYZ[,2] <- refY * revY
  valXYZ[,3] <- refZ * revZ
  return(valXYZ)
}

