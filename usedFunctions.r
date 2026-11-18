transDVImage  <- function(srcImage, examType) {
  if (examType == "M") {
    DVImage   <- log10(1/srcImage[,,1])
  } else if (examType == "E") {
    DVImage <- 10*(log10(1/srcImage[,,2])-1.44*log10(1/srcImage[,,1]))    
  }
  DVImage     <- as.Image(DVImage)
  return(DVImage)
}

applyFilter   <- function(srcImage, redMask, greenMask, blueMask) {
  temp_mask <- thresh(srcImage, 10, 10, 0.03)
  temp_mask <- opening(temp_mask, makeBrush(3, shape="disc"))
  temp_mask <- fillHull(bwlabel(temp_mask))
  
  x1_mask_r <- opening(redMask, makeBrush(3, shape="disc"))
  x1_mask_g <- opening(greenMask, makeBrush(3, shape="disc"))
  x1_mask_b <- opening(blueMask, makeBrush(3, shape="disc"))
  
  x1_mask   <- array(0, c(length(srcImage[,1,1]), length(srcImage[1,,1]),3))
  x1_mask[,,1] <- x1_mask_r
  x1_mask[,,2] <- x1_mask_g
  x1_mask[,,3] <- x1_mask_b
  x1_mask   <- as.Image(x1_mask)
  
  lst_mask  <- propagate(srcImage, temp_mask, x1_mask)
  lst_mask_bw <- channel(lst_mask, 'gray')
  temp_mask_bw <- channel(temp_mask, 'gray')
  #rslt_x1   <- fillHull(bwlabel(lst_mask_bw))
  #temp_mask_bw <- fillHull(bwlabel(temp_mask_bw))
  
  rslt_x1   <- paintObjects(lst_mask_bw, srcImage, opac=c(1,1), col=c('#ff0000', NA))
  rslt_x1   <- paintObjects(temp_mask_bw, rslt_x1, opac=c(1,1), col=c('#000000', NA))
  
  return(rslt_x1)
}

combineImage  <- function(srcImage) {
  combinedImage <- EBImage::combine(rgbImage(red=srcImage[,,1]), rgbImage(green=srcImage[,,2]), 
                                    rgbImage(blue=srcImage[,,3]), srcImage)
  return(combinedImage)
}

selectImage   <- function(oriImage, area, locData) {
  if (area == "L") {
    startX <- locData[9,1]
    startY <- locData[14,2]
    endX   <- locData[23,1]
    endY   <- locData[23,2]
  } else if (area == "R") {
    startX <- locData[20,1]
    startY <- locData[16,2]
    endX   <- locData[10,1]
    endY   <- locData[20,2]
  } else {
    startX <- min(locData[1:18,1])
    startY <- max(locData[1:18,2]) + 15
    endX <- max(locData[1:18,1])
    endY <- min(locData[19:length(locData[,1]),2])
  }
  
  x1 <- oriImage[startX:endX, startY:endY,]
  return(x1)
}


showImage  <- function(displayImage, displayType,..., colorPallet=1, findPoint="max", imgTitle="") {
  if (displayType == 1) {
    display(displayImage, method = "raster", all=TRUE, title=imgTitle)
  } else if (displayType ==2) {
    if (colorPallet == 1) {
      pallet=colorRampPalette(c("blue","green", "yellow", "red"), space="rgb")
    } else if (colorPallet == 2) {
      pallet=colorRampPalette(c("black", "white"), space="rgb")
    } else if (colorPallet == 3) {
      pallet=colorRampPalette(c("green", "red"), space="rgb")
    } else if (colorPallet == 4) {
      pallet=colorRampPalette(c("blue", "yellow"), space="rgb")
    } else if (colorPallet == 5) {
      pallet=colorRampPalette(c("red","yellow", "green", "blue"), space="rgb")
    }
    
    breaks <- seq(min(displayImage), max(displayImage),length.out=100)
    par(mar=c(3,3,3,3))
    displayImage  <- displayImage[,ncol(displayImage):1]
    displayImage_vw  <- graphics::image(seq(dim(displayImage)[1]), seq(dim(displayImage)[2]), displayImage, 
                                col=pallet(length(breaks)-1), breaks=breaks, xaxt="n", yaxt="n", ylab="", xlab=imgTitle)
    #Add additional graphics
    if (findPoint == "max") {
      markPoint  <- which.max(displayImage)
      displayColor <- "blue"
    } else if (findPoint == "min") {
      markPoint  <- which.min(displayImage)
      displayColor <- "red"
    }
    
    points(markPoint %% dim(displayImage)[1], markPoint %/% dim(displayImage)[1], 
                                pch=2, lwd=2, cex=2,col=displayColor)
  }
}

edgeDetectFTT <- function(srcImage) {
  filter = matrix(1, nc=3, nr=3)
  filter[2, 2] = -8
  imgDataEdge <- filter2(srcImage, filter)
  return(imgDataEdge)  
}

checkImage    <- function(srcImage, locData) {
  for (i in 1:length(locData[,1])) {
    srcImage <- drawCircle(srcImage, locData[i,1], locData[i,2], 5, col="red", fill=TRUE)
  }
  return(srcImage)
}


check.halt <- function(msgID, type) {
  if (type==TRUE) {
    msgDisplay <- paste("Press any key to show a/an ", msgID, sep="")
    keyValue <- readline(msgDisplay)
  }
  return(TRUE)
}

image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}




transColor2RGB  <- function(valColor, transMode){
    dimOfvalue <- dim(valColor)
  numPages   <- dimOfvalue[3]
  numCols    <- length(valColor[1,,1])
  numRows    <- length(valColor[,1,1])
  rtnColor   <- array(0, c(numRows, numCols, numPages))
  
  if (transMode == "CIEXYZ") {
    for (c in 1:numCols) {
      rtnColor[, c, ]  <- myLab2XYZ(cbind(valColor[, c, 1], valColor[, c, 2], valColor[, c, 3]))
    }
  } else if (transMode == "RGB") {
    for (c in 1:numCols) {
      rtnColor[, c, ]  <- myXYZ2RGB(cbind(valColor[, c, 1], valColor[, c, 2], valColor[, c, 3])/100)
    }
  } else {
    rtnColor <- valColor
  }
  return(rtnColor)
}


transColorSpace <- function(valRGB, transMode) {
      dimOfvalue <- dim(valRGB)
  numPages   <- dimOfvalue[3]
  numCols    <- length(valRGB[1,,1])
  numRows    <- length(valRGB[,1,1])
  rtnColor   <- array(0, dim=c(numRows, numCols, numPages))
  
  if (transMode == "CIEXYZ") {
    for (c in 1:numCols) {
        rtnColor[, c, ]  <- myRGB2XYZ(cbind(valRGB[,c,1], valRGB[,c,2], valRGB[,c,3]))
    }
  } else if (transMode == "CIELAB") {
    for (c in 1:numCols) {
        rtnColor[, c, ]  <- myXYZ2Lab(cbind(valRGB[,c,1], valRGB[,c,2], valRGB[,c,3]))
    }    
  } else {
    rtnColor <- valRGB
  }
  return(rtnColor)
}
