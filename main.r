# ==========================================================================
# This program is developed to diagnosis skin status of human face.
#                                       Created by Ph.D./Prof. Jaeho H. BAE
#                           Dept. of Industrial Management, Osan University
#                                                                Sep., 2014.
#                                                     knowhow.bae@gmail.com
# ==========================================================================


main <- function(..., area="T", save="N", halt=TRUE) {
  source("handlerFiles.r")
  timeTag         <- format(Sys.time(), "%Y%m%d-%H%M%S")
  seq             <- 110
  
  lib.call()
  
  if(checkDirectory() == FALSE) {on.exit()}

  # Step 01. Select related files
  currentDir      <- getFileName(type=1)
  prefix          <- getFileName(type=4)
  imgFile         <- getFileName(type=2, fileID = prefix)
  locFile         <- getFileName(type=3)
  location        <- transLoc(locFile)
  
  # Step 02. Read an orignal image
  rawImage        <- readImage(imgFile)
  oriImage        <- rawImage[,,1:3]
  oriImage        <- as.Image(oriImage)
  
  check.halt("original loaded Image.", halt)
  showImage(oriImage, 1)
  
  # Step 03. Edge detection and display it
  edgeImage       <- edgeDetectFTT(oriImage)
  check.halt("edge-detected Image.", halt)
  showImage(edgeImage, 1)
  seq             <- saveImage(edgeImage, prefix, seq, timeTag, save, 1)
  
  # Step 04. Check detected points on an original file and display it
  chkdImage       <- checkImage(oriImage, location)
  check.halt("cheked Image.", halt)
  showImage(chkdImage, 1)
  seq             <- saveImage(chkdImage, prefix, seq, timeTag, save, 1)  
  
  # Step 05. Make an image with selected area  
  targetImage     <- selectImage(oriImage, area, location)
  check.halt("Selected Image.", halt)
  showImage(targetImage, 1)
  
  # Step 06. Seperate an image into a RGB colorspace
  tI_RGB          <- channel(targetImage, 'rgb')
  tI_R            <- channel(targetImage, 'asred')
  seq             <- saveImage(tI_R, prefix, seq, timeTag, save, 1)  
  tI_G            <- channel(targetImage, 'asgreen')
  seq             <- saveImage(tI_G, prefix, seq, timeTag, save, 1)  
  tI_B            <- channel(targetImage, 'asblue')
  seq             <- saveImage(tI_B, prefix, seq, timeTag, save, 1)  
  
  tI_ALL          <- combineImage(tI_RGB)
  check.halt("Combined Image.", halt)
  showImage(tI_ALL, 1)
  seq             <- saveImage(tI_ALL, prefix, seq, timeTag, save, 1)  

  
  # Step 07. Show a histogram result of selected tartget Image
  check.halt("histogram of selected image.", halt)
  hist(targetImage)
  
  # Step 08. Examine extra ordinary values of target image in RGB color value
  tI_EO_R         <- targetImage[,,1] > (mean(targetImage[,,1]) + sd(targetImage[,,1]))
  check.halt("image with extra-high R-value.", halt)
  showImage(tI_EO_R, 1)
  tI_EO_R_filled  <- rgbImage(red=tI_EO_R)
  check.halt("image with extra-high R-value in red color.", halt)
  showImage(tI_EO_R_filled, 1)
  seq             <- saveImage(tI_EO_R_filled, prefix, seq, timeTag, save, 1)    

  tI_EO_G         <- targetImage[,,2] > (mean(targetImage[,,2]) + sd(targetImage[,,2]))
  check.halt("image with extra-high G-value.", halt)
  showImage(tI_EO_G, 1)
  tI_EO_G_filled  <- rgbImage(green=tI_EO_G)
  check.halt("image with extra-high G-value in green color.", halt)
  showImage(tI_EO_G_filled, 1)
  seq             <- saveImage(tI_EO_G_filled, prefix, seq, timeTag, save, 1)    

  tI_EO_B         <- targetImage[,,3] > (mean(targetImage[,,3]) + sd(targetImage[,,3]))
  check.halt("image with extra-high B-value.", halt)
  showImage(tI_EO_B, 1)
  tI_EO_B_filled  <- rgbImage(blue=tI_EO_B)
  check.halt("image with extra-high B-value in blue color.", halt)
  showImage(tI_EO_B_filled, 1)
  seq             <- saveImage(tI_EO_B_filled, prefix, seq, timeTag, save, 1)    
  
  tI_EO_RGB_filled <- rgbImage(red=tI_EO_R, green=tI_EO_G, blue=tI_EO_B)
  check.halt("image with extra-high RGB-value filled.", halt)
  showImage(tI_EO_RGB_filled, 1)
  seq             <- saveImage(tI_EO_RGB_filled, prefix, seq, timeTag, save, 1)    
  
  # Step 09. Colorspace taransformation and save the result
  # RGB -> CIE XYZ -> CIE L*a*b*
  # Verify each converted values with following site.
  # http://davengrace.com/cgi-bin/cspace.pl  
  tI_XYZ          <- transColorSpace(tI_RGB, transMode = "CIEXYZ")
  tI_Lab          <- transColorSpace(tI_XYZ, transMode = "CIELAB")

  # Step 10. Examine extra ordinary values of target image and apply filters
  tI_Masked       <- applyFilter(srcImage = targetImage, redMask = tI_EO_R, greenMask = tI_EO_G, blueMask = tI_EO_B)
  check.halt("image after diagnosys of melanin (in black) and erythema (in red).", halt)
  showImage(tI_Masked, 1)
  seq             <- saveImage(tI_Masked, prefix, seq, timeTag, save, 1)    

  # Step 11. Examine Derma Vision's algorithm
  tI_DV_MI        <- transDVImage(srcImage = targetImage, examType = "M")
  tI_DV_EI        <- transDVImage(srcImage = targetImage, examType = "E")
  check.halt("erythema diagnosis image by Dermavision's algorithm.", halt)
  showImage(tI_DV_EI, 2, colorPallet = 1, findPoint = "max")
  seq             <- saveImage(tI_DV_EI, prefix, seq, timeTag, save, 1)    
  check.halt("melanin diagnosis image by Dermavision's algorithm.", halt)
  showImage(tI_DV_MI, 2, colorPallet = 1, findPoint = "max")
  seq             <- saveImage(tI_DV_MI, prefix, seq, timeTag, save, 1)    

  # Step 12. Examine CIE-L*a*b* images
  tI_nor_L        <- normalize(tI_Lab[,,1])
  check.halt("result image in L* value of CIE-L*a*b*.", halt)
  showImage(tI_nor_L, 2, colorPallet = 5, findPoint = "min")
  seq             <- saveImage(tI_nor_L, prefix, seq, timeTag, save, 1)    
  
  tI_nor_a        <- normalize(tI_Lab[,,2])
  check.halt("result image in a* value of CIE-L*a*b*.", halt)
  showImage(tI_nor_a, 2, colorPallet = 1, findPoint = "max")
  seq             <- saveImage(tI_nor_a, prefix, seq, timeTag, save, 1)    
  
  tI_nor_b        <- normalize(tI_Lab[,,3])
  check.halt("result image in b* value of CIE-L*a*b*.", halt)
  showImage(tI_nor_b, 2, colorPallet = 4, findPoint = "min")
  seq             <- saveImage(tI_nor_b, prefix, seq, timeTag, save, 1)    

  # Step 13. Remap Colorspace considering external light source
  targetL         <- mean(tI_Lab[,,1])
  reMap_tI_Lab    <- remapColorLab(tI_Lab, targetL)
  
  reMap_tI_nor_L  <- reMap_tI_Lab[,,1]
  check.halt("result image in remapped L* value of CIE-L*a*b*.", halt)
  showImage(reMap_tI_nor_L, 2, colorPallet = 5, findPoint = "min")
  seq             <- saveImage(reMap_tI_nor_L, prefix, seq, timeTag, save, 1)    
  
  reMap_tI_nor_a  <- reMap_tI_Lab[,,2]
  check.halt("result image in remapped a* value of CIE-L*a*b*.", halt)
  showImage(reMap_tI_nor_a, 2, colorPallet = 1, findPoint = "max")
  seq             <- saveImage(reMap_tI_nor_a, prefix, seq, timeTag, save, 1)    
  
  reMap_tI_nor_b        <- reMap_tI_Lab[,,3]
  check.halt("result image in remapped b* value of CIE-L*a*b*.", halt)
  showImage(reMap_tI_nor_b, 2, colorPallet = 4, findPoint = "min")
  seq             <- saveImage(reMap_tI_nor_b, prefix, seq, timeTag, save, 1)  
  
  # Step 14. Transform to RGB Color Space and display it
  reMap_tI_XYZ    <- transColor2RGB(reMap_tI_Lab, transMode = "CIEXYZ")
  reMap_tI_RGB    <- transColor2RGB(reMap_tI_XYZ, transMode = "CIELab")
  reMap_tI_RGB    <- as.Image(reMap_tI_RGB)
  
  re_Map_tI_DV_MI        <- transDVImage(srcImage = reMap_tI_RGB, examType = "M")
  re_Map_tI_DV_EI        <- transDVImage(srcImage = reMap_tI_RGB, examType = "E")
  check.halt("erythema diagnosis image by remapped Dermavision's algorithm.", halt)
  showImage(re_Map_tI_DV_EI, 2, colorPallet = 1, findPoint = "max")
  seq             <- saveImage(re_Map_tI_DV_EI, prefix, seq, timeTag, save, 1)    
  check.halt("melanin diagnosis image by remapped Dermavision's algorithm.", halt)
  showImage(re_Map_tI_DV_MI, 2, colorPallet = 1, findPoint = "max")
  seq             <- saveImage(re_Map_tI_DV_MI, prefix, seq, timeTag, save, 1)  
  
  reMap_tI_RGB    <- channel(targetImage, 'rgb')
  showImage(reMap_tI_RGB, 1)
  
  
  if (save=="Y") {
    xlsx_name       <- paste("ColorSpace", timeTag, ".xlsx", sep="")
    nameFile        <- paste("./srcImage/", prefix, "/", xlsx_name, sep="")
    dataWB          <- loadWorkbook(nameFile, create = TRUE)
      saveXlsx(dataWB, "RGB-R", tI_RGB[,,1])
      saveXlsx(dataWB, "RGB-G", tI_RGB[,,2])
      saveXlsx(dataWB, "RGB-B", tI_RGB[,,3])
      saveXlsx(dataWB, "CIEXYZ-X", tI_XYZ[,,1])
      saveXlsx(dataWB, "CIEXYZ-Y", tI_XYZ[,,2])
      saveXlsx(dataWB, "CIEXYZ-Z", tI_XYZ[,,3])
      saveXlsx(dataWB, "CIELab-L", tI_Lab[,,1])
      saveXlsx(dataWB, "CIELab-a", tI_Lab[,,2])
      saveXlsx(dataWB, "CIELab-b", tI_Lab[,,3])
    saveWorkbook(dataWB)
    
    
    pdf_name        <- paste("Display Image", timeTag, ".pdf", sep="")
    pdf_File        <- paste("./srcImage/", prefix, "/", pdf_name, sep="")
    pdf(pdf_File, paper="a4r", width = 9.7, bg="transparent")

    par(mfrow=c(2,2))
      showImage(targetImage, 1, imgTitle="Target Image")
      showImage(tI_ALL, 1, imgTitle="Target Image decomposited by RGB")
      hist(targetImage)
      showImage(tI_EO_R_filled, 1, imgTitle="Extra ordinary in R value")
      showImage(tI_EO_G_filled, 1, imgTitle="Extra ordinary in G value")
      showImage(tI_EO_B_filled, 1, imgTitle="Extra ordinary in B value")
      showImage(tI_EO_RGB_filled, 1, imgTitle="Extra ordinary in RGB value")
      showImage(tI_Masked, 1, imgTitle="Test result in my algorithm")
      showImage(tI_DV_EI, 2, colorPallet = 1, findPoint = "max", imgTitle="DermaVision's Erythma Image")
      showImage(tI_DV_MI, 2, colorPallet = 1, findPoint = "max", imgTitle="DermaVision's Melanin Image")
      showImage(tI_nor_L, 2, colorPallet = 5, findPoint = "min", imgTitle="L* Image in CIE-L*a*b*")
      showImage(tI_nor_a, 2, colorPallet = 1, findPoint = "max", imgTitle="a* Image in CIE-L*a*b*")
      showImage(tI_nor_b, 2, colorPallet = 4, findPoint = "min", imgTitle="b* Image in CIE-L*a*b*")
      showImage(reMap_tI_nor_L, 2, colorPallet = 5, findPoint = "min", imgTitle="Remapped L* Image in CIE-L*a*b*")
      showImage(reMap_tI_nor_a, 2, colorPallet = 1, findPoint = "max", imgTitle="Remapped a* Image in CIE-L*a*b*")
      showImage(reMap_tI_nor_b, 2, colorPallet = 4, findPoint = "min", imgTitle="Remapped b* Image in CIE-L*a*b*")
      showImage(re_Map_tI_DV_EI, 2, colorPallet = 1, findPoint = "max", imgTitle="Modified Dermavision's Erythma Image")
      showImage(re_Map_tI_DV_MI, 2, colorPallet = 1, findPoint = "max", imgTitle="Modified Dermavision's Melanin Image")
      showImage(reMap_tI_RGB, 1, imgTitle="Modified Original Image")
      par(new=F)
    dev.off()
  }

}