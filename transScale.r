# ==========================================================================
# This program is developed to diagnosis skin status of human face.
#                                       Created by Ph.D./Prof. Jaeho H. BAE
#                           Dept. of Industrial Management, Osan University
#                                                                Sep., 2014.
#                                                     knowhow.bae@gmail.com
# ==========================================================================


axis.normalize <- function(axis.current,..., minVal=NULL, maxVal=NULL, minTar=0, maxTar=1) {
  if (is.null(nrow(axis.current))) {
    axis.current <- matrix(axis.current)
  } else {
    axis.current <- matrix(axis.current, ncol=ncol(axis.current), nrow=nrow(axis.current))
  }
  
  if (is.null(minVal)) {
    min.value  <- min(axis.current)  
  } else {min.value <- minVal}
  
  if (is.null(maxVal)) {
    max.value  <- max(axis.current)
  } else {max.value <- maxVal}
  
  norm.value <- normalize(axis.current, ft=c(minTar, maxTar), inputRange=c(min.value, max.value))
  return.val <- list(min = min.value, max = max.value, data = norm.value)
  return(return.val)
}


axis.denormalize <- function(data, min, max) {
  return.val <- data * (max-min) + min
  return(return.val)
}

