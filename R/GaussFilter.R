#' Gaussian Filtering of signal
#'
#' This function takes a signal, calculates the (robust) Z-transform,
#' takes its absolute value, subtracts the threshold and returns the positive
#' part of the transformed signal
#'
#' @param x A numeric vector comprising the signal
#' @param threshold The cutoff above which the absolute transformed signal will be considered true signal
#' @param robust A logical. Should robust scaling be used (default = FALSE)
#'
#' @return
#'
#' @examples
GaussFilter <- function(x, threshold, robust = FALSE) {
  if(robust){
    m <- median(x, na.rm=T)
    s <- mad(x, na.rm=T)
  } else {
    m <- mean(x, na.rm=T)
    s <- sd(x, na.rm=T)
  }
  z <- (x - m)/s
  out <- abs(z)
  out <- pmax(out - threshold, 0)
  return(out)
}