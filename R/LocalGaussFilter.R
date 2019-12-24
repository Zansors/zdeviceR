#' LocalGaussFilter
#'
#' This function takes a signal and applies moving means and SDs to it of a particular
#' window (`n`). It then scales the data by subtracting from each point the average of the
#' previous values in the window and dividing by their SD. The absolute value is then taken and
#' a threshold (`threshold`) is applied to determine the signal.
#'
#' A robust option using rolling median and MAD is yet to be implemented
#'
#' @param x A vector of numerical values
#' @param n The width of the window to perform rolling calculations on
#' @param threshold The threshold beyond which the normalized data is considered signal
#' @param robust (not implemented)
#'
#' @return A new numerical vector of thresholded values
#' @import zoo
#' @export
#'
#' @examples
#' x <- rnorm(1000000)
#' y <- LocalGaussFilter(x, n = 100, threshold = 3)
#'
LocalGaussFilter <- function(x, n, threshold, robust = FALSE){
    m <- rollmeanr(x, n, na.rm = T, na.pad = T)
    m2 <- rollmeanr(x^2, n, na.rm = T, na.pad = T)
    s <- sqrt(m2 - m^2)
    z <- (x - m)/s
    out <- abs(z)
    out <- pmax(out - threshold, 0)
    return(out)
}