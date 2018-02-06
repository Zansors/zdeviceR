#' find_peaks
#'
#' This function finds peaks in a time series of data, defined to be a local maxima where
#' at least `m` points on either side are smaller than the deemed peak
#'
#' This function is written by Stasia Grinberg and is available at https://github.com/stas-g/findPeaks
#'
#' @param x A numerical vector
#' @param m The window that defines the local maximum (default = 3)
#'
#' @return A vector of indices where the peaks have been identified
#' @export
#'
#' @examples
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}