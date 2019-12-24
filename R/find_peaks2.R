#' Detect peaks in data based on their amplitude and other features
#'
#' Translated from the Python version at https://github.com/demotu/BMC
#' License: MIT
#' Version: 1.0
#'
#' @param x 1-d array
#' @param mph (NA, number), optional (default = NA). Detect peaks wthat are greater than minimum peak height (if `valley=F`) or peaks that are smaller than maximum peak height (if `valley=T`)
#' @param mpd positive integer, optional (default = 1). Detect peaks that are at least separated by minimum peak distance (in number of data)
#' @param threshold positive number, optional (default = 0). Detect peaks (valleys) that are greater (smaller) than  `threshold` in relation to their immediate neighbors
#' @param edge One of NA, `rising`, `falling` or `both`, optional (default = `rising`). for a flat peak, keep only the rising edge ('rising'), only the falling edge ('falling'), both edges ('both'), or don't detect a flat peak (None).
#' @param kpsh Boolean, optional (default = FALSE). Keep peaks with same height even if they are closer than `mpd`
#' @param valley Boolean, optional (default = FALSE). If TRUE, detect valleys (local minima) instead of peaks (local maxima)
#'
#' @return a 1-d array with indices of the peaks in `x`
#' @export
#'
#' @examples
#'
#'  from detect_peaks import detect_peaks
#'  x = rnorm(100)
#'  x[60:81] = NA
#'  # detect all peaks and plot data
#'  ind = detect_peaks(x, show=TRUE)
#'  print(ind)
#'
#'  x = sin(2*pi*5*seq(0,1,length.out=200)) + rnorm(200)/5
#'  # set minimum peak height = 0 and minimum peak distance = 20
#'  detect_peaks(x, mph=0, mpd=20, show=TRUE)
#'
#'  x = c(0, 1, 0, 2, 0, 3, 0, 2, 0, 1, 0)
#'  # set minimum peak distance = 2
#'  detect_peaks(x, mpd=2, show=TRUE)
#'
#'  x = sin(2*pi*5*seq(0, 1, length.out=200)) + rnorm(200)/5
#'  # detection of valleys instead of peaks
#'  detect_peaks(x, mph=-1.2, mpd=20, valley=True, show=True)
#'
#'  x =c(0, 1, 1, 0, 1, 1, 0)
#'  # detect both edges
#'  detect_peaks(x, edge='both', show=TRUE)
#'
#'  x =c(-2, 1, -2, 2, 1, 1, 3, 0)
#'  # set threshold = 2
#'  detect_peaks(x, threshold = 2, show=TRUE)
detect_peaks <- function(x, mph = NA, mpd = 1, threshold=0, edge = 'rising',
                        kpsh = FALSE, valley = FALSE) {
  x = as.vector(x)
  if (length(x) < 3){
    return(integer(0))
  }
  if(valley){
    x = -x
    if(!is.na(mph)){
      mph = -mph
    }
  }
  dx = x[2:length(x)] - x[1:(length(x)-1)]
  indna <- which(is.na(x))
  if(length(indna)>0){
    x[indna] <- Inf
    dx[which(is.na(dx))] <- Inf
  }
  ine <- numeric()
  ire <- numeric()
  ife <- numeric()
  if(is.na(edge)){
    ine = which((c(dx, 0) < 0) & (c(0, dx) > 0))
  } else {
    if(tolower(edge) %in% c('rising','both')){
      ire = which((c(dx, 0) <= 0) & (c(0, dx)>0))
    }
    if(tolower(edge) %in% c('falling','both')){
      ife = which((c(dx, 0) < 0) & (c(0, dx) >= 0))
    }
  }
  ind = sort(unique(c(ine, ire, ife)))
  if (length(ind) > 0 & length(indna) > 0){
   ind <- setdiff(ind, unique(c(indna, indna-1, indna+1)))
  }
  if (length(ind) > 0 & ind[1] == 1){
    ind = ind[2:length(ind)]
  }
  if(length(ind) > 0 & ind[length(ind)] == length(x)){
    ind = ind[1:(length(ind)-1)]
  }
  if (length(ind) > 0 & !is.na(mph)){
    ind = ind[x[ind] >= mph]
  }
  if (length(ind) > 0 & threshold > 0){
    dx = min(c(x[ind] - x[ind-1], x[ind]- x[ind+1]))
    ind = ind[dx >= threshold]
  }
  if (length(ind) > 0 & mpd > 1){
    ind <- ind[rev(order(x[ind]))]
    idel <- rep(0, length(ind))
    for (i in 1:length(ind)){
      if(!idel[i]){
        idel <- idel | ((ind >= ind[i] - mpd) & (ind <= ind[i] + mpd) &
          ifelse(kpsh, x[ind[i]] > x[ind], TRUE))
        idel[i] <- 0
      }
    }
    ind <- sort(ind[!idel])
  }
  return(ind)
}