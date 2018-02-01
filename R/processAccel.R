# Processing motion data
#
# @param accel_dat A character vector obtained in readData
# @param rate Sampling rate for accel data in Hz
#
# @return A data frame containing times and corresponding recorded signal
#
#
# @examples
processAccel <- function(accel_dat, rate = 10){
  secs <- stringr::str_extract(accel_dat, '^L\\d+') %>% stringr::str_replace('L','') %>%
    as.numeric()
  record_times <- ((1:rate) - 1)/rate
  accel <- stringr::str_split(accel_dat, ',') %>%
    purrr::map(`[`, -1) %>%
    purrr::map(as.numeric) %>%
    purrr::map(function(d) {
      tmp <- data.frame(matrix(d, ncol = 3, byrow = T))
      names(tmp) <- c('x','y','z')
      return(tmp)})
  ind <- which(sapply(accel, nrow) != rate) # Checking for corrupt records
  if(length(ind)>0){
    accel <- accel[-ind]  %>%
      purrr::reduce(rbind)
    secs <- secs[-ind]
  } else {
    accel <- accel %>% purrr::reduce(rbind)
  }
  Time <- (rep(secs, rep(rate, length(secs)))) + rep(record_times, length(secs))
  out <- cbind('Time' = Time, accel)
  return(out)
}