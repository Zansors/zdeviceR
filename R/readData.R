#' Reading data from zdevice
#'
#' @param filename A string with the path to the data file
#' @param sound_rate Sampling rate for sound data (default = 256)
#' @param accel_rate Sampling rate for motion data (default = 10)
#'
#' @return A list with 3 components:
#'    \enumerate{
#'        \item timestamp: The timestamp when the recording was started
#'        \item Sound: A data.frame with time and sound data
#'        \item Motion: A data.frame with time and motion data (x, y, and z axes)}
#'
#' @export
#'
#' @examples
readData <- function(filename, sound_rate=256, accel_rate = 10){
  x <- readLines(filename)
  timestamp <- c('date' = stringr::str_extract(x[2], '\\d{2}/\\d{2}/\\d{2}'),
                 'time' = stringr::str_extract(x[2], '\\d{2}:\\d{2}:\\d{2}'))
  dat <- x[-(1:4)]
  sound_dat <- dat[stringr::str_detect(dat,'^A')] %>% processSound
  accel_dat <- dat[stringr::str_detect(dat, '^L')] %>% processAccel
  return(list('timestamp' = timestamp,
              'Sound' = sound_dat,
              'Motion' = accel_dat))
}