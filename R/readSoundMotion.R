#' readSound
#'
#' Reading sound data from zdevice downloads
#'
#' @param filename Text field giving path to file to be read
#' @param sound_rate Rate that sound is sampled, in Hz
#'
#' @return a data.frame with timestamp (Time) and sound intensity (Sound) columns
#' @export
#'
#' @examples
#' sound_dat <- readSound(system.file('extdata', 'example_data.txt'), package='zdeviceR')
readSound <- function(filename, sound_rate=256) {
  dat <- read_file_cpp2(path.expand(filename))
  dat <- strsplit(dat, '\\r|\\n')[[1]]
  sound_dat <- dat[stringr::str_detect(dat,'^A')] %>% processSound(rate = sound_rate)
  return(sound_dat)
}

#' readMotion
#'
#' @param filename Text field giving path to file to be read
#' @param accel_rate Rate that motion was sampled, in Hz
#'
#' @return a data.frame with timestamp (Time) and x, y and z direction accelerations
#' @export
#'
#' @examples
#' accel_dat <- readMotion(system.file('extdata', 'example_data.txt'), package='zdeviceR')
#'
readMotion <- function(filename, accel_rate = 10){
  dat <- read_file_cpp2(path.expand(filename))
  dat <- strsplit(dat, '\\r|\\n')[[1]]
  accel_dat <- dat[stringr::str_detect(dat, '^L')] %>% processAccel(rate = accel_rate)
  return(accel_dat)
}