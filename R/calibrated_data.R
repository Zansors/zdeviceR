#' Combining sound and motion into data channels at same times
#'
#' @param dat A list, output from `readData`.
#'
#' @return A single data.frame/tibble containing 5 columns: time, x, y, z, sound
#' @export
#'
#' @examples
calibrated_data <- function(dat){
  sound <- dat$Sound
  motion <- dat$Motion
  bl = motion
  bl$sound = approx(sound$Time, sound$Sound, xout = motion$Time)$y
  bl$Mins = bl$Time/60
  return(bl)
}