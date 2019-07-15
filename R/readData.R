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
#' @import magrittr
#' @export
#'
#' @examples
#' dat <- readData(system.file('extdata','example_data.txt', package='zdeviceR'))
#' sound_data <- dat[['Sound']]
#' motion_data <- dat[['Motion']]
#' start_time <- dat[['timestamp']]
#'
readData <- function(filename, sound_rate=256, accel_rate = 10, test_grep=F){
  if(test_grep){
    require(data.table)
    require(glue)
    filename <- stringr::str_replace_all(filename, ' ', '\\\\ ')
    cmd1 <- glue("grep A {filename}")
    cmd2 <- glue("grep -v A {filename} | sed '1,4d' | sed '/^[[:space:]]*$/d'")
    sound <- as.data.frame(fread(cmd = cmd1))
    motion <- as.data.frame(fread(cmd = cmd2))
    sound_dat <- processSound(sound)
    accel_dat <- processAccel(motion)
    timestamp <- c(
      'date' = readLines(pipe(glue("grep -Eo '\\d{{2}}/\\d{{2}}/\\d{{2}}' {filename}"))),
      'time' = readLines(pipe(glue("grep -Eo '\\d{{2}}:\\d{{2}}:\\d{{2}}' {filename}")))
    )
  }
  dat <- read_file_cpp2(path.expand(filename))
  dat <- strsplit(dat, '\\r|\\n')[[1]]
  dat <- dat[dat!='']
  ts_ind <- which(stringr::str_detect(dat[1:10],'Recording'))
  timestamp <- c('date' = stringr::str_extract(dat[ts_ind], '\\d{2}/\\d{2}/\\d{2}'),
                 'time' = stringr::str_extract(dat[ts_ind], '\\d{2}:\\d{2}:\\d{2}'))
  sound_dat <- processSound(dat[stringr::str_detect(dat,'^A')])
  accel_dat <- processAccel(dat[stringr::str_detect(dat, '^L')])
  return(list('timestamp' = timestamp,
              'Sound' = sound_dat,
              'Motion' = accel_dat))
}