# Processing sound data
#
# @param sound_dat A character vector obtained in readData
# @param rate Sampling rate for sound dat in Hz
#
# @return A data frame containing times and corresponding recorded signal
#
#
# @examples
processSound <- function(sound_dat, rate = 256){
 secs <- stringr::str_extract(sound_dat, '^A\\d+') %>% stringr::str_replace('A','') %>%
    as.numeric()
  record_times <- ((1:rate)-1)/rate
  Sound <- stringr::str_split(sound_dat, ',') %>%
    purrr::map(`[`, -1) %>%
    purrr::map(as.numeric) %>%
    unlist()
  Time <- (rep(secs, rep(rate, length(secs))))+rep(record_times, length(secs))
  return(data.frame('Time'=Time, 'Sound'=Sound))
}