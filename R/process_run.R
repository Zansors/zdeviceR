#' process_run
#'
#' Processing run data to get cadence and respiration rate
#'
#' @param D A data.frame processed through zdeviceR::calibrated_data, downsampled to 10Hz
#'
#' @return A data.frame with time, Cadence and Respiration rate
#' @export
#'
#' @examples
process_run <- function(D){
  require(tidyverse)
  require(zdeviceR)
  require(RcppRoll)
  D <- D %>% mutate(avgd = as.numeric(roll_mean(abs(scale(sound)), n = 10, fill = NA))) %>%
    mutate(avgd2 = as.numeric(roll_mean(avgd, n = 10, fill = NA))) %>%
    mutate_at(vars(x:z), roll_mean, n = 1, fill = NA)
  peaks = data.frame(mins = D$Mins, cadence = rep(0, nrow(D)), respiration = rep(0, nrow(D)))
  peaks$cadence[detect_peaks(D$y)] = 1
  peaks$respiration[detect_peaks(D$avgd2, mpd=20)] <- 1
  peaks <- peaks %>% mutate_at(vars(cadence, respiration), roll_sum, n = 600, fill = NA) %>%
    set_names(c('Minutes', 'Cadence', 'Respiration')) %>% as_tibble()
  return(peaks)
}
