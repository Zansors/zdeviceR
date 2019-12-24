#' Process run data from calibrated (10 Hz) data
#'
#' @param d data.frame, output from `calibrated_data`
#'
#' @return data.frame with various run statistics
#' @export
#'
#' @examples
process_run_v2 <- function(d){
  suppressPackageStartupMessages(library(tidyverse))
  require(zdeviceR)
  require(RcppRoll)

  d <- d %>% mutate(avgd = as.numeric(roll_mean(abs(scale(sound)),
                                                n = 10, fill = NA))) %>%
    mutate(avgd2 = as.numeric(roll_mean(avgd, n = 10, fill = NA))) %>%
    mutate_at(vars(x:z), roll_mean, n = 1, fill = NA)
  peaks <- tibble(
    mins = d$Mins,
    cadence = rep(0, nrow(d)),
    respiration = rep(0, nrow(d))
  ) %>%
  mutate(respiration = ifelse(is.na(d$avgd2), NA, respiration))
  peaks$cadence[detect_peaks(d$y, mpd = 5)] <-  1
  peaks$respiration[detect_peaks(d$avgd2, mpd=20, threshold = 0.001)] <- 1
peaks <- peaks %>%
  mutate(cadence_rate = roll_sum(cadence, n = 600, fill = NA),
         respiration_rate = roll_sum(respiration,  n = 600, fill = NA))

resp_rate <- peaks
n_na <- min(which(!is.na(resp_rate$respiration)))-1
resp_variability <- resp_rate %>%
  mutate(m = c(rep(NA, n_na), cum_mean(respiration[(n_na+1):n()]))*600) %>%
  mutate(v = c(rep(NA, n_na), cum_var(respiration[(n_na+1):n()]))*600) %>%
  mutate(lcb = m - 2*sqrt(v),
         ucb = m + 2 * sqrt(v)) #%>%
resp_intensity <- select(d, Mins, avgd2)
n_na <- min(which(!is.na(resp_intensity$avgd2))) - 1
resp_intensity <- resp_intensity %>%
  mutate(m = c(rep(NA, n_na), cum_mean(avgd2[(n_na+1):n()]))) %>%
  mutate(v = c(rep(NA, n_na), cum_var(avgd2[(n_na+1):n()]))) %>%
  mutate(cardio_thresh = m[Mins==10] + sqrt(v[Mins==10])) %>%
  mutate(fit_thresh = m[Mins==10]+ 3*sqrt(v[Mins==10])) %>%
  mutate(over_thresh = m[Mins==10] + 6*sqrt(v[Mins==10])) %>%
  mutate(signal = roll_meanr(avgd2, 600)) %>%
  # select(-m, -v) %>%
  rename(mins = Mins,  intensity = avgd2, cum_mean = m, cum_var = v,
         fit_thresh=fit_thresh, over_thresh=over_thresh, mean_signal = signal)
out <- bind_cols(list(select(d, x:sound), resp_rate,
                      select(resp_variability, m:ucb),
                      select(resp_intensity, intensity:mean_signal)))
return(out)
}
