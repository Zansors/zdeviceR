#' Signal processing the zdevice signal
#'
#' @param d A two-column data.frame with the first column being time and the second being signal
#' @param preprocess Options are 'ma' (Moving average) and 'none'
#' @param filtering Options are 'lgf' (local Gaussian Filtering), 'gf' (Gaussian Filtering),
#'   'mgf' (Mixed Gaussian Filtering, not implemented), and 'none'
#' @param output Options are 'rmean' (running mean), 'rmedian' (running median),
#'   'apnea', and 'none'
#' @param preParams Parameters for the chosen preprocess method, as a list
#' @param filtParams Parameters for the chosen filtering method, as a list
#' @param outputParams Parameters for the chosen output method, as a list
#'
#' @import RcppRoll
#' @return
#' @export
#'
#' @examples
SignalProcessing <- function(d,
                             rate = 256,
                             thresh_right = c('stoptime' = 2000, 'last_window' = NULL),
                             preprocess = 'ma',
                             filtering = 'lgf',
                             output='rmax',
                             preParams = list(window = 256L),
                             filtParams = list(threshold = 3.5),
                             outputParams = list(window = 256L)){
  names(d) <- c('Time', 'y')

# Right threshold ---------------------------------------------------------

  if(names(thresh_right) == 'stoptime') d <- d[d$Time <= thresh_right,]
  if(names(thresh_right) == 'last_window') d <- d[1:(nrow(d)-thresh_right),]

# Pre-processing ----------------------------------------------------------

  if(preprocess == 'ma') d$y1 <- RcppRoll::roll_max(d$y, n = preParams$window,
                                                    fill = NA)
  if(preprocess == 'none') d$y1 <- d$y

# Filtering the noise -----------------------------------------------------

  if (filtering == 'gf') d$z <- GaussFilter(d$y1, threshold = filtParams$threshold,
                                            robust = filtParams$robust)
  if (filtering == 'lgf') d$z <- LocalGaussFilter(d$y1, threshold = filtParams$threshold,
                                                  robust = filtParams$robust)
  if (filtering == 'none') d$z <- d$y1

# Generating the final output ---------------------------------------------

  if (output == 'rmax') d$signal <- RcppRoll::roll_maxr(d$z, n = outputParams$window)
  if (output == 'rmean') d$signal <- RcppRoll::roll_meanr(d$z, n = outputParams$window)
  if (output == 'apnea') d$signal <- ApneaSignal(d$z, rate = rate)
  if (output == 'none') d$signal <- d$z

  return(d)
}
