#' AudioProcessing
#'
#' Signal processing the zdevice audio signal
#'
#' This function allows the modular implementation of different pre-processing, filtering
#' and summarization steps in processing zdevice audio signal.
#' There are several choices for each of these
#' steps already implemented.
#'
#'
#' **Pre-processing**
#'
#' 1. "none" : No pre-processing
#' 2. "ma" : Run a moving average on the raw signal
#'     + "window" : The width of the moving average window in seconds
#'
#' **Filtering**
#'
#' 1. "none": No filtering
#' 2. "gf" : Gaussian filtering
#'     - "threshold": The threshold value beyond which we consider the filtered values to be signal
#'     - "robust": Use `median` and `mad` to scale the data instead of `mean` and `sd`
#' 3. "lgf": Local Gaussian Filtering (Default)
#'     - "threshold": The threshold value beyond which we consider the filtered values to be signal
#'     - "window" : The window for computing the local mean and sd for scaling the data, in seconds
#'     - "robust": *not implemented*
#'
#' **Summarizing**
#'
#' 1. "none" : no summarizing
#' 2. "rmax" : Moving maximum (Default)
#'     - "window": Window of the moving average in seconds
#' 3. "rmean" : Moving average
#'     - "window": Window of moving average in seconds
#' 4. "apnea" : The apnea detection methodology (not implemented)
#'
#' **Specifying parameters:** One way to specify the parameters in this function call is to create
#' a YAML file of the parameters and read it with `yaml::read_yaml`. This would get to a nested list
#' that could be passed as the `params` object. For example::
#'
#' ```
#' pre:
#'   window: 1
#' filt:
#'   robust: FALSE
#'   threshold: 3
#'   roll_window: 30
#' output:
#'   window: 1
#' thresh_right:
#'   stoptime: 2000
#'
#' ```

#' @param d A two-column data.frame with the first column being time and the second being signal
#'
#' @param preprocess Options are 'ma' (Moving average) and 'none'
#' @param filtering Options are 'lgf' (local Gaussian Filtering), 'gf' (Gaussian Filtering),
#'   'mgf' (Mixed Gaussian Filtering, not implemented), and 'none'
#' @param output Options are 'rmean' (running mean), 'rmedian' (running median),
#'   'apnea', and 'none'
#' @param params Nested list of parameters, or path to a yaml config file (see Details)
#' @param rate Sampling rate for the signal
#' @param thresh_right Threshold to filter out signal from the end to remove artifacts like
#'     removing device and the like, that can bias results. There are two options:
#'     1. stoptime: Time (in seconds) from the end when we stop
#'     2. last_window: which is the last observation (index) to keep (is this good?)
#'
#' @import RcppRoll
#' @return
#' @export
#'
#' @examples
#'
AudioProcessing <- function(d,
                             rate = 256,
                             preprocess = 'none',
                             filtering = 'lgf',
                             output='rmax',
                             params){
  names(d) <- c('Time', 'y')
  preParams <- params$pre
  filtParams <- params$filt
  outputParams <- params$output
  thresh_right <- params$thresh_right

# Right threshold ---------------------------------------------------------
  if(length(thresh_right) > 0){
    if(names(thresh_right) == 'stoptime') d <- d[d$Time <= thresh_right$stoptime,]
    if(names(thresh_right) == 'last_window') d <- d[1:(nrow(d) - thresh_right$last_window),]
  }
# Pre-processing ----------------------------------------------------------

  if(preprocess == 'ma') d$y1 <- RcppRoll::roll_max(d$y, n = preParams$window * rate,
                                                    fill = NA)
  if(preprocess == 'none') d$y1 <- d$y

  print('Done pre-processing...')
# Filtering the noise -----------------------------------------------------

  if (filtering == 'gf') d$z <- GaussFilter(d$y1, threshold = filtParams$threshold,
                                            robust = filtParams$robust)
  if (filtering == 'lgf') d$z <- LocalGaussFilter(d$y1, n = filtParams$roll_window * rate,
                                                  threshold = filtParams$threshold,
                                                  robust = filtParams$robust)
  if (filtering == 'none') d$z <- d$y1

  print('Done filtering...')

# Generating the final output ---------------------------------------------

  if (output == 'rmax') d$signal <- RcppRoll::roll_max(d$z, n = outputParams$window * rate, na.rm = T, fill = NA)
  if (output == 'rmean') d$signal <- zoo::rollmean(d$z, outputParams$window * rate, na.rm = T, na.pad=T)
  if (output == 'apnea') d$signal <- ApneaSignal(d$z, rate = rate)
  if (output == 'none') d$signal <- d$z

  print('Creating signal...')

  return(d)
}
