#' Title
#'
#' @param d
#' @param preprocess
#' @param filtering
#' @param output
#' @param preParams
#' @param filtParams
#' @param outputParams
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
  if(names(thresh_right) == 'stoptime') d <- d[d$Time <= thresh_right,]
  if(names(thresh_right) == 'last_window') d <- d[1:(nrow(d)-thresh_right),]
  d$y1 <- case_when(
    preprocess == 'ma' ~ RcppRoll::roll_max(d$y, n = preParams$window,
                                            align='center', fill = 'center'),
    preprocess == 'none' ~ d$y
  )

}
