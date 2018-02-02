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
                             preprocess = 'ma',
                             filtering = 'lgf',
                             output='rmax',
                             preParams = list(window = 256L),
                             filtParams = list(threshold = 3.5),
                             outputParams = list(window = 256L)){
  names(d) <- c('Time', 'y')
  if (preprocess == 'ma') {
    d$y1 <- RcppRoll::roll_max(d$y, n = preParams$window,
                               align = 'center', fill = 'center')
  } else if (preprocess == 'none') {
    d$y1 <- d$y
  }

}
