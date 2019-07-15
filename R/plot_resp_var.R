#' Plot breathing variability
#'
#' @param d data.frame, output of process_run_v2
#'
#' @return
#' @export
#'
#' @examples
plot_resp_var <- function(d){
  require(ggplot2)
  plt <- ggplot(resp_variability) + geom_line(aes(mins, respiration_rate)) +
    geom_line(aes(mins, lcb), color='red') +
    geom_line(aes(mins, ucb), color = 'red') +
    # ylim(0,80) +
    labs(x = 'Minutes', y = 'Breathing rate')
}
