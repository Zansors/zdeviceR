#' Plot breating rate
#'
#' @param d data.frame, output from process_run_v2
#'
#' @return
#' @export
#'
#' @examples
plot_resp_rate <- function(d){
  require(ggplot2)
  require(glue)
  plt <- ggplot(d, aes(x = mins, y = respiration_rate)) +
    geom_line() +
    ylim(0,40) +
    labs(x = 'Minutes', y = 'Breathing rate') +
    theme_bw()
  plt <- plt + annotate('text',
                        x = max(d$mins, na.rm=T)-7,
                        y = 35,
                        label = glue("Average rate = {round(mean(d$respiration_rate, na.rm=T), 1)} / min"))
  print(plt)
}