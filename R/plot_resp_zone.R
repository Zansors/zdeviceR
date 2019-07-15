#' Plotting breathing zones
#'
#' @param d data.frame object, output of process_run_v2
#'
#' @return
#' @export
#'
#' @examples
plot_resp_zones <- function(d){
  require(ggplot2)
  plt <- ggplot(d)+ geom_point(aes(mins, intensity), alpha = 0.5, color='grey') +
    geom_line(aes(mins, cardio_thresh), color='green', linetype=2)+
    geom_line(aes(mins, fit_thresh), color = 'green') +
    geom_line(aes(mins, over_thresh), color = 'red') +
    geom_line(aes(mins, mean_signal), color = 'brown') +
    theme_bw() +
    theme(axis.text.y = element_blank()) +
    labs(x = 'Minutes', y = 'Breathing intensity')
  threshs <- d %>% select(ends_with('thresh')) %>% summarize_all(unique) %>% as_vector()
  threshs <- c(0, threshs, 40)
  midpts <- head(threshs, -1) + diff(threshs)/2
  plt <- plt +
    annotate('text', x=5,y=midpts,
             label=c('Warmup zone', 'Cardio zone',
                     'Fitness zone', 'Overtraining zone'))
  print(plt)
}