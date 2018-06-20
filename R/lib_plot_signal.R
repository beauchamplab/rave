#' plot signals, this is a library for visualizing signals (raw, pwelch, spectro-heatmap, etc.)
NULL



#' to be junked
plot_signal <- function(s, sample_rate, time = NULL, boundary = 1000,
                        xbins = 1000, details = F, ...){
  if(is.null(time)){
    x = seq(0, (length(s) - 1))
  }else{
    x = seq(floor(time[1] * sample_rate), ceiling(time[2] * sample_rate))
    x = x[x >= 0 & x <= (length(s) - 1)]
  }
  time = force(x / sample_rate)
  signal = force(s[x + 1])

  args = list(...)
  if(length(args[['ylim']]) == 2){
    ybnds = args[['ylim']]
  }else{
    ybnds = range(signal)
  }

  if(!is.null(boundary)){
    outsel = abs(signal) > boundary
    data.frame(
      Time = time,
      Signal = signal,
      Outliers = outsel
    ) -> tmp
  }else{
    details = F
    data.frame(
      Time = time,
      Signal = signal,
      Outliers = F
    ) -> tmp
  }




  ggplot2::ggplot() + ggplot2::aes(
    x = Time,
    y = Signal
  ) +
    ggplot2::stat_bin2d(bins = xbins, data = tmp) ->
    p

  if(details && sum(outsel) > 0){
    p + ggplot2::geom_linerange(ggplot2::aes(ymax = Signal, ymin = 0), data = tmp[outsel,]) ->
      p
  }
  if(!is.null(boundary)){
    p + ggplot2::geom_hline(ggplot2::aes(yintercept = c(boundary, -boundary)), color = 'red') ->
      p
  }
  p +

    ggplot2::scale_fill_continuous(high = '#f7786b', low = '#92a8d1') +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank()) ->
    p

  return(p)

}





