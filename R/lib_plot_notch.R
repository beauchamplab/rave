
save_notch_plots <- function(project_name, subject_code, blocks, chls, srate, ...){
  dirs = rave:::get_dir(subject_code, project_name)
  subject_dir = dirs$preprocess_dir
  for(b in blocks){
    async({
      require(rave)
      fname = file.path(subject_dir, sprintf('Notch_Block%s.pdf', b))
      cairo_pdf(fname, width = 12, height = 7, onefile = T)
      for(chl in chls){
        rave:::pre_plot_notch(project_name, subject_code, b, chl, srate, ...)
      }
      dev.off()
    }, plan = NULL)
  }
}

pre_plot_notch <- function(
  project_name, subject_code, block_num, chl, srate = 2000,
  compress = 20,
  window = 128, noverlap = 8, xlim = c(0, 300), cex = 1, ...
){
  dirs = rave:::get_dir(subject_code, project_name)
  subject_dir = dirs$preprocess_dir

  cfile = file.path(subject_dir, sprintf('chl_%d.h5', chl))

  # load 3 data:
  # raw, notch, notch+compress
  s_r = rhdf5::h5read(cfile, name = sprintf('/raw/%s', block_num))
  s_n = rhdf5::h5read(cfile, name = sprintf('/notch/%s', block_num))
  s_c = rhdf5::h5read(cfile, name = sprintf('/notch/compress%d/%s', compress[1], block_num))
  boundary = 3 * sd(s_n)
  ylm = max(max(abs(s_c)), boundary) + 1

  grid::grid.newpage()
  lay <- rbind(c(1,1,1),
               c(2,3,4))

  graphics::layout(mat = lay)

  # plot 1
  plot(seq(1, length(s_c))/srate*compress, s_c, xlab = 'Time (seconds)', ylab = 'Voltage', main = sprintf('Channel - %d - Filtered signal', chl),
       type = 'l', ylim = c(-ylm-1, ylm+1),yaxt="n", col = 'black', lwd = 0.5,
       cex.axis = cex, cex.lab = cex, cex.main = cex, cex.sub = cex)
  abline(h = c(-1,1) * boundary, col = 'red')
  ticks<-c(-ylm, -boundary,0,boundary, ylm)
  axis(2,at=ticks,labels=round(ticks),
       cex.axis = cex, cex.lab = cex, cex.main = cex, cex.sub = cex)

  # plot 2, 3 too slow, need to be faster
  pwelch(s_r, fs = srate, window = window,
         noverlap = noverlap, plot = 1, col = 'orangered', cex = cex,
         log = 'y', xlim = xlim, spec_func = spectrum.pgram)
  pwelch(s_n, fs = srate, window = window, noverlap = noverlap, cex = cex, plot = 2, col = 'black', log = 'y', xlim = xlim, spec_func = spectrum.pgram)
  legend('topright', c('Raw signal', 'Notch filtered'), col = c('orangered', 'black'), lty = 1, cex = cex)


  pwelch(s_r, fs = srate, window = window, noverlap = noverlap, cex = cex, plot = 1, col = 'orangered', log = 'xy', xlim = c(0, log10(xlim[2])), spec_func = spectrum.pgram)
  pwelch(s_n, fs = srate, window = window, noverlap = noverlap, cex = cex, plot = 2, col = 'black', log = 'xy', xlim = c(0, log10(xlim[2])), spec_func = spectrum.pgram)
  legend('topright', c('Raw signal', 'Notch filtered'), col = c('orangered', 'black'), lty = 1, cex = cex)

  hist(s_n, nclass = 100,
       xlab = 'Signal Voltage - Notch filtered', main = 'Histogram',
       cex.axis = cex, cex.lab = cex, cex.main = cex, cex.sub = cex)


}



spectrum.pgram <- function (
  x, spans = NULL, taper = 0.1, plot = FALSE, max_freq = 300, ...
)
{
  if(!is.ts(x)){
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
  }
  xfreq <- frequency(x)
  x <- as.matrix(x)
  N <- N0 <- nrow(x)
  nser <- 1

  # if (detrend) {
  t <- 1L:N - (N + 1)/2
  sumt2 <- N * (N^2 - 1)/12
  for (i in 1L:ncol(x)) x[, i] <- x[, i] - mean(x[, i]) -
    sum(x[, i] * t) * t/sumt2
  # }
  # else if (demean) {
  x <- sweep(x, 2, colMeans(x), check.margin = FALSE)
  # }
  x <- spec.taper(x, taper)
  u2 <- (1 - (5/8) * taper * 2)
  u4 <- (1 - (93/128) * taper * 2)
  # if (pad > 0) {
  #   x <- rbind(x, matrix(0, nrow = N * pad, ncol = ncol(x)))
  #   N <- nrow(x)
  # }
  NewN <- nextn(N)
  x <- rbind(x, matrix(0, nrow = (NewN - N), ncol = ncol(x)))
  N <- nrow(x)

  freq <- seq.int(from = xfreq/N, by = xfreq/N, to = max_freq)
  Nspec <- length(freq)
  xfft <- mvfft(x)
  pgram <- array(NA, dim = c(N, ncol(x), ncol(x)))
  for (i in 1L:ncol(x)) {
    for (j in 1L:ncol(x)) {
      pgram[, i, j] <- xfft[, i] * Conj(xfft[, j])/(N0 *
                                                      xfreq)
      pgram[1, i, j] <- 0.5 * (pgram[2, i, j] + pgram[N,
                                                      i, j])
    }
  }

  df <- 2
  bandwidth <- sqrt(1/12)

  df <- df/(u4/u2^2)
  df <- df * (N0/N)
  bandwidth <- bandwidth * xfreq/N
  pgram <- pgram[2:(Nspec + 1), , , drop = FALSE]
  spec <- matrix(NA, nrow = Nspec, ncol = nser)
  for (i in 1L:nser) spec[, i] <- Re(pgram[1L:Nspec, i, i])
  # if (nser == 1) {
  #   coh <- phase <- NULL
  # }
  # else {
  #   coh <- phase <- matrix(NA, nrow = Nspec, ncol = nser *
  #                            (nser - 1)/2)
  #   for (i in 1L:(nser - 1)) {
  #     for (j in (i + 1):nser) {
  #       coh[, i + (j - 1) * (j - 2)/2] <- Mod(pgram[,
  #                                                   i, j])^2/(spec[, i] * spec[, j])
  #       phase[, i + (j - 1) * (j - 2)/2] <- Arg(pgram[,
  #                                                     i, j])
  #     }
  #   }
  # }
  for (i in 1L:nser) spec[, i] <- spec[, i]/u2
  spec <- drop(spec)
  spg.out <- list(freq = freq, spec = spec, df = df, bandwidth = bandwidth,
                  method = "Raw Periodogram")
  return(spg.out)
}






