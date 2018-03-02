save_car_plots <- function(project_name, subject_code, blocks, chls, srate, plan, ...){
  dirs = rave:::get_dir(subject_code, project_name)
  subject_dir = dirs$preprocess_dir
  for(b in blocks){
    async({
      require(rave)
      fname = file.path(subject_dir, sprintf('CAR_%s_Block%s.pdf', plan, b))
      cairo_pdf(fname, width = 12, height = 7, onefile = T)
      for(chl in chls){
        rave:::pre_plot_car(project_name, subject_code, b, chl, srate, ...)
      }
      dev.off()
    }, plan = NULL)
  }
}

pre_plot_car <- function(
  project_name, subject_code, block_num, chl, srate = 2000,
  compress = 20, col = 'black',
  window = 128, noverlap = 8, xlim = c(0, 300), cex = 1, ...
){
  dirs = rave:::get_dir(subject_code, project_name)
  subject_dir = dirs$preprocess_dir

  cfile = file.path(subject_dir, sprintf('chl_%d.h5', chl))

  # load 3 data:
  # notch, new, new+compress
  s_r = rhdf5::h5read(cfile, name = sprintf('/notch/%s', block_num))
  s_n = rhdf5::h5read(cfile, name = sprintf('/CAR/%s', block_num))
  s_c = rhdf5::h5read(cfile, name = sprintf('/CAR/compress%d/%s', compress[1], block_num))
  boundary = 3* sd(s_n)
  ylm = max(max(abs(s_c)), boundary) + 1

  grid::grid.newpage()
  lay <- rbind(c(1,1,1),
               c(2,3,4))

  graphics::layout(mat = lay)

  # plot 1
  plot(seq(1, length(s_c))/srate*compress, s_c, xlab = 'Time (seconds)', ylab = 'Voltage',
       main = sprintf('Channel - %d - Filtered signal', chl), lwd = 0.5,
       type = 'l', ylim = c(-ylm-1, ylm+1),yaxt="n", col = col,
       cex.axis = cex, cex.lab = cex, cex.main = cex, cex.sub = cex)
  abline(h = c(-1,1) * boundary, col = 'red')
  ticks<-c(-ylm, -boundary,0,boundary, ylm)
  axis(2,at=ticks,labels=round(ticks),
       cex.axis = cex, cex.lab = cex, cex.main = cex, cex.sub = cex)

  # plot 2, 3 too slow, need to be faster
  pwelch(s_r, fs = srate, window = window,
         noverlap = noverlap, plot = 1, col = 'grey80', cex = cex,
         log = 'y', xlim = xlim, spec_func = rave:::spectrum.pgram)
  pwelch(s_n, fs = srate, window = window, noverlap = noverlap, cex = cex, plot = 2, col = col, log = 'y', xlim = xlim, spec_func = spectrum.pgram)
  legend('topright', c('Before CAR', 'After CAR'), col = c('grey80', col), lty = 1, cex = cex)


  pwelch(s_r, fs = srate, window = window, noverlap = noverlap, cex = cex, plot = 1, col = 'grey80', log = 'xy', xlim = c(0, log10(xlim[2])), spec_func = spectrum.pgram)
  pwelch(s_n, fs = srate, window = window, noverlap = noverlap, cex = cex, plot = 2, col = col, log = 'xy', xlim = c(0, log10(xlim[2])), spec_func = spectrum.pgram)
  legend('topright', c('Before CAR', 'After CAR'), col = c('grey80', col), lty = 1, cex = cex)

  hist(s_n, nclass = 100,
       xlab = 'Signal Voltage - Notch filtered', main = 'Histogram',
       cex.axis = cex, cex.lab = cex, cex.main = cex, cex.sub = cex)


}


