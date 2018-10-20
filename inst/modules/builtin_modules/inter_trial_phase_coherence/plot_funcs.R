# Plot functions

#' @author Zhengjia Wang, John Magnotti
#' @description Provide easy ways to set plot layouts
#' @param K number of plots to be made
#' @param nrows number of rows for the plot, default 1
#' @param legend expression for generating legend, see "?legend"
#' @param legend_size legend width/height, default is lcm(3)
#' @param legend_side 1 - bottom, 2 - left, 3 - top, 4 - right. Default is 4
#' @param s_margin margins within each plots see "?par" for "mar"
#' @param b_margin margins for the whole plot see "?par" for "oma"
#' @param l_margin legend margin
easy_layout <- function(K, nrows = 1, legend,
                        legend_size = lcm(3), legend_side = 4,
                        s_margin = par('mar'), b_margin = par('oma'),
                        l_margin = local({
                          mar = s_margin;
                          mar[legend_side] = 0;
                          mar[(legend_side + 2) %% 4] = 0.5;
                          mar
                        })){

  # calculate nrow and ncols
  ncols = ceiling(K / nrows)
  K = nrows * ncols
  mat = matrix(seq_len(K) + 1, nrow = nrows, byrow = T)


  switch (as.character(legend_side),
    '1' = {
      mat = rbind(mat, 1)
      layout(mat, heights = c(rep(1, nrows), legend_size))
    },
    '2' = {
      mat = cbind(1, mat)
      layout(mat, widths = c(legend_size, rep(1, ncols)))
    },
    '3' = {
      mat = rbind(1, mat)
      layout(mat, heights = c(legend_size, rep(1, nrows)))
    },
    {
      mat = cbind(mat, 1)
      layout(mat, widths = c(rep(1, ncols), legend_size))
    }
  )
  oma = par('oma')
  mar = par('mar')
  re = list(
    oma = oma,
    mar = mar
  )

  par(oma = b_margin)

  # draw legend first!
  parent_env = parent.frame()
  expr = eval(substitute(substitute(legend)), parent_env)

  par(mar = l_margin)
  eval(expr, envir = new.env(parent = parent_env))

  par(mar = s_margin)
}

#' Try to guess decimal points of the number and returns string
pretty_num <- function(x, digits = 3, roundup = 5, decimal.mark = '.', ...){
  ss = base::prettyNum(x, ...)
  s = unlist(strsplit(ss, ''))
  sel = s == decimal.mark
  if(sum(sel)){
    e = which(sel) + digits
    l = length(s)
    if(l > e && s[e+1] >= roundup){
      s[e] = as.character(as.integer(s[e]) + 1L)
    }
    end = min(e, l)
    paste(s[1:end], collapse = '')
  }else{
    ss
  }
}

#' @author John Magnotti
#' @Note We are just plotting image(zmat) rather than t(zmat) as you might expect. the rave_calculators know this so we can
#' save a few transposes along the way
#' @description The idea here is to to separate the plotting of the heatmap from all the accoutrements that are done in the decorators
draw_img <- function(zmat, x, y, xlab='Time (s)', ylab='Frequency (Hz)',
                     zlim, log='', ...) {

  zmat %<>% clip_x(lim=zlim)

  image(x=x, y=y, z=zmat,
        zlim=zlim, col=crp, xlab=xlab, ylab=ylab, cex.lab=rave_cex.axis, axes=F, useRaster = FALSE, log=log, ...)

  # return the clipped zmat
  invisible(zmat)
}

heat_map_axes <- function(x, y, xax=TRUE, yax=TRUE, yntick=6) {
  if(missing(x)) x <- time_points
  if(missing(y)) y <- frequencies

  if(xax) rave_axis(1, at=pretty(x), tcl=0, lwd=0)
  if(yax) rave_axis(2, at=quantile(y, 0:(yntick-1)/(yntick-1)) %>% round, tcl=0, lwd=0)
}



rave_color_bar <- function(zlim, actual_lim, clrs=rave_heat_map_colors, ylab='Mean % Signal Change',
                           mar=c(5.1, 5.1, 2, 2), sym = T, ticks = NULL, digits = 1, ...) {
  max_zlim = max(abs(zlim))
  if(sym || length(zlim) == 1){
    draw_zlim = c(-1,1) * max_zlim
  }else{
    draw_zlim = zlim
  }


  cbar <- matrix(seq(-max_zlim, max_zlim, length=length(rave_heat_map_colors))) %>% t

  # this is from -1 to 1
  x = cbar / max_zlim
  sel = x %within% draw_zlim
  par(mar=mar)
  image(cbar[, sel, drop = F],
        col=clrs[sel], axes=F, ylab=ylab, main='',
        cex.main=rave_cex.main, cex.lab=rave_cex.lab, cex.axis=rave_cex.axis, ...)

  rave_main(sprintf(
    '[%s]',
    paste(sapply(actual_lim, pretty_num, digits = digits), collapse = ':')
  ))

  labels = c(draw_zlim, -max_zlim, max_zlim, 0, ticks)
  at = labels / max_zlim
  unique = !duplicated(at)

  start = min(x[sel])
  end = max(x[sel]) - start
  rave_axis(2, at= (at[unique] - start) / end, labels = sprintf(
    sprintf('%%.%df', digits), labels[unique]), tcl=0.3)
  box()

  invisible(zlim)
}







#
# # Test
# easy_layout(3, legend = {
#   rave_color_bar(zlim = c(0,1), actual_lim = c(0, 1), ylab = 'Inter-Trial Coherence', sym = F, ticks = 0.6)
# })



















