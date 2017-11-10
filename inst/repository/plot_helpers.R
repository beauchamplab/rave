#
# functions to make it easier to use base plots
#


# a default plot that can be used if the user hasn't specified data -- only use this until we get a better solution
default_plot <- function() {
  plot.clean(1, 1, type='n', main='No Conditions Specified')
}


ebars = function(x, y=NULL, sem=NULL, length = 0.05, type='n', col='black', pt.col=col, code=2, ...) {
  if(is.null(y)) {
    if(is.matrix(x)) {
      y <- x[,1]
      sem <- x[,2]
    } else {
      y <- x
    }
    x <- seq_along(y)
  }

  if(is.matrix(y)) {
    sem <- y[,2]
    y <- y[,1]
  }

  if(is.null(sem)) {
    sem <- y
    y <- x
    x <- seq_along(y)
  }

  ebars.y(x, y, sem, length, code=code, col=col, ...)
  points(x, y, type=type, col=pt.col, ...)
}

ebars.x = function(x, y, sem, length = 0.05, ...) {
  arrows(x - sem, y, x + sem, y, angle = 90, code = 3, length = length, ...)
}

ebars.y = function(x, y, sem, length = 0.05, up = T, down = T, code = 2, ...) {
  if (up) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y + sem), angle = 90, code = code, length = length, ...)
  }
  if (down) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y - sem), angle = 90, code = code, length = length, ...)
  }
}

ebar_polygon = function(x, y, sem, col, border = NA) {
  polygon(c(x, rev(x)), c(y + sem, rev(y - sem)), border = border, col = col)
}


getAlphaRGB = function(colname, alpha) {
  c = col2rgb(colname)
  rgb(t(c), alpha = alpha, maxColorValue = 255)
}

plot.clean = function(xlim, ylim, x = 1, y = 1, type = "n", xlab="", ylab="", ...) {
  plot(x, y, type = type, axes = F, ylab = ylab, xlab = xlab, xlim = range(xlim), ylim = range(ylim), ...)
}

# put each variable into a separate column
# group_data is a list with components A and B
# this is a list to allow different N in each group
# the reason to use barplot here is that it takes care of the width between groups for us, even though by default we don't actually show the bars
barplot.scatter = function(group_data, ylim=NULL, bar.cols=NA, bar.borders=NA, cols='black', ebar.lwds=3, ebar.cols=cols,
                            pchs=19, pt.alpha=200, jitr_x=.35, xlab='Group', ylab='Mean % Signal Change', ...) {

  logger(str(group_data))

  mses <- group_data %>% sapply(m_se)

  print(mses)


  yax <- do_if(ylim %>% is.null, {
    group_data %>% sapply(range) %>% pretty
  }, ylim)

  mses_plot <- mses[,!is.na(mses[1,]),drop=FALSE]

  x <- barplot(mses_plot, las=1, ylim=range(yax) %>% stretch(.01), col=bar.cols, border=bar.borders,
    ylab=ylab, names.arg=colnames(mses_plot), xlab=xlab, axes=F, ...)

  axis(2, at=yax, las=1, cex.axis=1.4)
  if(min(yax) < 0) abline(h=0, col='lightgray')

  # if one of these is NULL, t-test still works
  wx <- group_data[[1]]
  wy <- group_data[[2]]

  if(is.null(wx)) {
      wx <- group_data[[2]]
      wy <- NULL
  }

  # p_val <- wilcox.test(wx, wy) %>% getElement('p.value')

  ts <- t.test(wx, wy)

  title(paste0('t-test: t = ', ts$statistic %>% format(digits=2), '  p = ', ts$p.value %>% format(digits=1)), cex.main=1.4)

  #emphasize the means
  mapply(function(xi, mi) {
    if(!is.na(mi))
      lines(xi + c(-.4, .4), rep(mi, 2), lwd=3, lend=2)
  }, x, mses[1,])


  cols = rep_len(cols, ncol(mses))
  pchs = rep_len(pchs, ncol(mses))
  bar.cols = rep_len(bar.cols, ncol(mses))
  ebar.cols = rep_len(ebar.cols, ncol(mses))
  bar.borders = rep_len(bar.borders, ncol(mses))

  for(ii in seq_along(group_data)) {
    if(!is.null(group_data[[ii]])) {
      add_points(x[ii], group_data[[ii]], col=getAlphaRGB(cols[ii], pt.alpha), pch=pchs[ii], jitr_x=jitr_x)
      ebars(x[ii], mses[1,ii], mses[2,ii], lwd=ebar.lwds, col=ebar.cols, code=0)
    }
  }


  # in case people need to further decorate
  invisible(x)
}


jitr = function(x, len=length(x), r=0.35) {
  x + runif(len, -r, r)
}

add_points = function(x, y, jitr_x=.2, pch=19, ...) {
  points(jitr(x, length(y), r=.2), y, pch=pch, ...)
}

# # # calcluation helpers

# mean +/- se
m_se <- function(x) c(mean(x), sd(x)/sqrt(length(x)))
mat_m_se <- function(m, DIM=2) apply(m, DIM, m_se)

se <- function(x) sd(x) / sqrt(length(x))

#mean +/- sd
m_sd <- function(x) c(mean(x), sd(x))

not_null <- function(x) !is.null(x)

do_if <- function(boolean_expression, if_clause, else_clause=NULL) {
  if(boolean_expression)
    return (if_clause)

  return (else_clause)
}


# make it easier to say not is.na in a pipe'd context
not_NA = function(x) !is.na(x)


# needed to simplify long expressions
colDiff <- function(m, ord=1:2) m[,ord[1]] - m[,ord[2]]

# 0-1 scale the data so we can manage the plot ranges easily
scl01 <- function(x) (x-min(x)) / diff(range(x))

# ensure data are within some bounds
clip_x <- function(x, lim) {
  x[x<min(lim)] <- min(lim)
  x[x>max(lim)] <- max(lim)

  x
}

# check for containment, by default containiment is inclusive of the boundaries: min(rng) <= x <= max(rng)
# specificy your own functions for lower bound and upper bound if you want
# works with vector-valued x for default LB_ and UB_ functions
# also works when length(rng) == 1
# is_within = function(x, rng, UB_FUNC=`<=`, LB_FUNC=`>=`) {
#   (x %>% UB_FUNC(max(rng))) &
#     (x %>% LB_FUNC(min(rng)))
# }

is_within <- function(x, minmax) (x>=minmax[1]) & (x<=minmax[2])

stretch <- function(x, pct) {
  d <- pct * diff(range(x))
  c(min(x)-d, max(x)+d)
}
