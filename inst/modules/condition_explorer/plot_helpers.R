#
# functions to make it easier to use base plots
#

# this has some nice formatters in it
source('./adhoc/condition_explorer/ecog_io_functions.R')

`%&%` <- function(s1,s2) paste0(s1,s2)


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

ebar_polygon = function(x, y, sem, alpha=100, col='black', fill=col, stroke=col, border = NA, add_line=TRUE, lwd=1, ...) {
  polygon(c(x, rev(x)), c(y + sem, rev(y - sem)), border = border, col = getAlphaRGB(fill, alpha))

  if(add_line) lines(x,y, col=stroke, lwd=lwd, ...)
}


getAlphaRGB = function(colname, alpha) {
  c = col2rgb(colname)
  rgb(t(c), alpha = alpha, maxColorValue = 255)
}

plot.clean = function(xlim, ylim, x = 1, y = 1, type = "n", xlab="", ylab="", ...) {
  plot(x, y, type = type, axes = F, ylab = ylab, xlab = xlab, xlim = range(xlim), ylim = range(ylim), ...)
}

get_list_elements <- function(ll, name) sapply(ll, getElement, name)

rave.axis <- function(side, at, tcl=-0.3, labels=at, las=1, cex.axis=1.5, mgpy=c(3, .5, 0), mgpx=c(3, .4, 0), ...) {
  if(length(side) > 1) {
    return (invisible(sapply(side, rave.axis,
      at=at, tcl=tcl, labels=labels, cex.axis=cex.axis, las=las, ...)
    ))
  }
  mgp <- mgpy
  if(side %% 2) mgp <- mgpx

  axis(side, at=at, labels=at, tcl=tcl, mgp=mgp, cex.axis=cex.axis, las=las, ...)
}


#
# group_data is a list
# this is a list to allow different N in each group
# NB: the reason we use barplot here is that it takes care of the width between groups for us, even though by default we don't actually show the bars
barplot.scatter = function(group_data, ylim, bar.cols=NA, bar.borders=NA, cols, ebar.cols='black', ebar.lwds=3, jitr_x,
                            pchs=19, pt.alpha=200, xlab='Group', ylab='Mean % Signal Change', ...) {

  nms <- group_data %>% get_list_elements('name')

  gnames <- nms
  if(any(duplicated(nms))) gnames <- paste0(LETTERS[seq_along(nms)], nms)

  ns <- group_data %>% get_list_elements('N')

  flat_data <- data.frame(
    'group' = factor(rep(gnames, ns)),
    'sigchange' = lapply(group_data, getElement, 'trials') %>% unlist
  )

  mses <- with(flat_data, do.call(rbind, tapply(sigchange, group, m_se)))

  yax <- do_if(missing(ylim), {
    pretty(flat_data$sigchange, high.u.bias = 100, n=4, min.n=3)
  }, ylim)

  x <- barplot(mses[,1], las=1, ylim=range(yax) %>% stretch(.01), col=bar.cols, border=bar.borders,
    ylab=ylab, names.arg=paste0(nms, ' (N=' %&% ns %&%')'), xlab=xlab, axes=F, ...)

  rave.axis(2, at=yax)

  if(min(yax) < 0) abline(h=0, col='lightgray')

  # if there are > 1 groups in the data, then do linear model, otherwise one-sample t-test
  if(length(unique(flat_data$group)) > 1) {
      res <- with(flat_data, {
        list( 'omni' = lm(sigchange ~ group -1) %>% summary,
               'cond' = lm(sigchange ~ group) %>% summary
            )
      })

      title(H[0] ~ mu[i] == mu[j] * ', ' ~ i != j)
  } else {
    res <- t.test(flat_data$sigchange)

    title(bquote(H[0] ~ mu == 0))
    #  '  p = ', res$p.value %>% format(digits=1)), cex.main=1.4)
  }

  #emphasize the means, this value +/- .4 should be set somwhere, right?
  lsize <- (1/3)*mean(unique(diff(x)))

  if(missing(jitr_x)) jitr_x <- 0.75*lsize

  if(missing(cols)) cols <- get_color(seq_along(group_data))

  mapply(function(xi, mi) {
    if(!is.na(mi))
      lines(xi + c(-lsize, lsize), rep(mi, 2), lwd=3, lend=2)
  }, x, mses[,1])

  par_rep <- function(y) rep_len(y, nrow(mses))

  cols %<>% par_rep
  pchs %<>% par_rep
  bar.cols %<>% par_rep
  ebar.cols %<>% par_rep
  bar.borders %<>% par_rep

  for(ii in seq_along(group_data)) {
    if(!is.null(group_data[[ii]])) {
      add_points(x[ii], group_data[[ii]]$trials, col=getAlphaRGB(cols[ii], pt.alpha), pch=pchs[ii], jitr_x=jitr_x)
      ebars(x[ii], mses[ii,1], mses[ii,2], lwd=ebar.lwds, col=ebar.cols, code=0)
    }
  }

  logger(mses)

  # in case people need to further decorate
  invisible(x)
}


jitr = function(x, len=length(x), r=0.35) {
  x + runif(len, -r, r)
}

add_points = function(x, y, jitr_x=.2, pch=19, ...) {
  points(jitr(x, length(y), r=jitr_x), y, pch=pch, ...)
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
