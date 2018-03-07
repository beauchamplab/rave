#
# functions to make it easier to use base plots
#

# this has some nice formatters in it
source('./adhoc/condition_explorer/ecog_io_functions.R')

`%&%` <- function(s1,s2) paste0(s1,s2)

# check for containment, by default containiment is inclusive of the boundaries: min(rng) <= x <= max(rng)
# specificy your own functions for lower bound and upper bound if you want
# works with vector-valued x for default LB_ and UB_ functions
# also works when length(rng) == 1
is_within <- function(x, minmax) (x>=minmax[1]) & (x<=minmax[2])

`%within%` <- function(a,b) {
  is_within(a,b)
}


#
# I'm thinking to put some constants here so that we get a general look/feel on our plots
#
rave_cex.main <- 1.5
rave_cex.axis <- 1.3
# putting this to 1.4 because 1.5 causes some clipping of the axis(2) label, we could also try to increase
# the left margin to compensate
rave_cex.lab <- 1.4

rave_axis <- function(side, at, tcl=-0.3, labels=at, las=1, cex.axis=rave_cex.axis, cex.lab=rave_cex.lab, mgpy=c(3, .6, 0), mgpx=c(3, .75, 0), ...) {
  if(length(side) > 1) {
    return (invisible(sapply(side, rave_axis,
      at=at, tcl=tcl, labels=labels, cex.axis=cex.axis, las=las, cex.lab=cex.lab, ...)
    ))
  }
  mgp <- mgpy
  if(side %% 2) mgp <- mgpx

  invisible(axis(side, at=at, labels=labels, tcl=tcl, mgp=mgp, cex.axis=cex.axis, las=las, cex.lab=cex.lab, ...))
}

# rave_title



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


do_poly <- function(x, y, col, alpha=50, ...) {
  polygon(c(x,rev(x)), rep(y, each=2), col=getAlphaRGB(col, alpha), border=NA, ...)
}

ebar_polygon = function(x, y, sem, alpha=100, col='black', fill=col,
  stroke=col, border = NA, add_line=TRUE, lwd=1, ...) {

  polygon(c(x, rev(x)), c(y + sem, rev(y - sem)), border = border, col = getAlphaRGB(fill, alpha))

  if(add_line) lines(x,y, col=stroke, lwd=lwd, ...)
}


getAlphaRGB = function(colname, alpha) {
  c = col2rgb(colname)
  rgb(t(c), alpha = alpha, maxColorValue = 255)
}

plot.clean = function(xlim, ylim, x = 1, y = 1, type = "n", xlab="", ylab="",
  cex.main=rave_cex.main, cex.axis=rave_cex.axis, cex.lab=rave_cex.lab,...) {

  plot(x, y, type = type, axes = F, ylab = ylab, xlab = xlab, xlim = range(xlim), ylim = range(ylim),
    cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab, ...)
}

get_list_elements <- function(ll, name) sapply(ll, getElement, name)


abs_cdiff <- function(m) {
  if(!is.matrix(m))
    return (1)

  abs(apply(m, 1, diff))
}

get_data_range <- function(ll, range_var='range') {
  sapply(ll, getElement, range_var) %>%
    c %>% range(na.rm=TRUE) %>% round
}


# barplot function that uses all the rave sizes and colors
rave_barplot <- function(height, cex.axis=rave_cex.axis, cex.lab=rave_cex.lab, cex.names=rave_cex.lab, ...) {
  barplot(height, cex.axis=cex.axis, cex.lab=cex.lab, cex.names=cex.names, las=1, ...)
}


#
# group_data is a list
# this is a list to allow different N in each group
# NB: the reason we use barplot here is that it takes care of the width between groups for us, even though by default we don't actually show the bars
barplot.scatter = function(group_data, ylim, bar.cols=NA, bar.borders=NA, cols, ebar.cols='gray30', ebar.lwds=3, jitr_x,
                            pchs=19, pt.alpha=175, xlab='Group', ylab='Mean % Signal Change', ebar.lend=2, ...) {

  nms <- group_data %>% get_list_elements('name')

  #yes, sometimes people use the same name for different groups, or don't give names. Let's create new names
  gnames <- paste0(LETTERS[seq_along(nms)], nms)

  # let's rename the group_data so we can refer to it using globally unique name
  # possible downside: Does renaming cause a copy of group_data to be made? check the efficiency of this vs. creating a vector of names and just
  # doing the lookup group_data[[which(gnames == name_i)]]
  names(group_data) <- gnames

  ns <- group_data %>% get_list_elements('N')

  flat_data <- data.frame(
    'group' = factor(rep(gnames, ns)),
    'sigchange' = lapply(group_data, getElement, 'trials') %>% unlist
  )

  # tapply is reordering the data to be based on alphabetical by group name, because it's a factor?
  # mses <- with(flat_data, do.call(rbind, tapply(sigchange, group, m_se)))
  # now mses is a list

  # this doesn't handle the case where a group is empty
  mses <- flat_data %>% split((.)$group) %>% lapply(function(x) with(x, m_se(sigchange)))

  # if we are missing a mean for a particular group, then we need to add it in so that
  # the plot later will work
  mses <- sapply(gnames, function(ni) {
    if(exists(ni, mses)) return (mses[[ni]])

    return (c('mean'=NA, 'se'=NA))
  }, simplify = FALSE, USE.NAMES = TRUE)

  yax <- do_if(missing(ylim), {
    pretty(flat_data$sigchange, high.u.bias = 100, n=4, min.n=3)
  }, ylim)


  #there are edge cases where length(mses) != length(names), take care of this with `ind` below
  bp_names <- paste0(nms, ' (N=' %&% ns %&%')')

  # this creates space for empty groups -- is this expected behavior? It is good to preserve the color
  # mapping, but I'd rather not have the empty, space... so we need to preserve the colors but not the empty space

  ind <- sapply(mses, function(x) not_NA(x['mean'])) %>% which

  x <- rave_barplot(mses[ind] %>% sapply('[', 1), ylim=range(yax) %>% stretch(.01), col=bar.cols, border=bar.borders,
    ylab=ylab, names.arg=bp_names[ind], xlab=xlab, axes=F, ...)

  rave_axis(2, at=yax)

  if(min(yax) < 0) abline(h=0, col='lightgray')

  #
  # FIXME I don't like doing all this stats work in here, can this be pushed somewhere else?
  #

  # if there are > 1 groups in the data, then do linear model, otherwise one-sample t-test
  if(length(unique(flat_data$group)) > 1) {
      # we need to check if they have supplied all identical data sets
      # the first condition being false should short-circuit so we don't get
      # a non-conformable error for the second condition
      if(all(0==diff(sapply(group_data, '[[', 'N'))) &
          all(1e-10 > abs_cdiff(sapply(group_data, '[[', 'trials'))) )
      {
        title('Conditions are too similar to compare')
      } else {
          res <- get_f(sigchange ~ group, flat_data) %>% pretty

          title(bquote(H[0] ~ mu[i] == mu[j] * ';' ~ R^2 == .(res[1]) ~ ',' ~ F == .(res[2]) ~ ','~ p==.(res[3])),
            cex.main=rave_cex.main)
      }

  } else {
    res <- flat_data$sigchange %>% get_t %>% pretty

    title(bquote(H[0] * ':' ~ mu == 0 * ';' ~ bar(x)==.(res[1]) * ',' ~ t==.(res[2]) * ',' ~ p==.(res[3])),
      cex.main=rave_cex.main)
  }


  #emphasize the means
  lsize <- (1/3)*mean(unique(diff(x)))
  # this means there is only 1 group. If there is one group, the barplot seems to get placed at 0.7, with
  # the usr range being 0.16 - 1.24.
  if(is.na(lsize)) lsize <- 1/3

  if(missing(jitr_x)) jitr_x <- 0.75*lsize

  if(missing(cols)) cols <- get_color(seq_along(group_data))


  # Ensure all parameters are sufficiently long. This feels extravagant, but needed because we're indexing into these variables
  # and we don't want to reach beyond the end
  par_rep <- function(y) rep_len(y, length(mses))

  cols %<>% par_rep
  pchs %<>% par_rep
  bar.cols %<>% par_rep
  ebar.cols %<>% par_rep
  bar.borders %<>% par_rep

  # x may not be the same length as group_data because we're skipping empty groups
  # we still want everything else to be based on group number
  xi <- 1
  for(ii in seq_along(group_data)) {
    if(group_data[[ii]]$has_t) {
      ni <- names(group_data)[ii]

      lines(x[xi] + c(-lsize, lsize), rep(mses[[ni]][1], 2), lwd=3, lend=ebar.lend, col=ebar.cols[ii])

      add_points(x[xi], group_data[[ii]]$trials,
        col=getAlphaRGB(cols[ii], pt.alpha), pch=pchs[ii], jitr_x=jitr_x)

      ebars.y(x[xi], mses[[ni]][1], mses[[ni]][2],
        lwd=ebar.lwds, col=ebar.cols[ii], code=0, lend=ebar.lend)
      xi <- xi+1
    }
  }

  # in case people need to further decorate
  invisible(x)
}

jitr = function(x, len=length(x), r=(1/3)*len) {
  x + runif(len, -r, r)
}

add_points = function(x, y, jitr_x=.2, pch=19, ...) {
  points(jitr(x, length(y), r=jitr_x), y, pch=pch, ...)
}

# ensure data are within some bounds
clip_x <- function(x, lim) {
  x[x<min(lim)] <- min(lim)
  x[x>max(lim)] <- max(lim)

  x
}

# useful for plotting when you want to go a bit beyond the data
stretch <- function(x, pct) {
  d <- pct * diff(range(x))
  c(min(x)-d, max(x)+d)
}


# # # calcluation helpers

# mean +/- se
m_se <- function(x) c('mean'=mean(x), 'se'=se(x))
mat_m_se <- function(m, DIM=2) apply(m, DIM, m_se)

se <- function(x, na.rm=FALSE) sd(x, na.rm=na.rm) / sqrt(sum(not_NA(x)))

#mean +/- sd
m_sd <- function(x, na.rm=FALSE) c('mean'=mean(x,na.rm=na.rm), 'sd'=sd(x,na.rm=na.rm))

not_null <- function(x) !is.null(x)

do_if <- function(boolean_expression, if_clause, else_clause=NULL) {
  if(all(boolean_expression))
    return (if_clause)

  return (else_clause)
}

# easy way to get +/- from a long vector
pm <- function(x,d)c(x-d,x+d)


# make it easier to say not is.na in a pipe'd context
not_NA = function(x) !is.na(x)

# needed to simplify long expressions
colDiff <- function(m, ord=1:2) m[,ord[1]] - m[,ord[2]]

# 0-1 scale the data so we can manage the plot ranges easily
scl01 <- function(x) (x-min(x)) / diff(range(x))

#enforce sum to 1, ignoring NA in the sum, but keeping them in the output
pscl <- function(x) x /sum(x, na.rm=TRUE)

