#
# functions to make it easier to use base plots
#


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

round_range <- function(x) {
    c(floor(min(x)), ceiling(max(x)))
}

get_data_range <- function(ll, range_var='range') {
  unlist(lapply(ll, getElement, range_var)) %>% range(na.rm=TRUE) #%>% round_range
}

# barplot function that uses all the rave sizes and colors
rave_barplot <- function(height, cex.axis=rave_cex.axis, cex.lab=rave_cex.lab, cex.names=rave_cex.lab, ...) {
  barplot(height, cex.axis=cex.axis, cex.lab=cex.lab, cex.names=cex.names, las=1, ...)
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


# We're getting some extreme values (way beyond 6SD) so let's trim them out
trim <- function(x, cutoff=6) {
  xmed <- median(x)
  z <- abs(x - xmed) / mad(x, center=xmed)
  x[z <= cutoff]
}

trimmed.mean <- function(x, cutoff=4) {
  mean(trim(x, cutoff))
}

trimmed.mse <- function(x, cutoff=4) {
  m_se(trim(x,cutoff))
}


#mean +/- sd
m_sd <- function(x, na.rm=FALSE) c('mean'=mean(x,na.rm=na.rm), 'sd'=sd(x,na.rm=na.rm))

not_null <- function(x) !is.null(x)


# this is primarily used for clauses with side effects (plotting etc)
do_if <- function(boolean_expression, if_clause, else_clause=NULL) {
  if(all(boolean_expression))
    return (if_clause)

  return (else_clause)
}

# easy way to get +/- from a long vector
pm <- function(x,d)c(x-d,x+d)
plus_minus <- function(x,d)c(x-d,x+d)


# make it easier to say not is.na in a pipe'd context
not_NA = function(x) !is.na(x)

# needed to simplify long expressions
colDiff <- function(m, ord=1:2) m[,ord[1]] - m[,ord[2]]

# 0-1 scale the data so we can manage the plot ranges easily
scl01 <- function(x) (x-min(x)) / diff(range(x))

#enforce sum to 1, ignoring NA in the sum, but keeping them in the output
pscl <- function(x) x /sum(x, na.rm=TRUE)







# # # Formatters for statistics


# helper function to build value labels
format_stat <- function(nm, stats=c('b', 't', 'p')) {
    sapply(stats, function(stat) sprintf('%s(%s)', stat, nm), USE.NAMES = FALSE)
}

get_f <- function(formula, data) {
    format_f(lm(formula, data))
}

format_f <-  function(lm.mod, test_name='All') {
    nms <- sapply(c('Rsq(%s)', 'F(%s)', 'p(%s)'), sprintf, test_name)

    with(summary(lm.mod), {
        c(r.squared, fstatistic[1],
          pf(fstatistic[1], fstatistic[2], fstatistic[3], lower.tail=FALSE))
    }) %>% set_names(nms) %>% `class<-`('fres')
}

# relying on a generic here
pretty.fres <- function(fres) {
    # don't save intermediate results back into fres or else it changes the type into character,
    # messing up following lines
    c(
        # R2
        ifelse(fres[1] < 0.01, '<0.01', round(fres[1],2)),
        #F stat
        ifelse(fres[2] < 0.01, '<0.01', round(fres[2],1)),
        #p value
        format(fres[3], digits=1)
    ) %>% `class<-`(c('fres', 'character'))
}

# helper function for t-tests that returns the values wanted by format_stat
get_t <- function(...) with(t.test(...), c(estimate, statistic, p.value)) %>% `class<-`('tres')

pretty.tres <- function(tres) {
    mapply(format, tres, digits=c(2,2,1)) %>%
        set_names(c('m', 't', 'p')) %>% `class<-`(c('tres', 'character'))
}

# these often won't look pretty unless they are used with pretty
# e.g., title(main=as.title(pretty(get_t(...))))
as.title <- function(res, ...) {
    UseMethod('as.title')
}

as.title.fres <- function(res, ...) {
    bquote(H[0] ~ mu[i] == mu[j] * ';' ~ R^2 == .(res[1]) ~ ',' ~ F == .(res[2]) * ','~ p==.(res[3]))
}

as.title.tres <- function(res,...) {
    bquote(H[0] * ':' ~ mu == 0 * ';' ~ bar(x)==.(res[1]) * ',' ~ t == .(res[2]) * ',' ~ p==.(res[3]))
}
