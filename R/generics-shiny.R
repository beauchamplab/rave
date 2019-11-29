`print.rave-palettes` <- function(x, plot=FALSE, dark, ...){
  ctx = rave_context()
  cat('RAVE palettes in "', paste(x$themes, collapse = '", "'), '" mode', 
      ifelse(length(x$themes) > 1, 's', ''), ' for "', 
      paste(x$types, collapse = '", "'), '" variables\n', sep = '')
  npal = length(x$palettes)
  cat('Total ', npal, ' palettes:\n', sep = '')
  lapply(x$palettes, function(p){
    cat('  ', p$name, ' (', length(p$colors), ' unique colors)\n', sep = '')
    NULL
  })
  max_col = max(sapply(x$palettes, function(p){ length(p$colors) }))
  if( plot || ctx$context == 'rave_module_debug' ){
    if(missing(dark)){
      dark = 'dark' %in% x$themes
    }
    mfrow = par('mfrow')
    bg = par('bg')
    fg = par('fg')
    on.exit(par(mfrow = mfrow, bg = bg, fg = fg), add = TRUE)
    par(mfrow = c(1, 1))
    if( dark ){
      par(bg = '#1A1A1A', fg = 'white')
    }else{
      par(bg = 'white', fg = 'black')
    }
    font_col = ifelse(dark, 'white', 'black')
    plot(c(0, npal*2+1), c(0, max_col + 1), type = 'n', 
         main = 'Color Palettes', axes = FALSE, cex.main = 2,
         xlab = '', ylab = '', col.main = font_col)
    axis(1, line = NA, at = seq_len(npal)*2-0.5, labels = seq_len(npal), 
         lwd = 0, cex.axis = 1.6, col.axis = font_col)
    
    draw_cols_cont = function(p, idx){
      y = seq_len(max_col)
      rect(xleft = idx-0.5, xright = idx+0.5, ybottom = y-0.5, ytop = y+0.5,
           col = p$colors, border = NA)
    }
    draw_cols_disc = function(p, idx){
      y = seq_along(p$colors)
      points(rep(idx, length(y)), y, col = p$colors, pch = 16)
    }
    
    lapply(seq_along(x$palettes), function(ii){
      p = x$palettes[[ii]]
      draw_cols_cont(p, ii*2-1)
      draw_cols_disc(p, ii*2)
    })
  }
  invisible(x)
}

.get_rave_theme <- function(
  packages = NULL, type = 'continuous', theme
){
  if(missing(theme)){
    theme = rave_options('default_theme')
    if(is.null(theme)){
      theme = 'light'
    }
  }
  stopifnot2(length(theme)==1 && theme %in% c('light', 'dark'),
             msg = 'theme must be either light or dark')
  stopifnot2(all(type %in% c('continuous', 'discrete')),
             msg = 'type must be either continuous or discrete, or both')
  packages = unique(c(packages, 'rave'))
  packages = packages[dipsaus::package_installed(packages)]
  
  # for each one of them, get yaml
  pals = lapply(packages, function(pkg){
    rave_context('rave_module_debug')
    .__rave_package__. = pkg
    pal_yaml = get_path('inst/palettes.yaml')
    if(length(pal_yaml) && file.exists(pal_yaml)){
      pal = yaml::read_yaml(pal_yaml)
      re = lapply(pal, function(p){
        p = as.list(p)
        if(any(type %in% p$type) && 
           any(theme %in% p$theme) && 
           any(type %in% p$type)){
          p$package = pkg
          return(p)
        }
        return(NULL)
      })
      re = dipsaus::drop_nulls(re)
      if(!length(re)){ re = NULL }else{ names(re) = NULL }
      re
    }else{
      NULL
    }
  })
  pals = unlist(pals, recursive = FALSE)
  
  # Find duplicated palettes
  nms = sapply(pals, '[[', 'name')
  pals = lapply(seq_along(nms), function(ii){
    p = pals[[ii]]
    p
  })
  names(pals) = sprintf('%d. %s', seq_along(nms), nms)
  re = list(
    names = names(pals),
    types = type,
    themes = theme,
    packages = packages,
    palettes = pals
  )
  class(re) <- c('rave-palettes', 'list')
  re
}

#' @title Get and Set 'RAVE' Themes
#' @export
get_rave_theme <- rave_context_generics('get_rave_theme', .get_rave_theme)

#' @export
get_rave_theme.default <- .get_rave_theme

#' @export
get_rave_theme.rave_module_debug <- function(packages = NULL, ...){
  ctx = rave_context()
  packages = c(ctx$package, packages)
  .get_rave_theme(packages, ...)
}

#' @export
get_rave_theme.rave_running <- function(packages = NULL, type = 'continuous',
                                        theme){
  ctx = rave_context()
  if(missing(theme)){
    session = shiny::getDefaultReactiveDomain()
    theme = session$userData$rave_theme
    theme %?<-% rave_options('default_theme')
    theme %?<-% 'light'
  }
  packages = c(ctx$package, packages)
  .get_rave_theme(packages, type, theme)
}

#' @export
get_rave_theme.rave_running_local <- get_rave_theme.rave_running



#' @export
set_rave_theme <- function(theme, .set_default = FALSE){
  ctx = rave_context()
  default_theme = rave_options('default_theme')
  if(missing(theme)){
    theme = rave_options('default_theme')
    if(is.null(theme) || !theme %in% c('light', 'dark')){
      theme = 'light'
    }
  }
  theme = match.arg(theme, c('light', 'dark'), several.ok = FALSE)
  
  if(!isTRUE(theme == default_theme) && .set_default){
    rave_options('default_theme' = theme)
  }
  
  session = shiny::getDefaultReactiveDomain()
  if(ctx$context == 'rave_running'){
    session$userData$rave_theme = theme
  }
  if(!is.null(session)){
    if( theme == 'light' ){
      shinyjs::removeClass(class = 'rave-dark', selector = 'body')
      shinyjs::addClass(class = 'rave-light', selector = 'body')
    }else{
      shinyjs::removeClass(class = 'rave-light', selector = 'body')
      shinyjs::addClass(class = 'rave-dark', selector = 'body')
    }
  }
  if(theme == 'light'){
    par(bg = 'white', fg = 'black', col = 'black', col.axis = 'black',
        col.main = 'black', col.lab = 'black', col.sub = 'black')
  }else{
    par(bg = '#1A1A1A', fg = 'white', col = 'white', col.axis = 'white',
        col.main = 'white', col.lab = 'white', col.sub = 'white')
  }
  return(theme)
}
