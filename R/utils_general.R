#' Convert file to base64 format
to_datauri <- function(file, mime = ''){
  info = file.info(file)
  ss = jsonlite::base64_enc(input = readBin(file, what = 'raw', n = info$size))
  ss = sprintf('data:%s;base64,%s', mime, ss)
  ss
}
# s = base64enc::dataURI(mime = 'image/*', file = '~/Desktop/Enlight1.jpg', encoding = 'base64')
# stringr::str_sub(s, end = 500)
#
# stringr::str_sub(ss, end = 500)


#' Format Print Strings
#' @export
fprintf <- function(..., collapse = '\n', lookup_env = parent.frame()){
  s = list(...)
  s = paste(sapply(s, as.character), collapse = collapse)
  s = stringr::str_remove_all(s, '\\$\\{\\{[\\ ]*\\}\\}')
  # find something wrapped with ${{...}}
  pattern = '\\$\\{\\{(.+?)\\}\\}'
  var_names = unlist(
    stringr::str_extract_all(s, pattern)
  )
  if(length(var_names)){
    var_names = stringr::str_match(var_names, pattern)[,2]
    var_keys = unique(var_names)
    vars = sapply(var_keys, function(nm){

      re = as.character(rlang::eval_tidy(rlang::parse_expr(nm), env = lookup_env))
      if(length(re) > 1){
        re = paste(re, collapse = ' ')
      }else if(!length(re)){
        re = ''
      }
      re
    }, simplify = F, USE.NAMES = T)
    vars = unlist(vars[var_names])
    s = stringr::str_split(s, pattern, simplify = T)
    # append '' to vars
    vars = c(vars, rep('', length(s) - length(vars)))
    s = as.vector(matrix(c(s, vars), nrow = 2, byrow = T))
    s = paste(s, collapse = '')
  }
  s
}




#' Convert bytes to KB, MB, GB,...
#' @usage to_ram_size(s, kb_to_b = 1000)
#' @export
to_ram_size <- function(s, kb_to_b = 1000){
  base = floor(log(max(abs(s), 1), kb_to_b))
  s = s / (kb_to_b ^ (base))
  if(is.na(base)){
    base = 0
  }
  attr(s, 'unit') = c('B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB')[base+1]
  class(s) = c('rave_bytes', class(s))
  s
}


#' @export
as.character.rave_bytes <- function(s, digit=1){
  sprintf(sprintf('%%.%df %s', digit, attr(s, 'unit')), s)
}

#' @export
print.rave_bytes <- function(s, digit=1){
  re = as.character(s)
  cat(re)
  invisible(re)
}

#' Get max RAM size (experimental)
#' @export
mem_limit <- function(){
  sys_info = Sys.info()
  sys_name = str_to_lower(sys_info['sysname'])
  default_return = list(
    total = NA,
    free = NA
  )
  if(str_detect(sys_name, '^win')){
    # windows
  }
  if(str_detect(sys_name, '^darwin')){
    tryCatch({
      ram_info = system2('sysctl', '-a', wait = T, stdout = T)
      ram_info = ram_info[str_detect(ram_info, 'mem')]
      ram_info = as.numeric(str_extract(ram_info, '[0-9]+$'))

      ram = list(
        total = max(ram_info),
        free = NA
      )
    }, error = function(e){
      default_return
    }) ->
      ram
    return(ram)
  }

  # linux
  tryCatch({
    units = str_to_lower(c('B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'))
    ram_info = system2('cat', '/proc/meminfo', stdout = T)
    ram_info = str_to_lower(ram_info)
    memtotal = ram_info[str_detect(ram_info, '(memtotal)|(memfree):')]
    ram_size = str_match(memtotal, '([0-9]+) ([\\w]b)$')
    ram = as.numeric(ram_size[,2]) * 1024^(-1 + sapply(ram_size[,3], function(x){which(units == x)}))
    list(
      total = max(ram),
      free = min(ram)
    )
  }, error = function(e){
    default_return
  }) ->
    ram
  return(ram)

}

#' @export
color_console <- function(enable = T){
  re = rave_options(crayon_enabled = enable)
  if(re){
    logger('RAVE switched to color console', level = 'INFO')
  }
}

#' @export
time_diff <- function(start, end){
  delta = unclass(end-start)
  list(
    delta = as.numeric(delta),
    units = attr(delta, 'units')
  )
}

#' For each element e1 ind1, find next element e2 in ind2 with e1<e2
#' @export
align_index = function(ind1, ind2, max_lag = 0){
  if(zero_length(ind1, ind2)){
    return(NULL)
  }

  if(max_lag == 0){
    max_lag = max(c(ind1, ind2))
  }

  compare = function(a,b){
    a < b & (b-a) < max_lag
  }

  o = outer(ind1, ind2, compare);o
  i = apply(o, 1, function(x){
    ifelse(sum(x) == 0, 0, min(which(x)))
  })
  sel = i>0
  if(sum(sel)){
    cbind(ind1[sel], ind2[i[sel]])
  }else{
    NULL
  }
}


# to be tested
#' @export
to_color <- function(x, default_length = 1, palette = NULL, shift = 2){
  if(length(x) == 1 && default_length > 1){
    x = rep(x, default_length)
  }
  shift = max(0, shift)

  env = new.env()
  env$text = paste(x)

  if(is.null(palette)){
    # if x is numeric but in factors
    z = x
    if(is.factor(x)){
      tryCatch({
        as.numeric(paste(x))
      }, error = function(e){
        NA
      }, warning = function(e){
        NA
      }) ->
        z
      if(sum(is.na(z))){
        z = x
      }
    }
    if(is.numeric(z)){
      # z is not a integer or improper
      if(sum(z < 1) || sum(abs(z - round(z))) > 1e-4){
        x = paste('X', x)
      }else{
        x = z + shift
      }
    }


    tryCatch({
      col2rgb(x, alpha = 1) / 255
    }, error = function(e){
      x = as.numeric(as.factor(x)) + shift
      col2rgb(x, alpha = 1) / 255
    }) ->
      cols
    env$colors = apply(cols, 2, function(y){
      do.call(rgb, as.list(y))
    })


  }else{
    if(!is.factor(x)){
      x = as.factor(x)
    }
    x = as.numeric(x)
    ncols = length(unique(x))
    if(is.function(palette)){
      palette = palette(ncols)
    }

    assertthat::assert_that(ncols >= length(palette), msg = 'Palette does not have enough length.')
    env$colors = palette[x]
  }

  # generate text
  env$palette = with(env, {
    unique(cbind(text, colors), MARGIN = 1)
  })

  return(as.list(env))

}

crop_data <- function(x, range){
  assertthat::assert_that(length(range) == 2, msg = 'Range must have length 2.')
  minr = min(range)
  maxr = max(range)
  x[x <= minr] = minr
  x[x >= maxr] = maxr
  x
}


test_colors <- function(cols){
  plot(seq_along(cols), col = cols , pch = 20)
}


#' @export
rave_palette <- function(n=1000, one_sided = F, colors = c(
  '#1a237e', '#42b3d5', '#dcedc8', '#ffffff', '#ffecb3', '#e85285', '#6a1b9a'
), width = c(0.5,1,4), alpha = T
){

  width = c(rev(width), width)
  len = seq_along(width)
  nn = round(width / sum(width) * n * (2-(one_sided == 0)))

  cols = lapply(len, function(ii){
    colorRampPalette(colors[ii + c(0,1)], interpolate = 'spline', alpha = alpha)(nn[ii])
  })


  m = length(width) / 2

  if(one_sided == 0){
    cols = c(
      unlist(cols[1:m]), colors[m+1], unlist(cols[-(1:m)])
    )
    return(cols)
  }else if(one_sided > 0){
    cols = c(colors[m+1], unlist(cols[-(1:m)])
    )
    return(cols)
  }else{
    cols = c(unlist(cols[1:m]), colors[m+1])
    return(cols)
  }
}

# test_colors(rave_palette(1001))

#' @export
image_plot = function(z, x, y, crop = NULL, symmetric = F, precision = 1, main = '', sub_titles = NULL, col = rave_palette(), nrow = 1,
                      cex.main = 2, cex.lab = 1.6, cex.axis = 1.4, las = 1, panel.last = NULL, ...){

  miss_x = missing(x)
  old.par = par(no.readonly = TRUE)
  on.exit({
    suppressWarnings(par(old.par))
  })
  mai = old.par$mai

  # if z is a list
  is_crop = length(crop) == 2
  if(!is.list(z)){
    z = list(z)
  }

  z_len = length(z)
  sub_titles %?<-% names(z)
  if(length(sub_titles) != z_len){
    sub_titles = rep('', z_len)
  }
  sub_titles = paste0('\n', sub_titles)
  zlim_actual = range(sapply(z, range))

  if(is_crop){
    z = lapply(z, crop_data, range = crop)
    zlim = range(crop)
  }else{
    zlim = range(sapply(z, range))
  }



  if(symmetric){
    zlim = max(abs(zlim))
    zlim = c(-zlim, zlim)
  }


  layout_matrix = matrix(nrow + c(
    seq_len(z_len),
    seq_len(ceiling(z_len/nrow) * nrow - z_len) + z_len
    ), nrow = nrow)
  layout_matrix = cbind(layout_matrix, seq_len(nrow))

  grid.newpage()
  graphics::layout(layout_matrix, widths = c(rep(1, ncol(layout_matrix) - 1), lcm(3)))

  # Legend first!
  par(mai = c(mai[1], 0, mai[3], 0.8))
  scales = seq(zlim[1], zlim[2], length.out = length(col))

  numeric_format = sprintf('%%.%df', precision)

  replicate(nrow, {
    image(x = 0, y = scales, z = t(scales),
          col = col, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    axis(4, at = c(zlim, 0), labels = sprintf(numeric_format, c(zlim, 0)), las = las, cex.axis = cex.axis)
  })

  title(main = '\n' %&% main, outer = T, cex.main = cex.main, adj = 0)
  sub = "Value Range: [" %&% paste(sprintf(numeric_format, zlim_actual), collapse = ', ') %&% ']'
  title(main = '\n\n' %&% sub, adj = 1, outer = T, cex.main = cex.main * 0.8)

  parent_env = parent.frame()

  pl.fun = function(){}

  body(pl.fun) = substitute(panel.last, env = environment())
  environment(pl.fun) = parent_env


  lapply(seq_len(length(z)), function(ii){

    if(ii %in% (layout_matrix[,1] - nrow)){
      par(mai = c(mai[1], mai[2], mai[3], 0.2))
    }else{
      par(mai = c(mai[1], 0.3, mai[3], 0.2))
    }
    # image(z = z[[ii]], zlim = zlim, col = col, las = las, cex.lab = cex.lab, cex.axis = cex.axis)
    if(!miss_x){
      image(x = x, y = y, z = z[[ii]], zlim = zlim, col = col, las = las, cex.lab = cex.lab,
            cex.axis = cex.axis, cex.main = cex.main * 0.8,
            main = sub_titles[ii], ...)
    }else{
      image(z = z[[ii]], zlim = zlim, las = las, col = col, cex.lab = cex.lab,
            cex.axis = cex.axis, cex.main = cex.main * 0.8, main = sub_titles[ii], ...)
    }

    pl.fun()
  })
  invisible()

}

diff_arg = function(x, srate, freq){
  delta = freq * 2 * pi / srate
  dif = c(delta, diff(x)) - delta
  di = exp(1i * dif)
  da = Arg(di)
  return(da)
}

is.na <- function(x, ...){
  if(!length(x)){
    return(logical(0L))
  }else{
    return(base::is.na(x))
  }
}

############################################### Internal
# utils, will be moved to rutabaga

`set_if_null<-` <- function(x, values) {
  if(is.null(x)) return(values)
  return (x)
}


# Module exec environment
add_to_session <- function(
  session,
  key = 'rave_id',
  val = paste(sample(c(letters, LETTERS, 0:9), 20), collapse = ''),
  override = FALSE
){
  if(!is.null(session)){
    if(override || !exists(key, envir = session$userData)){
      assign(key, val, envir = session$userData)
    }
    return(get(key, envir = session$userData))
  }
  return(NULL)
}

################### Exported methods

#' Concatenate two strings
#' @usage s1 \%&\% s2
#' @examples
#' you <- 'my friend.'
#' print('Hello, ' %&% you)
#' @export
`%&%` <- function(s1,s2) paste0(s1,s2)

#' Evaluate expressions
#' @usage eval_dirty(expr, env = parent,frame(), data = NULL)
#' @details \code{eval_dirty} uses \code{base::eval()} function to evaluate expressions.
#' Compare to \code{rlang::eval_tidy}, which won't affect original environment,
#' \code{eval_dirty} will cause changes to the environment. Therefore if \code{expr}
#' contains assignment, environment will be changed in this case.
#' @examples
#' expr = quote(a <- 111)
#' a = 1; env = globalenv()
#' rlang::eval_tidy(expr, env)
#' print(a)  # Will be 1
#' eval_dirty(expr, env)
#' print(a)  # a is changed
#' @importFrom rlang quo_squash
#' @importFrom rlang is_quosure
#' @export
eval_dirty <- function(expr, env = parent.frame(), data = NULL){

  if(is_quosure(expr)){
    expr = quo_squash(expr)
  }

  if(!is.null(data)){
    return(base::eval(expr, enclos = env, envir = data))
  }else{
    return(base::eval(expr, envir = env))
  }
}

#' Assign if not exists, or NULL
#' @examples
#' # Remove a if exists
#' if(exists('a', envir = globalenv()))  rm(a, envir = globalenv())
#'
#' # Assign
#' a %?<-% 1; print(a)
#'
#' # However, if assigned, nothing happens
#' a = 1;
#' a %?<-% 2;
#' print(a)
#'
#' # in a list
#' a = list()
#' a$e %?<-% 1; print(a$e)
#' a$e %?<-% 2; print(a$e)
#'
#' @importFrom rlang quo
#' @importFrom rlang !!
#' @export
`%?<-%` <- function(lhs, rhs){
  env = parent.frame()
  lhs = substitute(lhs)

  tryCatch({
    is.null(eval(lhs, envir = env))
  }, error = function(e){
    return(TRUE)
  }) ->
    isnull

  if(isnull){
    quo <- quo(!!lhs <- !!rhs)
    eval_dirty(quo, env = env)   # Need to assign values, no eval_tidy
  }
}

#' Check if value is within a data range
#' @usage
#' is_within(x, ref, strict = FALSE)
#' @examples
#' a <- 1:10
#' a[is_within(a, c(2,5))]
#' a[is_within(a, c(2,5), strict=T)]
#' a[is_within(a, 2:5)]
#'
#' a[a %within% 2:5]
#' @export
is_within <- function(x, ref, strict = FALSE){
  rg = range(ref)
  if(strict){
    return(x > rg[1] & x < rg[2])
  }else{
    return(x >= rg[1] & x <= rg[2])
  }
}

#' @rdname is_within
#' @export
`%within%` <- function(x,ref){
  is_within(x,ref)
}

#' Apply each element under "with" clause
#' @usage lapply_expr <- function(X, expr, wrapper = NULL, env = environment())
#' @details The goal of this function is to get rid of ugly "$" operator.
#' X should be a list of lists, for each elements, \code{expr} will be
#' evaluated. You can use the name of the lists directly. If the names are not
#' provided, use \code{.x} as default variable. See examples.
#' @examples
#' # X is a list of named lists, with name of each elements be "a"
#' X <- replicate(n = 3, list(a = rnorm(10)))
#' lapply_expr(X, {mean(a)})
#'
#' # X is a list of unnamed lists, use ".x" as your vairable names
#' X <- replicate(n = 3, list(rnorm(10)))
#' lapply_expr(X, {mean(.x)})
#'
#' # wrapper needs to be a function. It will be applied to the results
#' # before returning the values.
#'
#' # example 1: unlist the result to be a vector
#' X <- replicate(n = 3, list(rnorm(10)))
#' lapply_expr(X, {mean(.x)}, wrapper = unlist)
#'
#' # example 2: wrap up html components using htmltools::tags$ul
#' lapply_expr(1:10, {
#'   htmltools::tags$li(sprintf('line: %d', .x))
#' }, wrapper = htmltools::tags$ul)
#' @importFrom rlang !!
#' @importFrom rlang as_quosure
#' @importFrom rlang eval_tidy
#' @export
lapply_expr <- function(X, expr, wrapper = NULL, env = parent.frame()){
  expr = substitute(expr, env = environment()) # prevent pre-eval of expr
  ..nms = unique(names(X))
  if(length(..nms) != 1 || ..nms == '') ..nms = '.x'
  lapply(X, function(..x){
    if(!is.list(..x)){
      ..x = list(..x)
      names(..x) = ..nms
    }
    eval_tidy(as_quosure(expr, env = env), data = ..x)
  }) ->
    re
  if(is.function(wrapper)){
    re = wrapper(re)
  }
  re
}

#' Evaluate function as if it's run within another environment
#' @usage eval_within(FUN, env = parent.frame(), ..., .args = list(), .tidy = T)
#' @param FUN Function to be evaluated
#' @param env Environment for evaluation
#' @param ...,.args Parameters needed within function
#' @param .tidy Evaluate with side effect? see example
#' @examples
#' # Arbitrary function
#' f <- function(a){b <- a*a; print(b); b}
#'
#' # environment for evaluation
#' env <- new.env()
#' a = 'This is Invalid.'
#' env$a = 11  # a*a is only valid within env
#'
#' # Case 1: evaluate f with no side effect
#' result <- eval_within(f, env = env, .tidy = T)
#' cat('Result:', result, '\nenv$a: ', env$a, '\nenv$b:', env$b)
#'
#' # Case 2: evaluate f with no side effect, but different "a"
#' result <- eval_within(f, env = env, a = 100, .tidy = T)
#' cat('Result:', result, '\nenv$a: ', env$a, '\nenv$b:', env$b)
#'
#' # Case 3: evaluate f with side effect
#' result <- eval_within(f, env = env, a = 20, .tidy = F)
#' cat('Result:', result, '\nenv$a: ', env$a, '\nenv$b:', env$b)
#'
#' @importFrom rlang fn_body
#' @importFrom rlang quo
#' @importFrom rlang eval_tidy
#' @export
eval_within <- function(FUN, env = parent.frame(), ..., .args = list(), .tidy = F){
  args = c(.args, list(...))
  if(is.null(env)){
    return(do.call(FUN, args = args))
  }else{
    expr = fn_body(FUN)
    quo = quo(!!expr)
    if(.tidy){
      eval_tidy(quo, data = args, env = env)
    }else{
      if(length(args) == 0){
        args = NULL
      }else{
        list2env(args, env)
      }
      eval_dirty(quo, env = env)
    }
  }
}

#' Function to clear all elements within environment
#' @usage clear_env(env, all.names = T)
#' @examples
#' env = new.env()
#' env$a = 1
#' print(as.list(env))
#'
#' clear_env(env)
#' print(as.list(env))
#' @export
clear_env <- function(env, all.names = T){
  if(is.environment(env)){
    rm(list = names(as.list(env, all.names = all.names)), envir = env)
  }
}


#' Check if an object is blank string ""
#' @export
is.blank <- function(s){
  s == ''
}

#' Check if value(s) is invalid
#' @usage is_invalid(x, any = F, .invalids = c('null', 'na'))
#' @param x Values to check
#' @param any If TRUE, then it will check if any element in x is invalid,
#' otherwise, it will check if all element of x is invalid
#' @param .invalids Possible choices: 'null', 'na', 'blank'
#' @examples
#' is_invalid(NULL)
#'
#' is_invalid(c(NA, 1))
#'
#' is_invalid(c(NA, 1), any = T)
#'
#' is_invalid('', .invalids = 'blank')
#' @export
is_invalid <- function(x, any = F, .invalids = c('null', 'na')){
  if('null' %in% .invalids){
    if(is.null(x) || !length(x)){
      return(TRUE)
    }
  }
  for(func in paste0('is.', .invalids)){
    res = do.call(func, args = list(x))
    if(length(res) > 1){
      if(any){
        res = any(res)
      }else{
        res = all(res)
      }
    }
    if(res){
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Get value, if value is invalid, then assign value
#' @usage get_val(x, key = NULL, ..., .invalids = c('null', 'na'))
#' @param x List or variable
#' @param key If not NULL, \code{x[[key]]} will be evaluated
#' @param ... Default value to be returned if x or x$key is invalid
#' @param .invalids See ?is_invalid
#' @export
get_val <- function(x, key = NULL, ..., .invalids = c('null', 'na')){

  if(is.null(key)){
    val = x
  }else{
    val = x[[key]]
  }
  if(is_invalid(val, .invalids = .invalids)){
    args = list(...)
    len = length(args)
    if(len){
      if(len == 1){
        val = args[[1]]
      }else{
        val = args
      }
    }
  }
  return(val)
}

#' Calculate if length of input(s) is zero
#' @usage zero_length(..., any = T, na.rm = F)
#' @param ... Element(s) to be evaluated in length
#' @param any Any element has zero-length? or all elements need to have zero-length
#' @param na.rm Should NA be removed before evaluation?
#' @examples
#' # any = TRUE, any element with zero length will yield "TRUE" result
#' # In this case, expressions c(1) and a==NULL are not evaluated
#' zero_length(NULL, c(1), a=={Sys.sleep(10)})
#'
#' # any = FALSE, if any one has non-zero length, return FALSE
#' # Notice that in these two cases, "Sys.sleep(10)" is not evaluated
#' zero_length(NULL, c(1), a=={Sys.sleep(10)}, any = F)
#'
#' # stop('') yields error, which will be counted as invalid/zero-length
#' zero_length(stop(''))
#' @export
zero_length <- function(..., any = T, na.rm = F){
  parent_env = parent.frame()
  args = as.list(match.call())[-1]
  len = length(args)
  if('any' %in% names(args)){
    len = len - 1
    args = args[1:len]
  }
  reNull = function(...){return(NULL)}
  for(i in 1:len){
    tryCatch({
      obj = eval(args[[i]], envir = parent_env)
      if(na.rm == TRUE || na.rm == i){
        obj = obj[!is.na(obj)]
      }
      obj
    }, error = reNull) ->
      obj
    if(any && length(obj) == 0){
      return(TRUE)
    }
    if(!any && length(obj) > 0){
      return(FALSE)
    }
  }
  return(!any)
}


#' Drop nulls within lists/vectors
#' @examples
#' x <- list(NULL,NULL,1,2)
#' dropNulls(x)
#' @export
dropNulls <- function (x, .invalids = c('null')) {
  x[!vapply(x, is_invalid, FUN.VALUE = logical(1), .invalids = .invalids)]
}

#' Convert to string and never goes wrong
#' @usage safe_str_c(x, sep = '', collapse = NULL, .error = '')
#' @param .error If x can't be converted to string, return this message
#' @examples
#' safe_str_c('Count - ', 3:1)
#'
#' safe_str_c('Count - ', 3:1, collapse = '..., ')
#'
#' # aaa doesn't exist
#' safe_str_c('Count - ', aaa, .error = 'aaa not exists')
#'
#' # aaa exists
#' aaa <- 0
#' safe_str_c('Count - ', aaa, .error = 'aaa not exists')
#' @export
safe_str_c <- function(..., sep = '', collapse = NULL, .error = ''){
  tryCatch({
    args = dropNulls(list(...))
    if(length(args)){
      return(stringr::str_c(..., sep = sep, collapse = collapse))
    }else{
      return('NULL')
    }
  }, error = function(e){
    return(.error)
  })
}


#' Try to find absolute path without error
#' @usage try_normalizePath(path, sep = c('/', '\\\\'))
#' @details It's always good to use "/" to separate path. I haven't tested
#' on windows, but this function should work. Basically this function uses
#' base::normalizePath. However base::normalizePath returns error if file
#' does not exist. try_normalizePath will check parent directories and try to
#' find absolute path for parent directories.
#' @examples
#' # "./" exist
#' try_normalizePath('./')
#'
#' # Case when path not exist
#' try_normalizePath("./this/path/does/not/exist/")
#' @importFrom stringr str_split
#' @export
try_normalizePath <- function(path, sep = c('/', '\\\\')){
  if(file.exists(path)){
    path = base::normalizePath(path)
    attr(path, 'is_absolute') = TRUE
    return(path)
  }else{
    # if dirname = itself
    dirname = dirname(path)
    if(dirname == path){
      attr(path, 'is_absolute') = FALSE
      return(path)
    }else{
      pre = try_normalizePath(dirname, sep = sep)

      p = unlist(str_split(path, pattern = paste(sprintf('(%s)', sep), collapse = '|')))
      fname = tail(p, 1)

      path = file.path(pre, fname, fsep = '/')
      attr(path, 'is_absolute') = attr(pre, 'is_absolute')
      return(path)
    }
  }
}


safe_object_size <- function(obj, env = NULL){
  if(is.character(obj) && !is.null(env)){
    obj = get(obj, envir = env, inherits = F)
  }
  tryCatch({
    pryr::object_size(obj)},
    error = function(e){
      return(0L)
    })->
    re
  re
}


#' Cache object
#' @usage cache(key, val, global = FALSE, swap = FALSE, file = tempfile(), name = 'data')
#' @param key Any R object, a named list would be the best.
#' @param val Value to cache, if key exists, then value will not be evaluated nor saved
#' @param global option for shiny app, where if global, then the the cache will ignore sessions.
#' @param swap When object size is too large, do you want to save it to local disk?
#' @param file,name If you use swap=T, see \code{\link{save_h5}}
#' @seealso \code{\link{clear_cache}}
#' @examples
#' \dontrun{
#' cache('a', 1) # returns 1
#' cache('a', 2) # still returns 1
#'
#' # clear cache
#' clear_cache()
#' cache('a', 2) # Now returns 2
#'
#' # Not run because a is cached
#' cache('a', 2)
#' cache('a', {Sys.sleep(10); 1})
#'
#' # Use swap
#'
#' y = cache('aa', 1:1000000, swap = T)
#' pryr::object_size(1:1000000)
#' pryr::object_size(y)
#' y[1:5]
#' }
#' @importFrom digest digest
#' @export
cache <- function(key, val, global = FALSE, replace = FALSE, session = NULL, swap = FALSE, file = tempfile(), name = 'data'){
  if(global){
    session = NULL
  }else{
    session %?<-% getDefaultReactiveDomain()
  }
  cache_env = getDefaultCacheEnvironment(session = session)

  k = digest(key)
  if(replace){
    cache_env[[k]] <- val
  }else{
    cache_env[[k]] %?<-% val
  }

  if(swap && any(
    is.matrix(cache_env[[k]]),
    is.array(cache_env[[k]]),
    is.vector(cache_env[[k]])
    ) &&
    is.numeric(cache_env[[k]])
  ){
    f = file
    name = 'junk'
    save_h5(cache_env[[k]], f, name = name, chunk = NULL, replace = T, new_file = T, level = 0)
    cache_env[[k]] = load_h5(f, name = name)
  }

  return(cache_env[[k]])
}


#' @title Clear cache
#' @seealso \code{\link{cache}}
#' @usage clear_cache(all = FALSE)
#' @param all Clear all cache? Don't turn it on in shiny app. This is for debug use.
#' @export
clear_cache <- function(all = FALSE, session = NULL){
  session %?<-% getDefaultReactiveDomain()
  cache_env = getDefaultCacheEnvironment(session = session)
  clear_env(cache_env)
  if(all){
    cache_env = getDefaultCacheEnvironment(session = NULL)
    clear_env(cache_env)
  }
}



#' @export
getDefaultCacheEnvironment <- function(
  session = getDefaultReactiveDomain()
){
  data_env = getDefaultDataRepository(session = session, session_based = T)
  data_env$.cache_env %?<-% new.env(parent = baseenv())
  data_env$.cache_env$.keys = c()
  return(data_env$.cache_env)
}

################################################### High performance functions

#' lapply using future package (async)
#' @usage lapply_async(x, fun, ..., .ncores = 0,
#'     .future_plan = future::multiprocess, .call_back = NULL,
#'     .packages = NULL, .globals = TRUE,
#'     .gc = TRUE)
#' @param x,fun,... (See ?lapply)
#' @param .ncores Number of cores to use. If the value is 0, the number of cores
#' will be determined by rave_options('max_worker').
#' @param .call_back A function takes current iteration number as argument, can be NULL.
#' @param .packages NULL be default, then the function will detect attached packages
#' automatically. Otherwise you have to specify the packages that you want to load.
#' @param .globals Automatically detect variables. See ?future::future
#' @param .gc Clean up environment after each iterations? Recommended for large datasets.
#' @examples
#' lapply_async(1:10, function(x){
#'   Sys.sleep(2) # Run for 1 secs
#'   Sys.getpid()
#' }, .ncores = 3, .call_back = function(i){
#'   cat('Running iteration -', i, '\n')
#' })
#' @importFrom future plan
#' @importFrom future future
#' @importFrom future value
#' @importFrom future values
#' @export
lapply_async <- function(x, fun, ..., .ncores = 0, .future_plan = future::multiprocess,
                         .call_back = NULL, .packages = NULL, .envir = environment(), .globals = TRUE, .gc = TRUE){
  # compatible with windows
  if(stringr::str_detect(Sys.info()['sysname'], '^[wW]in')){
    args = list(...)
    return(lapply(seq_along(x), function(ii){
      if(is.function(.call_back)){
        try({
          .call_back(ii)
        })
        do.call(fun, c(
          list(x[ii]),
          args
        ), envir = .envir)
      }
    }))
  }

  .ncores = as.integer(.ncores)
  if(.ncores <= 0){
    .ncores = rave_options('max_worker')
  }
  if(is.null(.packages)){
    .packages = stringr::str_match(search(), 'package:(.*)')
    .packages = .packages[,2]
    .packages = rev(.packages[!is.na(.packages)])
  }
  .niter = length(x)
  .ncores = min(.ncores, .niter)

  .future_list = list()
  .future_values = list()

  .i = 0
  while(.i < .niter){
    .i = .i+1
    .x = x[[.i]]

    if(is.function(.call_back)){
      try({
        .call_back(.i)
      })
    }

    .future_list[[length(.future_list) + 1]] = future::future({
      fun(.x)
    }, envir = .envir, substitute = T, lazy = F, globals = .globals, .packages = .packages, gc = .gc,
    evaluator = .future_plan, workers = .ncores)


    if(length(.future_list) >= .ncores){
      # wait for one of futures resolved
      .future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
      .future_list[[1]] = NULL
    }

  }

  return(c(.future_values, future::values(.future_list)))
}



#' @export
restart_rave <- function(reload = T, quiet = FALSE){
  unloadns = function(ns_){
    ns = ns_
    if(isNamespaceLoaded(ns)){
      ns = asNamespace(ns)
      sub_ns = getNamespaceUsers(ns)
      for(sbns in sub_ns){
        unloadns(sbns)
      }
      if(!quiet){
        base::message("Unload namespace - ", ns_)
      }
      unloadNamespace(ns_)
    }
  }

  unloadns('rave')

  cmd = ''
  if(reload){
    cmd = 'base::library(rave)'
  }

  rstudioapi::restartSession(cmd)
}
