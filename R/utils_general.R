get_os <- function(){
  os = R.version$os
  if(stringr::str_detect(os, '^darwin')){
    return('darwin')
  }
  if(stringr::str_detect(os, '^linux')){
    return('linux')
  }
  if(stringr::str_detect(os, '^solaris')){
    return('solaris')
  }
  if(stringr::str_detect(os, '^win')){
    return('windows')
  }
  return('unknown')
}


get_ram <- function(){
  os = get_os()
  ram = 128*1024^3
  safe_ram = function(e){
    suppressWarnings({
      min(memory.limit(), 128*1024^3)
    })
  }

  ram = tryCatch({
    switch (
      os,
      'darwin' = {
        ram = substring(system("sysctl hw.memsize", intern = TRUE), 13)
      },
      'linux' = {
        ram = system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE)
        ram = as.numeric(ram) * 1024
      },
      'solaris' = {
        ram = system("prtconf | grep Memory", intern = TRUE)
        ram = stringr::str_trim(ram)
        ram = stringr::str_split(ram, '[ ]+')[[1]][3:4]

        power = match(ram[2], c("kB", "MB", "GB", "TB", "Kilobytes", "Megabytes", "Gigabytes", "Terabytes"))
        ram = as.numeric(ram[1]) * 1024^(1 + (power-1) %% 4)
      },
      'windows' = {
        ram = system("wmic MemoryChip get Capacity", intern = TRUE)[-1]
        ram = stringr::str_trim(ram)
        ram = ram[nchar(ram) > 0]
        ram = sum(as.numeric(ram))
      }, {
        ram = min(memory.limit(), 128*1024^3)
      }
    )
    ram
  }, error = safe_ram, warning = safe_ram)
  ram = as.numeric(ram)
  ram
}


get_ncores <- function(){
  as.numeric(future::availableCores())
}

get_cpu <- function(){
  os = get_os()

  safe_cpu = function(...){
    list(
      vendor_id = NA,
      model_name = NA
    )
  }
  cpu = tryCatch({
    switch (
      os,
      'darwin' = list(
        vendor_id = system("sysctl -n machdep.cpu.vendor", intern = TRUE),
        model_name = system("sysctl -n machdep.cpu.brand_string", intern = TRUE)
      ),
      'linux' = list(
        vendor_id = gsub("vendor_id\t: ", "", unique(system("awk '/vendor_id/' /proc/cpuinfo", intern = TRUE))),
        model_name = gsub("model name\t: ", "", unique(system("awk '/model name/' /proc/cpuinfo", intern = TRUE)))
      ),
      'windows' = list(
        model_name = system("wmic cpu get name", intern = TRUE)[2],
        vendor_id = system("wmic cpu get manufacturer", intern = TRUE)[2]
      ),
      list(
        vendor_id = NA,
        model_name = NA
      )
    )
  }, error = safe_cpu, warning = safe_cpu)
  cpu
}





# Convert file to base64 format
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
#' @param ... characters
#' @param collapse character to collapse characters
#' @param lookup_env which environment to look for data?
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
#'
#' @param s size
#' @param kb_to_b 1KB=nB, n=1000 by default
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
as.character.rave_bytes <- function(x, digit=1, ...){
  sprintf(sprintf('%%.%df %s', digit, attr(x, 'unit')), x)
}

#' @export
print.rave_bytes <- function(x, digit=1, ...){
  re = as.character(x, digit = digit, ...)
  cat(re)
  invisible(re)
}

#' Get max RAM size (experimental)
#' @export
mem_limit <- function(){

  total = get_ram()


  bit = 8 * .Machine$sizeof.pointer
  bit = bit - (bit > 32) * 4 - 4
  free = sum(gc()[,1] * c(bit, 8))

  list(
    total = total,
    free = total - free
  )


}

#' Enable RAVE color console
#' @param enable logical, enable or not
#' @export
color_console <- function(enable = T){
  re = rave_options(crayon_enabled = enable)
  if(re){
    logger('RAVE switched to color console', level = 'INFO')
  }
}

time_diff <- function(start, end){
  delta = unclass(end-start)
  list(
    delta = as.numeric(delta),
    units = attr(delta, 'units')
  )
}

#' For each element e1 ind1, find next element e2 in ind2 with e1<e2
#' @param ind1,ind2 two index arrays
#' @param max_lag if positive, e1 < e2 < e1+max_lag
#' @export
align_index <- function(ind1, ind2, max_lag = 0){
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


# TODO: Test this function
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

    assert_that(ncols >= length(palette), msg = 'Palette does not have enough length.')
    env$colors = palette[x]
  }

  # generate text
  env$palette = with(env, {
    unique(cbind(text, colors), MARGIN = 1)
  })

  return(as.list(env))

}

crop_data <- function(x, range){
  assert_that(length(range) == 2, msg = 'Range must have length 2.')
  minr = min(range)
  maxr = max(range)
  x[x <= minr] = minr
  x[x >= maxr] = maxr
  x
}


test_colors <- function(cols){
  plot(seq_along(cols), col = cols , pch = 20)
}


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

# TODO: Questioning
image_plot <- function(z, x, y, crop = NULL, symmetric = F, precision = 1, main = '', sub_titles = NULL, col = rave_palette(), nrow = 1,
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

  title(main = paste0('\n', main), outer = T, cex.main = cex.main, adj = 0)
  sub = paste0("Value Range: [", paste(sprintf(numeric_format, zlim_actual), collapse = ', '), ']')
  title(main = paste0('\n\n', sub), adj = 1, outer = T, cex.main = cex.main * 0.8)

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

`set_if_null<-` <- function(x, value) {
  if(is.null(x)) return(value)
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

paste_c <- function(x, y){
  base::paste0(x, y)
}


`%&%` = paste_c

#' Evaluate expressions
#'
#' @param expr R expression or rlang quo
#' @param env environment to evaluate
#' @param data dataframe or list
#'
#' @details \code{eval_dirty} uses \code{base::eval()} function to evaluate expressions.
#' Compare to \code{rlang::eval_tidy}, which won't affect original environment,
#' \code{eval_dirty} will cause changes to the environment. Therefore if \code{expr}
#' contains assignment, environment will be changed in this case.
#' @examples
#' \dontrun{
#' expr = quote(a <- 111)
#' a = 1; env = globalenv()
#' rlang::eval_tidy(expr, env)
#' print(a)  # Will be 1
#' eval_dirty(expr, env)
#' print(a)  # a is changed
#' }
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
#'
#' @param lhs an object to check or assign
#' @param value value to be assigned if lhs is NULL
#'
#' @examples
#' \dontrun{
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
#' }
#'
#' @export
`%?<-%` <- function(lhs, value){
  env = parent.frame()
  lhs = substitute(lhs)

  tryCatch({
    is.null(eval(lhs, envir = env))
  }, error = function(e){
    return(TRUE)
  }) ->
    isnull

  if(isnull){
    # quo <- quo(!!lhs <- !!value)
    quo <- quo(do.call('=', list(quote(!!lhs), !!value)))
    eval_dirty(quo, env = env)   # Need to assign values, no eval_tidy
  }
}

is_within <- function(x, ref, strict = FALSE){
  rg = range(ref)
  if(strict){
    return(x > rg[1] & x < rg[2])
  }else{
    return(x >= rg[1] & x <= rg[2])
  }
}

`%within%` <- function(x,ref){
  is_within(x,ref)
}


#' Evaluate function as if it's run within another environment
#' @param FUN Function to be evaluated
#' @param env Environment for evaluation
#' @param ...,.args Parameters needed within function
#' @param .tidy Evaluate with side effect? see example
#' @examples
#' \dontrun{
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
#' }
#'
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
#'
#' @param env environment to clean
#' @param all.names clear all variables?
#'
#' @examples
#' \dontrun{
#' env = new.env()
#' env$a = 1
#' print(as.list(env))
#'
#' clear_env(env)
#' print(as.list(env))
#' }
#' @export
clear_env <- function(env, all.names = T){
  if(is.environment(env)){
    rm(list = names(as.list(env, all.names = all.names)), envir = env)
  }
}


#' Check if an object is blank string ""
#' @param x vector of characters
#' @export
is.blank <- function(x){
  x == ''
}

#' Check if value(s) is invalid
#' @param x Values to check
#' @param any If TRUE, then it will check if any element in x is invalid,
#' otherwise, it will check if all element of x is invalid
#' @param .invalids Possible choices: 'null', 'na', 'blank'
#' @examples
#' \dontrun{
#' is_invalid(NULL)
#'
#' is_invalid(c(NA, 1))
#'
#' is_invalid(c(NA, 1), any = T)
#'
#' is_invalid('', .invalids = 'blank')
#' }
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
#' @param ... Element(s) to be evaluated in length
#' @param any Any element has zero-length? or all elements need to have zero-length
#' @param na.rm Should NA be removed before evaluation?
#' @examples
#' \dontrun{
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
#' }
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
#'
#' @param x list
#' @param .invalids 'null', 'na', 'blank' default is null
#'
#' @examples
#' \dontrun{
#' x <- list(NULL,NULL,1,2)
#' dropNulls(x)
#' }
#' @export
dropNulls <- function (x, .invalids = c('null')) {
  x[!vapply(x, is_invalid, FUN.VALUE = logical(1), .invalids = .invalids)]
}

#' Convert to string and never goes wrong
#' @param ... characters to concatenate
#' @param sep,collapse see stringr::str_c
#' @param .error If x can't be converted to string, return this message
#' @examples
#' \dontrun{
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
#' }
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


# Try to find absolute path without error
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
    object_size(obj)},
    error = function(e){
      return(0L)
    })->
    re
  re
}


#' Cache object
#' @param key Any R object, a named list would be the best.
#' @param val Value to cache, if key exists, then value will not be evaluated nor saved
#' @param global option for shiny app, where if global, then the the cache will ignore sessions.
#' @param replace Force replace cache?
#' @param session internally used
#' @param swap Save to swap? usually when val is a large matrix or vector
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
#' object.size(1:1000000)
#' object.size(y)
#' y[1:5]
#' }
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
#' @param all Clear all cache? Don't turn it on in shiny app. This is for debug use.
#' @param session internally used
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


#' Get Cache Environment
#' @param session internally used
#' @export
getDefaultCacheEnvironment <- function(
  session = getDefaultReactiveDomain()
){
  session_id = add_to_session(session)
  session_id %?<-% '.TEMP'
  global_env = globalenv()
  if(!is.environment(global_env[['.cache_rave']])){
    global_env[['.cache_rave']] = new.env(parent = emptyenv())
  }
  global_env[['.cache_rave']][[session_id]] %?<-% new.env(parent = emptyenv())
  return(global_env[['.cache_rave']][[session_id]])
}

################################################### High performance functions

#' lapply using future package (async)
#' @param x,fun,... (See ?lapply)
#' @param .ncores Number of cores to use. If the value is 0, the number of cores
#' will be determined by rave_options('max_worker').
#' @param .call_back A function takes current iteration number as argument, can be NULL.
#' @param .packages NULL be default, then the function will detect attached packages
#' automatically. Otherwise you have to specify the packages that you want to load.
#' @param .globals Automatically detect variables. See ?future::future
#' @param .gc Clean up environment after each iterations? Recommended for large datasets.
#' @param .envir intrnally used
#' @examples
#' \dontrun{
#' lapply_async(1:10, function(x){
#'   Sys.sleep(2) # Run for 1 secs
#'   Sys.getpid()
#' }, .ncores = 3, .call_back = function(i){
#'   cat('Running iteration -', i, '\n')
#' })
#' }
#' @export
lapply_async <- function(x, fun, ..., .ncores = 0,
                         .call_back = NULL, .packages = NULL, .envir = environment(), .globals = TRUE, .gc = TRUE){
  # compatible with windows
  args = list(...)
  if(stringr::str_detect(Sys.info()['sysname'], '^[wW]in')){
    return(lapply(seq_along(x), function(ii){
      if(is.function(.call_back)){
        try({
          .call_back(ii)
        })
        do.call(fun, c( list(quote(x[ii])), args), envir = .envir)
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


  rave_setup_workers(.ncores, use_fork = TRUE)
  if(.ncores != rave_options('max_worker')){
    on.exit({
      rave_setup_workers()
    })
  }


  .future_list = list()
  .future_values = list()


  if(.niter == 0){
    return(list())
  }

  .this_env = environment()


  lapply(seq_along(x), function(.i){
    if(is.function(.call_back)){
      try({
        .call_back(.i)
      })
    }

    expr = rlang::quo_squash(rlang::quo({ do.call(fun, c(list(quote(x[[!!.i]])), args)) }))

    .this_env$.future_list[[length(.future_list) + 1]] = future::future(expr, envir = .envir, substitute = FALSE, lazy = FALSE, globals = .globals, .packages = .packages, gc = .gc)


    if(length(.future_list) >= .ncores){
      # wait for one of futures resolved
      .this_env$.future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
      .this_env$.future_list[[1]] = NULL
    }
  })

  return(c(.future_values, future::values(.future_list)))
}


# to be removed

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

  eval(quote(rm(list = ls(all.names = T, envir = globalenv()), envir = globalenv())))
  eval(parse(text = sprintf('rstudioapi::restartSession(%s)', cmd)))
}
