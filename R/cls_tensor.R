
#' @import R6
#' @export
Tensor <- R6::R6Class(
  classname = 'Tensor',
  public = list(
    dim = NULL,
    dimnames = NULL,
    data = NULL,

    print = function(...){
      cat('Dimension: ', paste(sprintf('%d', self$dim), collapse = ' x '), '\n')

      if(length(self$dimnames) > 0){
        lapply(self$dimnames, function(x){
          s = paste(x, collapse = ', ')
          if(stringr::str_length(s) > 20){
            s = paste0(stringr::str_sub(s, end = 17), '...')
          }
          s
        }) ->
          a
        for(x in 1:length(a)){
          cat('- ', names(a)[x], ': ', a[[x]], '\n', sep = '')
        }
      }
    },

    initialize = function(data, dim, dimnames, varnames){

      if(!missing(dim)){
        self$dim = dim
        if(!assertthat::are_equal(dim(data), dim)){
          logger('Dimension does not match', level = 'WARNING')
        }
      }else if(!is.null(base::dim(data))){
        self$dim = base::dim(data)
      }else{
        self$dim = length(data)
      }

      if(!missing(dimnames)){
        self$dimnames = dimnames
      }else if(!is.null(base::dimnames(data))){
        self$dimnames = base::dimnames(data)
      }else{
        self$dimnames = lapply(1:length(varnames), function(v){
          1:(self$dim[v])
        })
      }
      names(self$dimnames) = varnames
      dimnames(data) = self$dimnames

      self$data = data
    },
    subset = function(..., drop = FALSE, data_only = F, .env = parent.frame()){
      ..wrapper = list2env(self$dimnames, parent = .env)
      assign('sss', ..wrapper, envir = globalenv())
      # expr = lapply(lazyeval::lazy_dots(...), function(x){x$env = .env; x})
      # class(expr) <- 'lazy_dots'
      # re = lazyeval::lazy_eval(expr, data = self$dimnames)
      quos = rlang::quos(...)
      re = sapply(quos, function(quo){
        quo = rlang::quo_set_env(quo, ..wrapper)
        eval_tidy(quo)
      }, simplify = F, USE.NAMES = T)

      dims = self$dim
      varnames = names(self$dimnames)

      tmp = self$dimnames; tmp = lapply(tmp, function(x){rep(T, length(x))})
      sub_dimnames = self$dimnames

      for(i in 1:length(re)){
        if(!names(re)[i] %in% varnames){
          n = varnames[length(re[[i]]) == dims]
          if(length(n) == 0){
            next
          }else if(length(n) > 1){
            logger('Varname not specified', level = "WARNING")
            n = n[1]
          }

          names(re)[i] = n
        }else{
          n = names(re)[i]
        }
        tmp[[n]] = re[[i]]
        sub_dimnames[[n]] = sub_dimnames[[n]][re[[i]]]
      }
      if(drop){
        for(n in names(sub_dimnames)){
          if(length(sub_dimnames[[n]]) <= 1){
            sub_dimnames[[n]] <- NULL
          }
        }
      }

      sub = do.call(`[`, args = c(list(self$data), tmp, list(drop = drop)))
      if(data_only){
        return(sub)
      }
      # get class
      cls = class(self);

      sapply(cls, function(cln){
        tryCatch({
          cl = get(cln, mode = 'environment')
          if(cl$classname == 'Tensor' && R6::is.R6Class(cl)){
            return(TRUE)
          }
          if(cl$get_inherit()$classname %in% cls && R6::is.R6Class(cl)){
            return(TRUE)
          }
          return(FALSE)
        },
        error = function(e){
          return(FALSE)
        }, quiet = TRUE)
      }) ->
        is_r6
      cls = cls[is_r6]

      if('Tensor' %in% cls){
        for(cln in cls){
          cl = get(cln, mode = 'environment')
          sub = cl$new(sub, dim = dim(sub), dimnames = sub_dimnames, varnames = names(dimnames(sub)))
          return(sub)
        }
      }else{
        sub = rave:::Tensor$new(sub, dim = dim(sub), dimnames = sub_dimnames, varnames = names(dimnames(sub)))
        return(sub)
      }


    },
    flatten = function(include_index = F, value_name = 'value'){
      nrow = prod(self$dim)
      re = data.frame(V = as.vector(self$data))
      names(re) = value_name
      if(include_index){
        for(i in 1:length(self$varnames)){
          vn = self$varnames[i]
          if(i > 1){
            each = prod(self$dim[1: (i - 1)])
          }else{
            each = 1
          }
          times = nrow / self$dim[i] / each

          re[[vn]] = rep(self$dimnames[[i]], each = each, times = times)
        }
        re = cbind(re[-1], re[1])
      }
      re
    }
  ),
  active = list(
    varnames = function(){
      return(names(self$dimnames))
    }
  )
)


#' @export
r_to_py.Tensor <- function(obj, convert = FALSE){
  reticulate::r_to_py(c(
    obj$dimnames,
    list(
      data = obj$data
    )),
    convert = convert)
}
