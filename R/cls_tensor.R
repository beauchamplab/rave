#' Wrapper class for tensor arrays
#' @export
Tensor <- R6::R6Class(
  classname = 'Tensor',
  private = list(
    .data = NULL,
    fst_locked = FALSE
  ),
  public = list(
    dim = NULL,
    dimnames = NULL,
    use_index = F,
    swap_file = '',
    hybrid = F,
    last_used = NULL,
    temporary = TRUE,

    finalize = function(){
      if(self$temporary){
        unlink(self$swap_file)
      }
    },

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
      invisible(self)
    },

    initialize = function(data, dim, dimnames, varnames, hybrid = F, use_index = F, swap_file = tempfile(), temporary = TRUE){
      self$temporary = temporary
      # get attributes of data
      dim %?<-% base::dim(data)
      dim %?<-% length(data)
      if(length(dim) < 2){
        dim = c(dim, 1)
        dim(data) = dim
      }


      dimnames %?<-% base::dimnames(data)
      dimnames %?<-% lapply(1:length(varnames), function(v){ seq_len(dim[v]) })

      names(dimnames) = varnames

      self$last_used = Sys.time()
      self$dimnames = dimnames
      self$dim = dim

      if(hybrid){
        if(use_index){
          data = apply(data, length(dim), as.vector)
          data = as.data.frame(data)
          names(data) = paste0('V', seq_len(ncol(data)))
          write_fst(data, swap_file, compress = 20)
        }else{
          data = data.frame(V1 = as.vector(data))
          write_fst(data, swap_file, compress = 20)
        }
      }else{
        private$.data = data
      }
      self$hybrid = hybrid
      self$use_index = use_index
      self$swap_file = swap_file

      rm(data)


      # if(!missing(dim)){
      #   self$dim = dim
      #   if(!assertthat::are_equal(dim(data), dim)){
      #     logger('Dimension does not match', level = 'WARNING')
      #   }
      # }else if(!is.null(base::dim(data))){
      #   self$dim = base::dim(data)
      # }else{
      #   self$dim = length(data)
      # }
      #
      # if(!missing(dimnames)){
      #   self$dimnames = dimnames
      # }else if(!is.null(base::dimnames(data))){
      #   self$dimnames = base::dimnames(data)
      # }else{
      #   self$dimnames = lapply(1:length(varnames), function(v){
      #     1:(self$dim[v])
      #   })
      # }
      # names(self$dimnames) = varnames
      # # dimnames(data) = self$dimnames
      #
      # private$.data = data
      # self$last_used = Sys.time()
    },
    subset = function(..., drop = FALSE, data_only = F, .env = parent.frame()){
      ..wrapper = list2env(self$dimnames, parent = .env)
      # expr = lapply(lazyeval::lazy_dots(...), function(x){x$env = .env; x})
      # class(expr) <- 'lazy_dots'
      # re = lazyeval::lazy_eval(expr, data = self$dimnames)
      quos = rlang::quos(...)
      re = sapply(quos, function(quo){
        # Use eval_dirty!
        # quo = rlang::quo_set_env(quo, ..wrapper)
        # eval_tidy(quo)
        eval_dirty(quo, env = ..wrapper)
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

      # sub = do.call(`[`, args = c(list(self$data), tmp, list(drop = drop)))
      # if hybrid, then we only load partial file
      if(!is.null(private$.data)){
        sub = do.call(`[`, args = c(alist(private$.data), tmp, list(drop = drop)))
      }else{
        # hybrid
        max_dim = length(self$dim)
        if(self$use_index){
          # we have to load the last index
          if(is.logical(tmp[[max_dim]])){
            tmp[[max_dim]] = which(tmp[[max_dim]])
          }
          load_dim = self$dim; load_dim[max_dim] = length(tmp[[max_dim]])
          sub = as.matrix(fst::read_fst(self$swap_file, columns = paste0('V', tmp[[max_dim]])))
          dim(sub) = load_dim
          tmp[[max_dim]] = seq_along(tmp[[max_dim]])
          sub = do.call(`[`, args = c(alist(sub), tmp, list(drop = drop)))
          dimnames(sub) = sub_dimnames
        }else{
          sub = do.call(`[`, args = c(alist(self$get_data()), tmp, list(drop = drop)))
        }
      }


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
          sub = cl$new(sub, dim = dim(sub), dimnames = sub_dimnames, varnames = names(sub_dimnames))
          return(sub)
        }
      }else{
        sub = Tensor$new(sub, dim = dim(sub), dimnames = sub_dimnames, varnames = names(sub_dimnames))
        return(sub)
      }


    },
    flatten = function(include_index = F, value_name = 'value'){
      nrow = prod(self$dim)
      re = data.frame(V = as.vector(self$get_data()))
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
    },

    # Serialize tensor to a file and store it via write_fst
    to_swap = function(use_index = F, delay = 0){
      if(delay == 0){
        self$to_swap_now(use_index = use_index)
      }else{
        delta = difftime(Sys.time(), self$last_used, units = 'secs')
        if(as.numeric(delta) >= delay){
          # this object might not be in use
          self$to_swap_now(use_index = use_index)
        }
      }
    },
    to_swap_now = function(use_index = F){
      if(!file.exists(self$swap_file)){
        self$swap_file = tempfile()
      }
      swap_file = self$swap_file

      self$hybrid = T
      d = private$.data
      if(is.null(d)){
        return()
      }
      private$.data = NULL
      if(use_index){
        # use the last dim as index
        index = length(self$dim)
        dim(d) = c(prod(self$dim) / self$dim[index], self$dim[index])
      }else{
        dim(d) = NULL
      }
      d = as.data.frame(d)

      write_fst(d, path = swap_file, compress = 20)
      self$use_index = use_index
      self$swap_file = swap_file

    },
    get_data = function(drop = F, gc_delay = 3){
      self$last_used = Sys.time()
      d = NULL
      if(!is.null(private$.data)){
        d = private$.data
      }else if(file.exists(self$swap_file)){
        # load data
        d = as.matrix(fst::read_fst(self$swap_file, as.data.table = F))
        dim(d) = self$dim
        dimnames(d) = self$dimnames
        if(gc_delay > 0){
          private$.data = d
        }
      }
      if(drop && !is.null(d)){
        d = d[drop=T]
      }

      if(self$hybrid){
        if(gc_delay <= 0){
          private$.data = NULL
        }else if(!is.null(private$.data)){
          self$last_used = Sys.time()
          later::later(function(){
            delta = difftime(Sys.time(), self$last_used, units = 'secs')

            if(self$hybrid && file.exists(self$swap_file) && (as.numeric(delta) - gc_delay >= - 0.001)){
              # remove RAM data
              private$.data = NULL
            }
          }, delay = gc_delay)
        }
      }

      return(d)
    },
    set_data = function(v){
      if(private$fst_locked){
        stop('This tensor instance is locked for read-only purpose. Cannot set data!')
      }
      self$last_used = Sys.time()
      private$.data = v
      if(self$hybrid && !is.null(v)){
        self$to_swap_now(use_index = self$use_index)
      }
    },
    collapse = function(keep, method = 'mean'){
      sel = keep %in% seq_along(self$dim)
      if(any(!sel)){
        stop('Argument keep is improper.')
      }
      d = self$get_data()

      if(!is.numeric(d) && !is.complex(d)){
        stop('This tensor is not a numeric tensor')
      }

      if(any(!is.finite(d))){
        logger('Tensor contains NaNs, converting to zeros', level = 'WARNING')
        d[!is.finite(d)] = 0
      }

      f_c = function(d){
        switch (
          method,
          'mean' = {
            d = collapse(d, keep = keep)
            d = d / prod(self$dim[-keep])
          },
          'median' = {
            d = apply(d, keep, median)
          }, {
            d = collapse(d, keep = keep)
          }
        )
        d
      }


      if(is.complex(d)){
        d = f_c(Re(d)) + 1i * f_c(Im(d))
      }else{
        d = f_c(d)
      }



      return(d)
    },
    operate = function(by, fun = .Primitive("/"), match_dim, mem_optimize = F){
      by_vector = as.vector(by)
      if(missing(match_dim)){
        return(self$get_data() / by_vector)
      }
      assertthat::assert_that(
        all(match_dim %in% seq_along(self$dim)),
        sum(abs(self$dim[match_dim] - dim(by))) == 0,
        msg = 'Dimension does not match: self$dim[match_dim] = dim(by) ?'
      )
      rest_dims = seq_along(self$dim)[-match_dim]
      max_dim = length(self$dim)

      if(mem_optimize && self$hybrid && self$use_index && max_dim %in% rest_dims && self$dim[[max_dim]] != 1){
        # This is a special case where we can avoid using too much memories
        rest_dims = rest_dims[rest_dims != max_dim]
        vapply(seq_len(self$dim[[max_dim]]), function(ii){
          sub = fst::read_fst(self$swap_file, as.data.table = F, columns = paste0('V', ii))[[1]]
          dim(sub) = self$dim[-max_dim]
          if(length(rest_dims)){
            perm = c(match_dim, rest_dims)
            sub = fun(aperm(sub, perm), by_vector)
            sub = aperm(sub, order(perm))
          }else{
            sub = fun(sub, by_vector)
          }
          sub
        }, FUN.VALUE = array(0, dim = self$dim[-max_dim])) ->
          re
        return(re)
      }else{
        # general case
        perm = c(match_dim, rest_dims)

        if(any(perm -seq_len(max_dim) != 0)){
          sub = aperm(self$get_data(), perm = perm)
          sub = fun(sub, by_vector)
          sub = aperm(sub, order(perm))
        }else{
          sub = fun(sub, by_vector)
        }
        return(sub)
      }
    }
  ),
  active = list(
    varnames = function(){
      return(names(self$dimnames))
    },
    data = function(v){
      if(missing(v)){
        logger('Tensor$data is deprecated, use Tensor$get_data() instead!', level = 'WARNING')
        # stop('Tensor$data is deprecated, use Tensor$set_data(data) instead!')
        self$get_data()
      }else{
        stop('Tensor$data is deprecated, use Tensor$set_data(data) instead!')
      }
    },
    read_only = function(v){
      if(missing(v)){
        return(private$fst_locked)
      }else{
        private$fst_locked = isTRUE(v)
      }
    }
  )
)

#' Convert tensor to python objects
#' @param x Tensor instance
#' @param convert TRUE to automatically convert Python objects to their R equivalent. Not implemented
#' @export
r_to_py.Tensor <- function(x, convert = FALSE){
  reticulate::r_to_py(c(
    x$dimnames,
    list(
      data = x$get_data()
    )),
    convert = convert)
}

#' @export
dim.Tensor <- function(x){
  x$dim
}

#' @export
dimnames.Tensor <- function(x){
  x$dimnames
}

#' @export
`[.ECoGTensor` <- function(obj, i, j, k, l){
  dim = obj$dim
  if(missing(i)){
    i = 1:dim[1]
  }
  if(missing(j)){
    j = 1:dim[2]
  }
  if(missing(k)){
    k = 1:dim[3]
  }
  if(missing(l)){
    l = 1:dim[4]
  }
  obj$subset(
    Trial = i,
    Frequency = j,
    Time = k,
    Electrode = l,
    drop = F
  )

  #
  #   nd <- obj$data[i,j,k,l, drop = FALSE]
  #   dimnames = obj$dimnames
  #   dimnames[['Trial']] = dimnames[['Trial']][i]
  #   dimnames[['Frequency']] = dimnames[['Frequency']][j]
  #   dimnames[['Time']] = dimnames[['Time']][k]
  #   dimnames[['Electrode']] = dimnames[['Electrode']][l]
  #   ECoGTensor$new(data = nd,
  #                  dim = dim(nd),
  #                  dimnames = dimnames,
  #                  varnames = c('Trial', 'Frequency', 'Time', 'Electrode'))
}


content.Tensor <- function(obj, ...){
  obj$get_data()
}

content <- function(obj, ...){
  UseMethod('content')
}

#' Subset for Tensor Object
#' @param x	object to be subsetted.
#' @param ... further arguments to be passed to or from other methods.
#' @param .env environtment to evaluate in
#' @export
subset.Tensor <- function(x, ..., .env = parent.frame()){
  x$subset(...,.env = .env)
}

#' Convert Tensors to Vector
#' @param x an R object.
#' @param ... passed from or to other methods.
#' @export
as.vector.Tensor <- function(x, ...){
  d = x$get_data()
  base::as.vector(d, ...)
}


