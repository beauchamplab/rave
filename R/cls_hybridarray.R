.global = new.env(parent = emptyenv())
.global$arrays = list()
.global$array_size = function(individual = FALSE){
  if(individual){
    sapply(.global$arrays, lobstr::obj_size)
  }else{
    lobstr::obj_size(.global$arrays)
  }
}
.global$ram_limit = 1024^3

#' @importFrom R6 R6Class
HybridArray <- R6::R6Class(
  classname = 'HybridArray',
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    path = NULL,
    alt_path = NULL,
    attributes = NULL,
    meta_path = NULL,
    partition = 0,
    index = 0,
    data_env = NULL,
    .dim = NULL,
    .dimnames = NULL,
    locked = FALSE,
    length = 0,
    new_file = TRUE,
    n = 0,
    cached = NULL,
    file_format = NULL,
    auto_gc = FALSE,
    data_type = '',
    write_table = function(x, file, ...){
      f = switch (
        private$file_format,
        'fst' = {
          write_table_fst
        },
        'csv' = {
          write_table_dt
        },{
          stop('file format `', private$file_format, '` not supported')
        }
      )
      if(is.null(dim(x))){
        dim(x) = c(length(x), 1)
      }

      if(private$data_type == 'complex'){
        re_f = sub('[/]{0}$', '_re', file)
        y = as.data.frame(apply(x, 2, Re))
        names(y) = paste0('V', seq_len(ncol(y)))
        f(y, re_f, ...)

        im_f = sub('[/]{0}$', '_im', file)
        y = as.data.frame(apply(x, 2, Im))
        names(y) = paste0('V', seq_len(ncol(y)))
        f(y, im_f, ...)
      }else{
        x = as.data.frame(x, row.names = NULL)
        names(x) = paste0('V', seq_len(ncol(x)))
        f(x, file, ...)
      }
    },
    load_table = function(file, columns = NULL){
      f = switch (
        private$file_format,
        'fst' = {
          load_table_fst
        },
        'csv' = {
          load_table_dt
        },{
          stop('file format `', private$file_format, '` not supported')
        }
      )
      if(private$data_type == 'complex'){
        re_f = sub('[/]{0}$', '_re', file)
        im_f = sub('[/]{0}$', '_im', file)
        as.matrix(f(re_f, columns)) + 1i*as.matrix(f(im_f, columns))
      }else{
        f(file, columns)
      }

    }
  ),
  public = list(
    use_partition = TRUE,
    ram_limit = Inf,
    print = function(..., cutoff = 50L){
      cat('Dimension: ', paste(self$get_dim(), collapse = ' x '), sep = '')
      if(!length(private$.dimnames) || !is.list(private$.dimnames)){
        cat('No dimension names.')
      }else{
        cat('\nNames:\n')
        header = c()
        content = c()
        for(ii in seq_along(private$.dimnames)){
          nm = names(private$.dimnames[ii])
          if(!length(nm)) nm = 'NULL'
          v = private$.dimnames[[ii]]
          d = private$.dim[[ii]]
          s = capture.output(utils::str(v, nchar.max = cutoff, give.attr = FALSE))
          header = c(header, sprintf('  %s', nm, d))
          content = c(content, s)
        }

        hls = vapply(header, function(h){
          length(strsplit(h, '')[[1]])
        }, 0)

        hl = max(hls)+1

        for(ii in seq_along(header)){
          s = sprintf('%s%s:%s', header[[ii]], paste(rep(' ', hl - hls[[ii]]), collapse = ''), content[[ii]])
          cat(s, '\n', sep = '')
        }
      }
      return(invisible())
    },
    finalize = function(){
      if(private$auto_gc){
        self$remove(force = TRUE)
      }else{
        if(!private$locked){
          self$swap_out()
        }
        unlink(private$alt_path, recursive = TRUE, force = TRUE)
      }
    },
    reset_changes = function(){
      private$n = 0
      private$data_env= list()
      private$cached = NULL
      if(private$locked){
        unlink(private$alt_path, recursive = TRUE, force = TRUE)
        private$alt_path = tempfile('junk')
      }
    },
    remove_partition = function(partition, replace = NA, force = FALSE, flush = TRUE){
      if(!force && private$locked){
        stop('Data is locked. Use $reset_partition(force=TRUE) to reset partition.')
      }
      if(length(partition) > 1){
        res = sapply(partition, self$reset_partition, replace = replace, force = force, flush = FALSE)
        if(flush && !private$locked && any(res)){
          self$swap_out()
          return(TRUE)
        }
        return(FALSE)
      }
      if(self$initialized){
        if(private$new_file){
          self$swap_out()
        }
        nparts = private$.dim[private$partition]
        if(partition < 1 || nparts > nparts){
          stop('Partition out of index')
        }
        if(!private$locked){
          # unlink
          unlink(file.path(private$path, sprintf('partition_%d', partition)), force = force)
          unlink(file.path(private$alt_path, sprintf('partition_%d', partition)), force = force)
          if(is.na(replace)){
            return(TRUE)
          }
        }

        # need to write new array
        roi = self$get_roi()
        roi[[private$partition]] = partition
        private$n = private$n + 1
        private$data_env[[private$n]] = list(
          data = array(replace, vapply(roi, length, 0)),
          roi = roi
        )

        if(flush){
          self$swap_out()
        }

        return(TRUE)
      }

      return(FALSE)
    },
    remove = function(force = FALSE){
      if(!force && private$locked){
        stop('Data is locked. Use $remove(force=TRUE) to clear data')
      }
      unlink(private$path, recursive = TRUE, force = TRUE)
      unlink(private$alt_path, recursive = TRUE, force = TRUE)
      private$data_env = list()
      private$n = 0
      private$.dim = NULL
      private$.dimnames = NULL
      private$attributes = list()
      private$length = 0
      private$locked = FALSE
      private$cached = NULL
      private$index = private$partition = 0
      private$new_file = TRUE
    },
    get_dim = function(){
      private$.dim
    },
    set_dim = function(dim){
      # internally used only!!!
      if(length(private$.dim) == 0){
        private$.dim = as.vector(dim)
      }else{
        stop('set_dim should be used internally')
      }
    },
    get_dimnames = function(){
      private$.dimnames
    },
    set_dimnames = function(dimnames){
      # check dim and dimnames
      if(!is.list(dimnames)){
        private$.dimnames = NULL
        return(invisible())
      }
      if(!all(vapply(dimnames, is.vector, FUN.VALUE = FALSE))){
        stop('each elements of dimnames should be a vector')
      }
      nms = names(dimnames)
      dim = private$.dim
      if(length(dimnames) != length(dim) || any(dim != vapply(dimnames, length, 0))){
        stop('dim does not match with dimnames')
      }
      private$.dimnames = dimnames
    },
    set_partition_index = function(which.idx){
      if(private$new_file){
        if(length(private$.dim) > 2){
          self$use_partition = TRUE
          private$partition = which.idx
        }
      }else if(private$partition != which.idx){
        stop('Cannot change current partition index from ', private$partition,  ' to ', which.idx)
      }
    },
    get_partition_index = function(){
      private$partition
    },
    stack_print = function(){
      print(str(private$data_env))
      invisible()
    },

    initialize = function(path, read_only = FALSE, format = getOption('hybrid_array.format', default = 'csv')){
      # check path
      if(file.exists(path)){
        info = file.info(path)
        if(!info$isdir){
          stop('path must be a directory')
        }
      }
      # dir.create(path = path, showWarnings = showWarnings, recursive = recursive, mode = mode)
      private$path = path
      private$alt_path = tempfile('junk')
      private$attributes = list()
      private$meta_path = file.path(private$path, '.meta.yaml')
      private$data_env = list()

      # TODO: read meta and set attributes
      if(file.exists(private$meta_path)){
        dat = yaml::read_yaml(private$meta_path, fileEncoding = 'UTF-8')
        for(nm in names(dat)){
          private[[nm]] = dat[[nm]]
        }
        private$new_file = FALSE
        if(read_only){
          self$lock_data()
        }
      }
      if(!length(private$file_format)){
        if(format %in% c('fst', 'csv')){
          private$file_format = format
        }else{
          private$file_format = 'csv'
        }
      }
      .global$arrays[[length(.global$arrays) + 1]] = self
    },

    lock_data = function(){
      private$locked = TRUE
    },
    init_data = function(x, dim, dimnames = NULL, ..., partial = FALSE){
      if(!private$new_file || private$n > 0){
        stop('use add_data')
      }
      if(length(x) == 0){
        return(invisible())
      }
      if(missing(dim)){
        dim = base::dim(x)
      }
      not_match = FALSE

      private$n = private$n + 1
      private$attributes = attributes(x) # might need better way
      private$.dimnames = dimnames
      private$length = length(x)
      self$set_dim(dim)
      print(3)
      if(length(private$.dim) == 0){
        private$.dim = c(private$length, 1)
      }else{
        private$length = prod(private$.dim)
      }
      if(
        length(private$.dim) != length(dim(x)) ||
        any(private$.dim != dim(x))
      ){
        not_match = TRUE
      }
      private$data_type = storage.mode(x)
      if(partial){
        all_roi = self$get_roi(...)

        private$data_env[[private$n]] = list(
          data = array(x, sapply(all_roi, length)),
          roi = all_roi
        )

        self$swap_out()
      }else{
        if(not_match){
          private$data_env[[private$n]] = list(
            data = array(x, private$.dim),
            roi = lapply(private$.dim, seq_len)
          )
        }else{
          private$data_env[[private$n]] = list(
            data = x,
            roi = lapply(private$.dim, seq_len)
          )
        }


        self$check_swap()

      }
    },

    check_swap = function(){
      if(private$n == 0){
        return(invisible())
      }
      if(
        .global$array_size() > .global$ram_limit ||
        self$ram_used > self$ram_limit
      ){
        self$swap_out()
      }
    },
    get_roi = function(..., ..duplicate = FALSE){
      roi = list()
      for(ii in seq_along(private$.dim)){
        idx = tryCatch({
          ...elt(ii)
        }, error = function(e){
          NULL
        })
        if(is.null(idx)){
          idx = seq_len(private$.dim[[ii]])
        }else{
          if(is.logical(idx)){
            if(length(idx) != private$.dim[[ii]]){
              stop(sprintf('Dimension %d not match', ii))
            }
            idx = which(idx)
          }else{
            if(!is.integer(idx)){
              idx = as.integer(idx)
            }
            if(!..duplicate && any(duplicated(idx))){
              stop('Duplicated index in dimension ', ii)
            }
          }
        }
        roi[[ii]] = idx

      }
      roi
    },
    alter_data = function(value, ...){
      if(length(value) == 0){
        return(invisible())
      }
      if(private$new_file && private$n == 0){
        warning('initializing data, calling init_data(value)')
        self$init_data(value)
        return(invisible())
      }
      if(...length() != length(private$.dim)){
        stop('Dimension not match')
      }
      # generate roi
      roi = self$get_roi(...)
      sub_dim = sapply(roi, length)
      print(sub_dim)
      if(is.atomic(value)){
        value = array(value, sub_dim)
      }else if(length(value) != prod(sub_dim)){
        value = array(value, sub_dim)
      }else if(length(dim(value)) != length(sub_dim)){
        dim(value) = sub_dim
      }

      private$n = private$n + 1
      private$data_env[[private$n]] = list(
        data = value,
        roi = roi
      )

      self$check_swap()
    },

    swap_out = function(meta = TRUE){
      use_partition = self$use_partition
      if(private$locked){
        dir.create(private$alt_path, showWarnings = FALSE, recursive = TRUE)
      }
      if(private$n == 0){
        return(invisible())
      }
      dim = private$.dim
      ndim = length(dim); ndim = max(ndim, 1)
      # usually use the last for partition and use the index as columns
      # use index when tensor mode >= 2 and use partition when mode >= 3
      if(ndim < 3){
        use_partition = FALSE
        private$partition = 0
      }
      # decide which dimension to be used as partition index
      if(isTRUE(use_partition) && private$partition == 0){
        private$partition = ndim
        if(private$index == ndim){
          private$partition = ndim - 1
        }
      }
      use_partition = private$partition
      private$index = ndim - (use_partition == ndim)
      use_index = private$index
      if(!dir.exists(private$path)){
        dir.create(private$path, showWarnings = FALSE, recursive = TRUE)
      }
      # two cases


      # case 1 partition and index
      tmp_env = new.env(parent = emptyenv())
      lapply(seq_len(private$n), function(idx){
        roi = private$data_env[[idx]]$roi
        tmp_env$idx = 1

        if(use_partition){
          sub_dim = private$.dim[-use_partition]
          sub_len = prod(sub_dim)
          w_dim = c(sub_len / private$.dim[use_index], private$.dim[use_index])
          apply(private$data_env[[idx]]$data, use_partition, function(subx){
            ii = tmp_env$idx
            tmp_env$idx = ii + 1

            part_idx = roi[[use_partition]][ii]

            fname = file.path(private$path, sprintf('partition_%d', part_idx))
            fname_alt = file.path(private$alt_path, sprintf('partition_%d', part_idx))
            if(private$locked && self$has_partition(part_idx, alt = TRUE)){
              old_data = as.matrix(load_table(fname_alt))
              dim(old_data) = sub_dim
            }else if(self$has_partition(part_idx, alt = FALSE)){
              old_data = as.matrix(load_table(fname))
              dim(old_data) = sub_dim
            }else{
              old_data = array(NA, sub_dim)
            }

            old_data = do.call(`[<-`, c(
              list(
                quote(old_data)
              ),
              roi[-use_partition],
              list(
                value = quote(subx)
              )
            ))
            dim(old_data) = w_dim


            if(private$locked){
              private$write_table(old_data, fname_alt)
            }else{
              private$write_table(old_data, fname)
            }
            NULL
          })
        }else{
          sub_dim = private$.dim
          sub_len = prod(sub_dim)
          w_dim = c(sub_len / private$.dim[use_index], private$.dim[use_index])
          fname = file.path(private$path, sprintf('partition_%d', 1))
          fname_alt = file.path(private$alt_path, sprintf('partition_%d', 1))
          if(private$locked && self$has_partition(part_idx, alt = TRUE)){
            old_data = as.matrix(load_table(fname_alt))
            dim(old_data) = sub_dim
          }else if(self$has_partition(part_idx, alt = FALSE)){
            old_data = as.matrix(load_table(fname))
            dim(old_data) = sub_dim
          }else{
            old_data = array(NA, sub_dim)
          }
          old_data = do.call(`[<-`, c(
            list(
              quote(old_data)
            ), roi,
            list(
              value = quote(private$data)
            )
          ))
          dim(old_data) = w_dim
          if(private$locked){
            private$write_table(old_data, fname_alt)
          }else{
            private$write_table(old_data, fname)
          }
        }
      })


      private$data_env = list()
      private$new_file = FALSE
      private$n = 0

      if(meta){
        self$save_meta()
      }


    },

    save_meta = function(){
      if(!private$locked){
        dir.create(private$path, showWarnings = F, recursive = T)
        metas = c('attributes', 'partition', 'index', '.dim', '.dimnames', 'locked', 'length', 'file_format', 'data_type')
        yaml::write_yaml(
          sapply(metas, function(m){
            private[[m]]
          }, simplify = FALSE, USE.NAMES = TRUE),
          file = private$meta_path,
          fileEncoding = 'UTF-8'
        )

        private$new_file = FALSE
      }

      invisible()
    },

    has_partition = function(part, alt){
      if(private$data_type == 'complex'){
        fname_alt = file.path(private$alt_path, sprintf('partition_%d_re', part))
        fname = file.path(private$path, sprintf('partition_%d_re', part))
      }else{
        fname_alt = file.path(private$alt_path, sprintf('partition_%d', part))
        fname = file.path(private$path, sprintf('partition_%d', part))
      }
      if(alt){
        file.exists(fname_alt)
      }else{
        file.exists(fname)
      }

    },

    get_data = function(..., drop = FALSE){
      if(private$new_file && private$n == 0){
        return(NULL)
      }
      # get index
      # roi = self$get_roi(1,3,4,5)
      roi = self$get_roi(...)
      sub_dim = sapply(roi, length)


      if(private$new_file){
        ii = 2
        dat = private$data_env[[1]]$data
        dat = do.call(`[`, c(
          list(quote(dat)),
          roi,
          list(drop = FALSE)
        ))
      }else{
        # load from files
        columns = roi[[private$index]]
        if(private$partition){
          local_dim = c(private$.dim[-c(private$index, private$partition)], sub_dim[private$index])
          dat = sapply(roi[[private$partition]], function(part_idx){
            fname = file.path(private$path, sprintf('partition_%d', part_idx))
            fname_alt = file.path(private$alt_path, sprintf('partition_%d', part_idx))

            local_roi = roi[-c(private$index, private$partition)]
            local_roi[[length(local_roi) + 1]] = seq_along(roi[[private$index]])

            if(private$locked && self$has_partition(part_idx, alt = TRUE)){
              v = as.matrix(private$load_table(fname_alt, columns))
              dim(v) = local_dim

              do.call(`[`, c(
                list(quote(v)),
                local_roi,
                list(drop=FALSE)
              ))
            }else if(self$has_partition(part_idx, alt = FALSE)){
              v = as.matrix(private$load_table(fname, columns))
              dim(v) = local_dim

              do.call(`[`, c(
                list(quote(v)),
                local_roi,
                list(drop=FALSE)
              ))
            }else{
              array(NA, sapply(local_roi, length))
            }


          })
          dim(dat) = c(sub_dim[-c(private$index, private$partition)],
                       sub_dim[private$index], sub_dim[private$partition])

          if(private$partition != length(private$.dim)){
            dim_order = c(seq_along(private$.dim)[-c(private$index, private$partition)],
                          c(private$index, private$partition))
            dat = aperm(dat, order(dim_order))
          }

        }else{
          fname = file.path(private$path, sprintf('partition_%d', 1))
          fname_alt = file.path(private$alt_path, sprintf('partition_%d', 1))

          columns = roi[[private$index]]
          local_dim = c(private$.dim[-c(private$index)], sub_dim[private$index])
          local_roi = roi[-private$index]
          local_roi[[length(local_roi) + 1]] = seq_along(roi[[private$index]])

          if(private$locked && self$has_partition(part_idx, alt = TRUE)){
            v = as.matrix(private$load_table(fname_alt, columns))
            dim(v) = local_dim

            dat = do.call(`[`, c(
              list(quote(v)),
              local_roi,
              list(drop=FALSE)
            ))
          }else if(self$has_partition(part_idx, alt = FALSE)){
            v = as.matrix(private$load_table(fname, columns))
            dim(v) = local_dim

            dat = do.call(`[`, c(
              list(quote(v)),
              local_roi,
              list(drop=FALSE)
            ))
          }else{
            dat = array(NA, sapply(local_roi, length))
          }
        }
        ii = 1
      }

      private$cached = dat

      if(private$n >= ii){
        lapply(seq(ii, private$n), function(n_idx){
          sub_data = private$data_env[[n_idx]]$data
          sub_roi = private$data_env[[n_idx]]$roi
          subsel_dat = lapply(seq_along(private$.dim), function(idx){
            r = sapply(sub_roi[[idx]], function(.x){
              r = roi[[idx]] == .x
              if(any(r)){
                which(r)
              }else{
                NA
              }
            })
          })
          subsel_sub = lapply(subsel_dat, function(x){!is.na(x)})
          subsel_dat = lapply(subsel_dat, function(x){x[!is.na(x)]})

          if(all(sapply(subsel_dat, length) > 0)){
            if(any(!unlist(subsel_sub))){
              sub_data = do.call(`[`, c(
                list(quote(sub_data)),
                subsel_sub,
                list(drop=FALSE)
              ))
            }


            private$cached = do.call(`[<-`, c(
              list(quote(private$cached)),
              subsel_dat,
              list(value=quote(sub_data))
            ))
          }
        })
      }


      if(private$new_file){

      }
      dat = private$cached
      private$cached = NULL

      if(drop){
        drop(dat)
      }else{
        dat
      }

    },

    set_data = function(v){
      if(self$initialized){
        self$alter_data(v)
      }else{
        self$init_data(v, dim(v), dimnames(v))
      }
      invisible()
    },

    subset = function(..., drop = FALSE, data_only = F, .env = parent.frame()){
      nexpr = ...length()
      if(nexpr == 0){
        return(self$get_data())
      }
      if(!self$is_namedarray){
        stop('This is not a named array. Please set dimnames')
      }
      parent_env = new.env(parent = .env)

      fs = substitute(list(...))
      fs = as.list(fs)[-1]
      dimnames = private$.dimnames

      nms = names(dimnames)

      roi = lapply(dimnames, function(x){ rep(TRUE, length(x)) })

      for(ii in seq_len(nexpr)){
        target = names(fs[ii])
        expr = fs[[ii]]
        if(!is.character(target)){
          if(!length(fs[[ii]]) || as.character(fs[[ii]][[1]]) != '~'){
            stop('cannot parse ', fs[[ii]])
          }
          target = unlist(as.character(fs[[ii]][[2]]))[1]
          expr = fs[[ii]][[3]]
        }
        if(target %in% nms){
          sel = eval(expr, envir = dimnames, enclos = parent_env)
          idx = which(nms == target)
          roi[[idx]] = sel & roi[[idx]]
        }
      }

      roi = lapply(roi, which)
      names(roi) = NULL
      re = do.call(self$get_data, roi)

      if(data_only){
        if(drop){
          return(drop(re))
        }else{
          return(re)
        }
      }else{
        x = HybridArray$new(path = tempfile(pattern = 'hybrid_temp'), read_only = FALSE, format = private$file_format)
        x$set_dim(dim = dim(re))
        if(private$partition){
          x$set_partition_index(private$partition)
        }
        subdimnames = sapply(seq_along(dimnames), function(ii){
          dimnames[[ii]][roi[[ii]]]
        })
        names(subdimnames) = names(dimnames)
        x$init_data(re, dim = dim(re), dimnames = subdimnames)
        x
      }

    },

    set_attr = function(key, val){
      if(key %in% c('length', 'dim')){
        stop('cannot set locked attribute: ', key)
      }
      private$attributes[[key]] = val
      self$save_meta()
    },
    get_attr = function(key, default = NULL){
      if(key %in% names(private$attributes)){
        private$attributes[[key]]
      }else{
        default
      }
    },
    to_swap = function(...){
      warning('to_swap() is soft-deprecated, use swap_out() in the future')
      self$swap_out()
    },
    to_swap_now = function(...){
      warning('to_swap_now() is soft-deprecated, use swap_out() in the future')
      self$swap_out()
    },

    collapse = function(keep, method = 'mean'){

      # TODO: need to optimize memory
      sel = keep %in% seq_along(private$.dim)
      if(any(!sel)){
        stop('keep is improper')
      }
      d = self$get_data()

      if(!is.numeric(d) && !is.complex(d)){
        stop('array is not a numeric, cannot collapse')
      }

      if(any(!is.finite(d))){
        logger('array contains NaNs, converting to 0', level = 'WARNING')
        d[!is.finite(d)] = 0
      }
      collapse = rutabaga::collapse

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
      passed = tryCatch({
        if((
          all(match_dim %in% seq_along(private$.dim)) &&
          sum(abs(private$.dim[match_dim] - dim(by))) == 0
        )){
          TRUE
        }else{
          FALSE
        }
      }, error = function(e){ FALSE }, warning = function(e){ FALSE })

      if(!passed){
        stop('Dimension does not match: self$dim[match_dim] = dim(by) ?')
      }


      rest_dims = seq_along(private$.dim)[-match_dim]
      perm = c(match_dim, rest_dims)

      if(mem_optimize && private$partition %in% rest_dims){

        sub_perm_droped = perm[perm != private$partition]
        sub_perm = c(sub_perm_droped, private$partition)
        need_perm = any(sub_perm_droped - seq_along(private$.dim[-private$partition]) != 0)
        roi = self$get_roi()

        res = lapply(seq_len(private$.dim[private$partition]), function(ii){
          # ii partition
          roi[[private$partition]] = ii
          d = do.call(self$get_data, roi)
          if(need_perm){
            d = aperm(d, perm = sub_perm)
          }
          dim(d) = private$.dim[sub_perm_droped]
          fun(d, by_vector)
        })
        res = vapply(res, function(x){x}, res[[1]])

        if(any(seq_along(sub_perm) - sub_perm != 0)){
          res = aperm(res, order(sub_perm))
        }
        return(res)
      }else{
        # general case
        perm = c(match_dim, rest_dims)

        if(any(perm - seq_along(perm) != 0)){
          sub = aperm(self$get_data(), perm = perm)
          sub = fun(sub, by_vector)
          sub = aperm(sub, order(perm))
        }else{
          sub = fun(self$get_data(), by_vector)
        }
        return(sub)
      }

    }

  ),

  active = list(
    is_hybridarray = function(){ TRUE },
    is_locked = function(){ private$locked },
    ram_used = function(){ lobstr::obj_size(self) },
    initialized = function(){ !(private$new_file && (private$n == 0)) },
    ndims = function(){ length(private$.dim) },
    saved_partitions = function(){
      if(private$partition > 0){
        part = private$.dim[private$partition]
      }else{
        part = private$.dim[length(private$.dim)]
      }
      # check file existence
      if(part == 0){
        return(0)
      }

      # fname = file.path(private$path, sprintf('partition_%d', ))
      # sum(file.exists(fname))
      sum(self$has_partition(seq_len(part), alt = FALSE))
    },
    file_location = function(){
      private$path
    },
    is_namedarray = function(){
      if(is.list(private$.dimnames)){
        nms = names(private$.dimnames)
        if(length(nms) == length(private$.dim) && !'' %in% nms){
          return(TRUE)
        }
      }
      return(FALSE)
    },
    dim = function(v){
      if(missing(v)){
        self$get_dim()
      }else{
        self$set_dim(v)
      }
    },
    dimnames = function(v){
      if(missing(v)){
        self$get_dimnames()
      }else{
        self$set_dimnames(v)
      }
    },
    use_index = function(v){ TRUE },
    swap_file = function(v){ .Deprecated() },
    hybrid = function(v){ TRUE },
    last_used = function(v){ .Deprecated() },
    temporary = function(v){
      if(missing(v)){
        private$auto_gc
      }else{
        private$auto_gc = isTRUE(v)
      }
    },
    read_only = function(){
      private$locked
    },
    varnames = function(){
      names(private$.dimnames)
    }
  )
)

is_hybridarray <- function(x){
  'R6' %in% class(x) && isTRUE(x$is_hybridarray)
}




# IO methods
load_table_fst <- function(file, columns = NULL){
  if(is.null(columns)){
    re = fst::read_fst(path = file)
  }else{
    if(is.logical(columns)){
      columns = which(columns)
    }
    if(!is.integer(columns)){
      warning('columns will be converted to integers')
      columns = as.integer(columns)
    }
    if(any(duplicated(columns))){
      stop('duplicated columns')
    }
    columns = paste0('V', columns)
    re = fst::read_fst(path = file, columns = columns)
  }

}
write_table_fst <- function(x, file, compress = 0, ...){
  fst::write_fst(x, file, compress = compress, ...)
}

load_table_dt <- function(file, columns = NULL){
  re = data.table::fread(file)
  if(!is.null(columns)){
    re = re[, columns]
  }
  re
}
write_table_dt <- function(x, file){
  data.table::fwrite(x, file = file, row.names = FALSE)
}


#' @export
`[.HybridArray` <- function(x, ..., drop = TRUE){

  if(!x$initialized){
    return(NA)
  }
  nargs  =...length()

  ###
  is_formula = FALSE
  if(nargs > 0 && x$is_namedarray){
    is_formula = tryCatch({
      'formula' %in% class(...elt(1))
    }, error = function(e){
      FALSE
    })
  }
  if(is_formula){
    # tidy evaluation
    parent_env = new.env(parent = parent.frame())
    fs = list(...)
    dimnames = x$get_dimnames()
    nms = names(dimnames)
    roi = lapply(dimnames, function(x){ rep(TRUE, length(x)) })
    for(f in fs){
      target = unlist(as.character(f[[2]]))[1]
      if(target %in% nms){
        sel = eval(f[[3]], envir = dimnames, enclos = parent_env)
        idx = which(nms == target)
        roi[[idx]] = sel & roi[[idx]]
      }
    }
    roi = lapply(roi, which)
    names(roi) = NULL
    re = do.call(x$get_data, roi)
  }else{
    if(nargs > 1 && x$ndims != nargs){
      stop('incorrect number of dimensions')
    }
    re = x$get_data(...)
  }

  if(drop){
    drop(re)
  }else{
    re
  }
}

#' @export
`[<-.HybridArray` <- function(x, ..., value){
  if(x$initialized){
    x$alter_data(value = value, ...)
  }else{
    if(missing(..1)){
      x$init_data(value, dim(value), dimnames(value))
    }else{
      x$init_data(value, dim(value), dimnames(value), ..., partial = TRUE)
    }
  }
  x
}

#' @title Create a Hybrid Array
#'
#' @description These functions create hybrid array instances for fast read/write process for
#' arrays with three or more dimensions. When the data is too large for RAM,
#' it's recommended to partition and store data on the local hard disks. Hybrid
#' array partitions the data along one of its dimensions.
#'
#' @param data array or an atomic element
#' @param dim dimension of data
#' @param dimnames \code{NULL} or named list of data dimensions
#' @param path path to store array
#' @param partition_index which dimension to create partition
#'
#' @details
#' When the array is too large for RAM to handle, use \code{hybrid_array_partial}.
#' For example, a 1000 x 1000 x 100 x 100 array could be ~ 80GB which could be
#' too big for a personal laptop to handle in RAM. To solve this problem, we could
#' generate 1000 sub-arrays with dimension 1 x 1000 x 100 x 100, with each ~ 80 MB.
#' To start, we use \code{hybrid_array_partial(..., which_partition=1)} to claim
#' the first dimension to be the partition index, then push sub-arrays. (see
#' example - "partial data")
#'
#' @examples
#' \dontrun{
#' # ------------ Simple in-memory usage ------------
#' data <- rnorm(1e5)
#' x <- hybrid_array(data, c(100, 100, 10))
#' x[]
#'
#' # ------------ partial data example ------------
#' # generate a 10 x 10 x 3 x 100 array x, but only with partial data
#'
#' # the second partition
#' data = array(rnorm(10000), c(10, 10, 1, 100))
#'
#' # x = array(NA, c(10, 10, 3, 100)); x[,,2,] <- data
#' x = hybrid_array_partial(data, dim = c(10,10,3,100), partition_index = 3, which_partition = 2)
#' x[,,2,]
#'
#' # Add more data
#' x[,,3,] <- data + 1
#'
#' # Check, should be all '1'
#' x[1,1,3,] - x[1,1,2,]
#'
#' # ------------ Hybrid example ~ 800MB data ------------
#' data <- rnorm(1e8)
#' x <- hybrid_array(data, c(100, 100, 100, 100))
#' x$ram_used
#' # save to disk, might take a while to write to disk
#' x$swap_out(); x$ram_used
#' x[1,2,1:10,2]
#' }
#'
#' @export
hybrid_array <- function(data = NA, dim = length(data), dimnames = NULL,
                         path = tempfile(pattern = 'hybridarray'), partition_index = NULL){
  x = HybridArray$new(path = path, read_only = FALSE)
  x$set_dim(dim)
  if(length(partition_index) == 1){
    if(partition_index > 0){
      x$set_partition_index(which.idx = partition_index)
    }else{
      x$use_partition = FALSE
    }
  }
  x$init_data(x = data, dim = dim, dimnames = dimnames)
  x
}

#' @rdname hybrid_array
#' @param which_partition which partition should data be when calling \code{hybrid_array_partial}
#' @export
hybrid_array_partial <- function(
  data, dim, which_partition, partition_index = length(dim),
  dimnames = base::dimnames(data),
  path = tempfile(pattern = 'hybridarray')
){
  if(length(dim) <= 2){
    stop('array dimension less than 3 not supported')
  }
  if(partition_index <= 0 || partition_index > length(dim)){
    stop('partition_index should be a index of dimension')
  }
  if(which_partition <= 0 || which_partition > dim[partition_index]){
    stop('which_partition: incorrect number of dimensions')
  }
  x = HybridArray$new(path = path, read_only = FALSE)
  x$set_dim(dim)
  x$set_partition_index(which.idx = partition_index)

  args = lapply(seq_along(dim), function(x){
    if(x == partition_index){
      which_partition
    }else{
      substitute()
    }
  })

  args = c( alist(x = data, dim = dim, dimnames = dimnames, partial = TRUE), args )

  do.call(x$init_data, args)
  x
}


#' @title Load Hybrid Array Stored in Hard Disk
#' @param path directory where hybrid array is stored
#' @examples
#' \dontrun{
#' ## Create an array
#' data <- rnorm(1e5)
#' x <- hybrid_array(data, c(100, 100, 10))
#'
#' # save to disk
#' x$swap_out()
#' path = x$file_location
#'
#' # Load from disk
#' y = load_hybrid_array(path)
#'
#' # check
#' range(y[] - x[]) # should be 0,0
#' }
#' @export
load_hybrid_array <- function(path){
  path = normalizePath(path, mustWork = TRUE)
  HybridArray$new(path = path)
}


#' @export
subset.HybridArray <- function(x, ..., drop = FALSE, data_only = F, .env = parent.frame()){
  x$subset(..., drop = drop, data_only = data_only, .env = .env)
}


#' @export
dimnames.HybridArray <- function(x){
  x$dimnames
}

#' @export
dim.HybridArray <- function(x){
  x$dim
}
