#' @import parallel
#' @import stringr
#' @export
ECoGRepository <- R6::R6Class(
  classname = 'ECoGRepository',
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    fast_baseline = function(from, to, electrode,
                             wrapper = TRUE, print.time = FALSE){
      assertthat::assert_that(
        self$is_epoched(electrodes = electrode),
        msg = 'Electrode not loaded, run .$epoch() first.'
      )
      t = Sys.time()
      srate = self$subject$sample_rate
      from = round(from * srate)
      to = round(to * srate)
      epoch = self$epochs$get('epoch_data')
      epoch$start = round(epoch$Onset * srate) + from
      # epoch$end = round(epoch$Onset * srate) + to
      epoch$end = epoch$start + (to - from)
      e = self$get_electrode(electrode, c('raw', 'power'))

      ###FIXME
      bls = e$power$data[,,1] * 0
      for(b in unique(epoch$Block)){
        sel = epoch$Block == b
        sub = epoch[sel, ]
        n = sum(sel)

        #NB: need to go one back in the cumsum vector
        #FIXME will it ever be the case sub$start-1 <= 0 ?
        slice = e$raw$get_data(block_num = b, time_point = c(sub$start-1, sub$end), name = 'cumsum')

        # if we take time 10:50, there are 1 + (50 - 10) time points
        bls[sel, ] = t(slice[,-c(1:n)] - slice[,1:n])/(1 + to - from)
      }

      # bld <- 100 * vapply(1:dim(e$power$data)[3], function(ii) {
      #   (e$power$data[,,ii] - bls) / bls
      # }, bls)

      bld = (e$power$data / as.vector(bls) - 1) * 100

      if(wrapper){
        dim(bld) = c(e$power$dim, 1)
        bld = ECoGTensor$new(
          data = bld,
          dimnames = c(e$power$dimnames, list(Electrode = electrode)),
          varnames = c(names(e$power$dimnames), 'Electrode')
        )
      }
      if(print.time){
        total_time = Sys.time() - t
        logger(sprintf('Baseline - Electrode (%d) Total time: %.3f (%s)', electrode, total_time, attr(total_time, 'units')))
      }
      return(bld)
    }
  ),
  public = list(
    subject = NULL,
    raw = NULL,
    power = NULL,
    phase = NULL,
    epochs = NULL,
    print = function(...){
      id = self$subject$subject_id
      last_epoch = self$power$get('last_epoch')
      epoch_name = self$power$get('epoch_name')
      epoch_type = self$power$get('epoch_type')
      cat('<ECoG Repository> [', id, ']\n', sep = '')
    },
    initialize = function(subject, autoload = T){
      self$raw = Map$new()
      self$power = Map$new()
      self$phase = Map$new()
      self$epochs = Map$new()
      if(R6::is.R6(subject) && 'Subject' %in% class(subject)){
        self$subject = subject
      }else{
        assertthat::assert_that('character' %in% class(subject),
                                msg = 'Param <subject> needs to be either subject ID or a Subject instance')
        self$subject = Subject$new(subject_id = subject)
      }
      if(autoload){
        self$load_electrodes()
      }
    },
    get_electrode = function(electrode, name = c('raw', 'power', 'phase')){
      e_str = as.character(electrode)
      re = lapply(name, function(nm){
        self[[nm]]$get(e_str)
      })
      names(re) = name
      return(re)
    },
    load_electrodes = function(electrodes){
      if(missing(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = electrodes[electrodes %in% self$subject$valid_electrodes]
      }
      electrodes = electrodes[!paste(electrodes) %in% self$raw$keys()]

      subject_id = self$subject$subject_id

      if(length(electrodes) > 0){
        logger('Loading electrodes')
        progress = progress(title = 'Checking data...', max = length(electrodes))
        for(e in electrodes){
          e_str = paste(e)
          progress$inc(sprintf('Electrode - %s', e_str))
          self$raw$set(key = e_str, value = Electrode$new(subject = self$subject, electrode = e))
        }
        logger('Loaded.')
        progress$close()
      }
    },
    epoch = function(epoch_name, pre, post, electrodes = NULL, names = c('power')){
      if(is.null(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = electrodes[electrodes %in% self$subject$valid_electrodes]
      }
      electrodes = electrodes[paste(electrodes) %in% self$raw$keys()]
      assertthat::assert_that(length(electrodes) > 0, msg = 'No electrode loaded.')

      digest = digest::digest(list(
        epoch_name, electrodes, pre, post
      ))
      if(self$epochs$contains('signature') && self$epochs$get('signature') == digest){
        return(NULL)
      }

      self$epochs$set('epoch_name', epoch_name)
      self$epochs$set('epoch_params', c(pre, post))
      self$epochs$set('epoch_data', rave:::load_meta(
        meta_type = 'epoch',
        subject_id = self$subject$subject_id,
        meta_name = epoch_name
      ))
      self$epochs$set('electrodes', electrodes)
      self$epochs$set('signature', digest)
      freqs = load_meta(subject_id = self$subject$subject_id, meta_type = 'frequencies')

      progress = progress(title = 'Loading data...', max = length(electrodes)+1)
      on.exit({progress$close()})

      epoch_data = self$epochs$get('epoch_data')
      rave_setup()
      for(e in electrodes){
        e_str = str_c(e)
        progress$inc(sprintf('Electrode - %s', e_str))
        raw_e = self$get_electrode(electrode = e, name = 'raw')$raw
        for(name in names){
          future::futureAssign(
            e_str, {
              rm(list = ls(all.names = T));
              raw_e$fast_epoch(
                epochs = epoch_data,
                freqs = freqs,
                pre = pre, post = post, name = name
              ) -> elc;
              rhdf5::H5close();
              return(elc)
            }, assign.env = self[[name]]$private$env
          )
        }
      }

      # lapply(electrodes, function(e){
      #
      #   e_str = str_c(e)
      #   progress$inc(sprintf('Electrode - %s', e_str))
      #   raw_e = self$get_electrode(electrode = e, name = 'raw')$raw
      #   for(name in names){
      #     self[[name]]$set(
      #       key = e_str,
      #       value = raw_e$fast_epoch(
      #         epochs = self$epochs$get('epoch_data'),
      #         freqs = freqs,
      #         pre = pre, post = post, name = name
      #       )
      #     )
      #   }
      #
      # })


      # bind electrodes
      progress$inc('Finalizing...')

      for(name in names){
        sample = self[[name]]$get(as.character(electrodes[1]))
        vapply(electrodes, function(e){
          e_str = as.character(e)
          self[[name]]$get(e_str)$data
        }, FUN.VALUE = sample$data) ->
          data

        self[[name]]$set(
          key = name,
          value = ECoGTensor$new(
            data = data,
            dimnames = c(sample$dimnames, list(Electrode = electrodes)),
            varnames = c(names(sample$dimnames), 'Electrode')
          )
        )
      }

    },
    is_epoched = function(electrodes, pre, post){
      passed = TRUE
      electrodes = electrodes[!electrodes %in% self$epochs$get('electrodes')]
      if(length(electrodes) > 1){
        logger(paste(electrodes, collapse = ', '), ' are not epoched.', level = 'WARNING')
        passed = FALSE
      }
      if(!missing(pre)){
        epoch_params = self$epochs$get('epoch_params')
        if(epoch_params[1] != pre || epoch_params[2] != post){
          logger('Time range does not match', level = 'WARNING')
          passed = FALSE
        }
      }

      return(passed)
    },
    apply_by_electrode = function(func, ..., electrodes = NULL, name = 'raw', parallel = FALSE, cl = NULL){
      if(length(electrodes) == 0){
        electrodes = self$subject$valid_electrodes
      }

      if(length(electrodes) == 0){
        return(NULL)
      }

      electrodes = lapply(electrodes, function(e){
        re = self$get_electrode(e, name = name)
        re$number = e
        return(re)
      })
      if(parallel){
        if(is.null(cl)){
          cl = parallel::makeForkCluster(nnodes = min(rave_options('max_worker'), parallel::detectCores()))
          on.exit({parallel::stopCluster(cl)})
        }

        return(parallel::parLapply(cl = cl, X = electrodes, fun = func, ...))
      }else{
        return(lapply(electrodes, func, ...))
      }

    },
    baseline = function(from, to, electrodes = NULL, print.time = FALSE){
      if(length(electrodes) == 0){
        electrodes = self$epochs$get('electrodes')
      }else{
        electrodes = self$subject$filter_valid_electrodes(electrodes)
      }
      if(length(electrodes) == 1){
        return(
          private$fast_baseline(from = from, to = to, electrode = electrodes, wrapper = TRUE, print.time = print.time)
        )
      }

      Sys.time() -> t


      sample = self$get_electrode(electrodes[1], c('power'))$power
      vapply(
        electrodes,
        function(e){
          suppressMessages(
            private$fast_baseline(from = from, to = to, electrode = e, wrapper = F)
          )
        },
        FUN.VALUE = sample$data
      ) ->
        d
      d = ECoGTensor$new(
        data = d,
        dimnames = c(sample$dimnames, list(Electrode = electrodes)),
        varnames = c(names(sample$dimnames), 'Electrode')
      )

      if(print.time){
        total_time = Sys.time() - t
        logger(sprintf('Baseline Total time: %.3f (%s)', total_time, attr(total_time, 'units')))
      }
      return(d)

    }
  )
)


