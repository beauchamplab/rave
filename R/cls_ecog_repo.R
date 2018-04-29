#' @import parallel
#' @import stringr
#' @export
ECoGRepository <- R6::R6Class(
  classname = 'ECoGRepository',
  portable = FALSE,
  cloneable = FALSE,
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
    get_electrode = function(electrode, name = 'raw'){
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
    epoch = function(epoch_name, pre, post, electrodes = NULL, func = NULL, names = c('power')){
      if(is.null(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = electrodes[electrodes %in% self$subject$valid_electrodes]
      }
      electrodes = electrodes[paste(electrodes) %in% self$raw$keys()]
      assertthat::assert_that(length(electrodes) > 0, msg = 'No electrode loaded.')

      digest = digest::digest(list(
        epoch_name, electrodes, pre, post, func
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

      progress = progress(title = 'Loading data...', max = (length(electrodes) + 1) * length(names))
      on.exit({progress$close()})

      epoch_data = self$epochs$get('epoch_data')

      .raws = self$raw
      re = list()

      for(name in names){
        lapply_async(electrodes, function(e){
          e_str = str_c(e)
          raw_e = .raws$get(e_str)

          raw_e$fast_epoch(
            epochs = epoch_data,
            freqs = freqs,
            pre = pre, post = post, name = name
          ) -> elc;
          rhdf5::H5close();

          if(is.function(func)){
            elc = func(elc)
          }
          return(elc)
        }, .call_back = function(i){
          progress$inc(sprintf('Electrode - %d (%s)', electrodes[i], name))
        }) ->
          results

        progress$inc('Finalizing...')



        if(!is.function(func)){
          env = environment()
          sample = results[[1]]$data[,,,1]
          .dim = dim(sample)
          .dimnames = dimnames(sample)

          self[[name]]$set(
            key = name,
            value = ECoGTensor$new(
              data = vapply(rep(1, length(results)), function(i){
                data = env$results[[1]]$data
                env$results[[1]] = NULL
                dimnames(data) = NULL
                return(data)
              }, FUN.VALUE = sample),
              dimnames = c(.dimnames, list(Electrode = electrodes)),
              varnames = c(names(.dimnames), 'Electrode')
            )
          )
        }else{
          re[[name]] = results
        }
      }


      return(re)

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


      # for(name in names){
      #   sample = self[[name]]$get(as.character(electrodes[1]))
      #   vapply(electrodes, function(e){
      #     e_str = as.character(e)
      #     self[[name]]$get(e_str)$data
      #   }, FUN.VALUE = sample$data) ->
      #     data
      #
      #   self[[name]]$set(
      #     key = name,
      #     value = ECoGTensor$new(
      #       data = data,
      #       dimnames = c(sample$dimnames, list(Electrode = electrodes)),
      #       varnames = c(names(sample$dimnames), 'Electrode')
      #     )
      #   )
      # }

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
    baseline = function(from, to, electrodes, print.time = FALSE){
      if(missing(electrodes)){
        electrodes = self$epochs$get('electrodes')
      }else{
        electrodes = self$subject$filter_valid_electrodes(electrodes)
      }
      if(length(electrodes) == 1){
        return(
          self$..baseline(from = from, to = to, electrode = electrodes, data_only = FALSE, print.time = print.time)
        )
      }

      Sys.time() -> t


      sample = self$power$get('power')$subset(Electrode = Electrode == electrodes[1], data_only = T)
      .dimnames = dimnames(sample)[1:3]
      vapply(
        electrodes,
        function(e){
          self$..baseline(from = from, to = to, electrode = e, data_only = T, re_dim = F)
        },
        FUN.VALUE = sample[,,,1]
      ) ->
        d
      d = ECoGTensor$new(
        data = d,
        dimnames = c(.dimnames, list(Electrode = electrodes)),
        varnames = c(names(.dimnames), 'Electrode')
      )

      if(print.time){
        total_time = Sys.time() - t
        logger(sprintf('Baseline Total time: %.3f (%s)', total_time, attr(total_time, 'units')))
      }
      return(d)

    },
    ..baseline = function(from, to, electrode, e_power = NULL,
                          print.time = FALSE, data_only = F, re_dim = T){
      assertthat::assert_that(
        self$is_epoched(electrodes = electrode),
        msg = 'Electrode not loaded, run .$epoch() first.'
      )
      t = Sys.time()
      srate = self$subject$sample_rate
      from = round(from * srate)
      to = round(to * srate)
      epoch = self$epochs$get('epoch_data')
      epoch$start = round(epoch$Time * srate) + from
      # epoch$end = round(epoch$Time * srate) + to
      epoch$end = epoch$start + (to - from)

      if(is.null(e_power)){
        e_raw = self$raw$get(str_c(electrode))
        e_power = self$power$get('power')$subset(Electrode = Electrode == electrode, data_only = T)
      }else{
        electrode = dimnames(e_power)$Electrode
        e_raw = self$raw$get(str_c(electrode))
        if(is(e_power, 'Tensor')){
          e_power = e_power$data
        }
      }

      ###FIXME
      bls_dim = dim(e_power)
      bls_dimnames = dimnames(e_power)
      bls = array(0, dim = bls_dim[c(1,2)])
      for(b in unique(epoch$Block)){
        sel = epoch$Block == b
        sub = epoch[sel, ]
        n = sum(sel)

        #NB: need to go one back in the cumsum vector
        #FIXME will it ever be the case sub$start-1 <= 0 ?
        slice = e_raw$get_data(block_num = b, time_point = c(sub$start-1, sub$end), name = 'cumsum')

        # if we take time 10:50, there are 1 + (50 - 10) time points
        bls[sel, ] = t(slice[,-c(1:n)] - slice[,1:n])/(1 + to - from)
      }

      bld = (e_power / as.vector(bls) - 1) * 100

      if(re_dim){
        dim(bld) = bls_dim
        dimnames(bld) = bls_dimnames
      }
      if(!data_only){
        bld = ECoGTensor$new(
          data = bld,
          dimnames = bls_dimnames,
          varnames = names(bls_dimnames)
        )
      }
      if(print.time){
        total_time = Sys.time() - t
        logger(sprintf('Baseline - Electrode (%d) Total time: %.3f (%s)', electrode, total_time, attr(total_time, 'units')))
      }
      return(bld)
    }
  )
)


