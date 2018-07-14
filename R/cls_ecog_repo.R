#' @import stringr
#' @export
ECoGRepository <- R6::R6Class(
  classname = 'ECoGRepository',
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    subject = NULL,
    raw = NULL,         # Map to store electrodes
    reference = NULL,   # Map to store references
    epochs = NULL,      # map to store epoch infos
    raw_volt = NULL,
    raw_power = NULL,
    raw_phase = NULL,
    volt = NULL,
    power = NULL,
    phase = NULL,
    print = function(...){
      id = self$subject$subject_id
      epoch_info = self$epochs$get('epoch_name')
      if(length(epoch_info)){
        epoch_param = self$epochs$get('epoch_params');

        epoch_info = 'Epoch: ' %&% epoch_info %&% '\n' %&%
          ' - Electrodes: ' %&% rave:::deparse_selections(self$epochs$get('electrodes')) %&% '\n' %&%
          sprintf(' - From %.2f to %.2f (sec)\n', -epoch_param[1], epoch_param[2])
      }else{
        epoch_info = '(Not epoched yet.)\n'
      }

      ref_name = self$reference$get('.reference_name')
      ref_name %?<-% '(No reference table)'
      ref_name = 'Reference table: ' %&% ref_name

      refed = self$reference$get('.is_referenced')
      if(!is.null(self$coef)){
        wave_info = sprintf('Wavelet coefficients: Loaded (%s)', ifelse(refed$spectrum, 'Referenced', 'Raw'))
      }else{
        wave_info = 'Wavelet coefficients: Not loaded'
      }

      if(!is.null(self$volt)){
        volt_info = sprintf('Voltage signal: Loaded (%s)', ifelse(refed$voltage, 'Referenced', 'Raw'))
      }else{
        volt_info = 'Voltage signal: Not loaded'
      }

      cat(sprintf('<ECoG Repository> [%s]\n\n%s\n%s\n%s\n%s\n', id, epoch_info, ref_name, wave_info, volt_info))
      return(self)
    },
    initialize = function(subject, reference = 'default', autoload = T){
      self$raw = Map$new()
      self$reference = Map$new()
      self$epochs = Map$new()
      if(R6::is.R6(subject) && 'Subject' %in% class(subject)){
        self$subject = subject
      }else{
        assertthat::assert_that('character' %in% class(subject),
                                msg = 'Param <subject> needs to be either subject ID or a Subject instance')
        subject = str_split_fixed(subject, '\\\\|/', 2)
        self$subject = Subject$new(project_name = subject[1], subject_code = subject[2], reference = reference)
      }

      # load electrodes
      if(autoload){
        self$load_electrodes(reference = reference)
      }
      invisible()
    },
    get_electrode = function(electrode, name = 'raw'){
      e_str = as.character(electrode)
      re = lapply(name, function(nm){
        self[[nm]]$get(e_str)
      })
      names(re) = name
      return(re)
    },
    load_electrodes = function(electrodes, reference = 'default'){
      if(missing(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = self$subject$filter_valid_electrodes(electrodes)
      }


      # Set up reference
      self$load_reference(reference, electrodes = electrodes)

      electrodes = electrodes[!paste(electrodes) %in% self$raw$keys()]

      ref_table = self$reference$get('ref_table')

      if(length(electrodes) > 0){
        progress = progress(title = 'Checking data...', max = length(electrodes))
        for(e in electrodes){
          e_str = paste(e)
          progress$inc(sprintf('Electrode - %s', e_str))
          # get reference
          ref = ref_table$Reference[ref_table$Electrode == e]
          self$raw$set(key = e_str, value = Electrode$new(subject = self$subject, electrode = e, reference_by = self$reference$get(ref), is_reference = F))
        }
        logger('Loaded.')
        progress$close()
      }
      invisible()
    },
    epoch = function(epoch_name, pre, post, electrodes = NULL, data_type = 'power', referenced = T, func = NULL, quiet = FALSE){
      if(is.null(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = self$subject$filter_valid_electrodes(electrodes)
      }
      electrodes = electrodes[paste(electrodes) %in% self$raw$keys()]
      assertthat::assert_that(length(electrodes) > 0, msg = 'No electrode loaded.')

      self$epochs$set('epoch_name', epoch_name)
      self$epochs$set('epoch_params', c(pre, post))
      self$epochs$set('epoch_data', rave:::load_meta(
        meta_type = 'epoch',
        subject_id = self$subject$subject_id,
        meta_name = epoch_name
      ))
      self$epochs$set('electrodes', electrodes)
      freqs = load_meta(subject_id = self$subject$subject_id, meta_type = 'frequencies')

      progress = progress(title = 'Loading data...', max = (length(electrodes) + 1) * length(data_type), quiet = quiet)
      on.exit({progress$close()})

      epoch_data = self$epochs$get('epoch_data')

      re = list()
      subject_id = self$subject$id

      raws = self$raw
      lapply_async(electrodes, function(e){
        electrode = raws$get(as.character(e))
        electrode$epoch(
          epoch_name = epoch_name,
          pre = pre, post = post, types = data_type, raw = !referenced
        ) -> elc;

        if(is.function(func)){
          elc = func(elc)
        }
        return(elc)
      }, .call_back = function(i){
        progress$inc(sprintf('Preparing electrode - %d', electrodes[i]))
      }) ->
        results

      progress$inc('Finalizing...')

      # collapse results
      if(!is.function(func)){
        env = environment()

        # spectrum

        if('power' %in% data_type){
          sample_spec = results[[1]]$power$data[,,,1]
          .dim = dim(sample_spec)
          .dimnames = dimnames(sample_spec)
          nm = ifelse(referenced, 'power', 'raw_power')
          self[[nm]] = ECoGTensor$new(
            data = vapply(seq_len(length(results)), function(i){
              data = env$results[[i]]$power$data
              env$results[[i]]$power = NULL
              # dimnames(data) = NULL
              return(as.vector(data))
            }, FUN.VALUE = sample_spec),
            dimnames = c(.dimnames, list(Electrode = electrodes)),
            varnames = c(names(.dimnames), 'Electrode')
          )
          rm(sample_spec)
        }

        if('phase' %in% data_type){
          sample_spec = results[[1]]$phase$data[,,,1]
          .dim = dim(sample_spec)
          .dimnames = dimnames(sample_spec)
          nm = ifelse(referenced, 'phase', 'raw_phase')
          self[[nm]] = ECoGTensor$new(
            data = vapply(seq_len(length(results)), function(i){
              data = env$results[[i]]$phase$data
              env$results[[i]]$phase = NULL
              # dimnames(data) = NULL
              return(as.vector(data))
            }, FUN.VALUE = sample_spec),
            dimnames = c(.dimnames, list(Electrode = electrodes)),
            varnames = c(names(.dimnames), 'Electrode')
          )
          rm(sample_spec)
        }

        if('volt' %in% data_type){
          sample_volt = results[[1]]$volt$data[,,1]
          .dim = dim(sample_volt)
          .dimnames = dimnames(sample_volt)
          nm = ifelse(referenced, 'volt', 'raw_volt')
          self[[nm]] = Tensor$new(
            data = vapply(seq_len(length(results)), function(i){
              data = env$results[[i]]$volt$data
              env$results[[i]]$volt = NULL
              dimnames(data) = NULL
              return(data)
            }, FUN.VALUE = sample_volt),
            dimnames = list(
              Trial = as.integer(.dimnames$Trial),
              Time = as.numeric(.dimnames$Time),
              Electrode = as.integer(electrodes)),
            varnames = c(names(.dimnames), 'Electrode')
          )
        }

        return(invisible())
      }else{
        return(results)
      }

    },
    load_reference = function(ref_name, electrodes = NULL){

      if(!length(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = self$subject$filter_valid_electrodes(electrodes)
      }

      # Set reference table
      ref_table = load_meta(
        meta_type = 'references',
        project_name = self$subject$project_name,
        subject_code = self$subject$subject_code,
        meta_name = ref_name
      )
      self$reference$set('ref_table', ref_table)

      # load partial references, also avoid invalid electrodes
      ref_table = ref_table[ref_table$Electrode %in% electrodes, ]

      # Trick: use lazy assign to allocate reference, this is hack to R6 but avoiding evaluations
      ref_env = self$reference$private$env
      unique_refs = unique(ref_table$Reference)
      lapply(unique_refs, function(ref){
        delayedAssign(ref, {
          Electrode$new(subject = self$subject, electrode = ref, is_reference = T)
        }, assign.env = ref_env)
      })

    },
    baseline = function(from, to, electrodes = NULL, print.time = FALSE){
      assertthat::assert_that(!is.null(self$power), msg = 'Please epoch power spectrum first.')

      start = Sys.time()

      # get baseline
      electrodes = electrodes[electrodes %in% self$power$dimnames$Electrode]
      electrodes %?<-% self$power$dimnames$Electrode

      self$power$subset(
        Time = Time %within% c(from, to),
        Electrode = Electrode %in% electrodes,
        data_only = T
      ) ->
        bl
      ntimepts = dim(bl)[3]
      bl = rutabaga::collapse(bl, keep = c(1,2,4)) / ntimepts


      re = ECoGTensor$new(
        data = aperm((aperm(
          self$power$subset(
            Electrode = Electrode %in% electrodes,
            drop = F,
            data_only = T
          ),
          c(1, 2, 4, 3)
        ) / as.vector(bl) - 1) * 100, c(1, 2, 4, 3)),
        dimnames = c(self$power$dimnames[1:3], list(Electrode = electrodes)),
        dim = c(self$power$dim[1:3], length(electrodes)),
        varnames = self$power$varnames
      )

      end = Sys.time()
      if(print.time){
        logger(sprintf('Baseline calculation - %.0f ms', as.numeric(end-start) * 1000))
      }
      return(re)
    }

  )
)


# baseline = function(from, to, electrodes, print.time = FALSE){
#   if(missing(electrodes)){
#     electrodes = self$epochs$get('electrodes')
#   }else{
#     electrodes = self$subject$filter_valid_electrodes(electrodes)
#   }
#   if(length(electrodes) == 1){
#     return(
#       self$..baseline(from = from, to = to, electrode = electrodes, data_only = FALSE, print.time = print.time)
#     )
#   }
#
#   Sys.time() -> t
#
#
#   sample = self$power$get('power')$subset(Electrode = Electrode == electrodes[1], data_only = T)
#   .dimnames = dimnames(sample)[1:3]
#   vapply(
#     electrodes,
#     function(e){
#       self$..baseline(from = from, to = to, electrode = e, data_only = T, re_dim = F)
#     },
#     FUN.VALUE = sample[,,,1]
#   ) ->
#     d
#   d = ECoGTensor$new(
#     data = d,
#     dimnames = c(.dimnames, list(Electrode = electrodes)),
#     varnames = c(names(.dimnames), 'Electrode')
#   )
#
#   if(print.time){
#     total_time = Sys.time() - t
#     logger(sprintf('Baseline Total time: %.3f (%s)', total_time, attr(total_time, 'units')))
#   }
#   return(d)
#
# },
# ..baseline = function(from, to, electrode, e_power = NULL,
#                       print.time = FALSE, data_only = F, re_dim = T){
#   assertthat::assert_that(
#     self$is_epoched(electrodes = electrode),
#     msg = 'Electrode not loaded, run .$epoch() first.'
#   )
#   t = Sys.time()
#   srate = self$subject$sample_rate
#   from = round(from * srate)
#   to = round(to * srate)
#   epoch = self$epochs$get('epoch_data')
#   epoch$start = round(epoch$Time * srate) + from
#   # epoch$end = round(epoch$Time * srate) + to
#   epoch$end = epoch$start + (to - from)
#
#   if(is.null(e_power)){
#     e_raw = self$raw$get(str_c(electrode))
#     e_power = self$power$get('power')$subset(Electrode = Electrode == electrode, data_only = T)
#   }else{
#     electrode = dimnames(e_power)$Electrode
#     e_raw = self$raw$get(str_c(electrode))
#     if(is(e_power, 'Tensor')){
#       e_power = e_power$data
#     }
#   }
#
#   bls_dim = dim(e_power)
#   bls_dimnames = dimnames(e_power)
#   bls = array(0, dim = bls_dim[c(1,2)])
#   for(b in unique(epoch$Block)){
#     sel = epoch$Block == b
#     sub = epoch[sel, ]
#     n = sum(sel)
#
#     #NB: need to go one back in the cumsum vector
#     #FIXME will it ever be the case sub$start-1 <= 0 ?
#     slice = e_raw$get_data(block_num = b, time_point = c(sub$start-1, sub$end), name = 'cumsum')
#
#     # if we take time 10:50, there are 1 + (50 - 10) time points
#     bls[sel, ] = t(slice[,-c(1:n)] - slice[,1:n])/(1 + to - from)
#   }
#
#   bld = (e_power / as.vector(bls) - 1) * 100
#
#   if(re_dim){
#     dim(bld) = bls_dim
#     dimnames(bld) = bls_dimnames
#   }
#   if(!data_only){
#     bld = ECoGTensor$new(
#       data = bld,
#       dimnames = bls_dimnames,
#       varnames = names(bls_dimnames)
#     )
#   }
#   if(print.time){
#     total_time = Sys.time() - t
#     logger(sprintf('Baseline - Electrode (%d) Total time: %.3f (%s)', electrode, total_time, attr(total_time, 'units')))
#   }
#   return(bld)
# }
