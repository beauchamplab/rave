#' Baseline signals
#' @param el Tensor or ECoGTensor object
#' @param from baseline start time
#' @param to baseline end time
#' @param mean or median, default is mean
#' @param unit "\%" percent signal change or "dB" decibel unit
#' @param data_only return array or tensor object?
#' @param hybrid if return tensor object, swap cache? useful for large dataset
#' @param swap_file by default tempfile, or you can specify path
#' @param mem_optimize optimize for large dataset? default is TRUE
#' @param preop function before baselining
#' @param op function for baselining
#' @export
baseline <- function(el, from, to, method = 'mean', unit = '%',
                    data_only = F, hybrid = T, swap_file = tempfile(), mem_optimize = T, preop = NULL, op){
  if(missing(el)){
    logger('baseline(el...) is changed now. Please update.', level = 'WARNING')
    el = module_tools$get_power()
  }
  assertthat::assert_that(is(el, 'Tensor'), msg = 'el must be an Tensor object.')
  assertthat::assert_that('Time' %in% el$varnames, msg = 'Need one dimname to be "Time".')

  assertthat::assert_that(unit %in% c('dB', '%', 'C'), msg = 'unit must be %-percent signal change or dB-dB difference, or C to customize')

  time_ind = which(el$varnames == 'Time')
  rest_dim = seq_along(el$dim)[-time_ind]

  if(unit == 'dB'){
    bs = el$subset(Time = Time %within% c(from, to))

    # log 10 of data, collapse by mean
    bs$set_data(log10(bs$get_data()))

    # Use better format of collapse function to avoid crashing
    bs = bs$collapse(keep = rest_dim, method = method)

    # for each time points, log10 and substract, Since dB is 10*log10(.)
    op = function(e1, e2){ 10 * (log10(e1) - e2) }

    bs = el$operate(by = bs, match_dim = rest_dim, mem_optimize = mem_optimize, fun = op)
  }else if(unit == '%'){
    bs = el$subset(Time = Time %within% c(from, to))
    op = function(e1, e2){ e1 / e2 * 100 - 100 }
    bs = bs$collapse(keep = rest_dim, method = method)
    bs = el$operate(by = bs, match_dim = rest_dim, mem_optimize = mem_optimize, fun = op)
  }else{
    # customizing
    bs = el$subset(Time = Time %within% c(from, to))
    if(is.function(preop)){
      bs$set_data(preop(bs$get_data()))
    }
    bs = bs$collapse(keep = rest_dim, method = method)
    bs = el$operate(by = bs, match_dim = rest_dim, mem_optimize = mem_optimize, fun = op)
  }

  if(data_only){
    return(bs)
  }else{
    ECoGTensor$new(data = bs, dim = dim(el), dimnames = dimnames(el), varnames = el$varnames, hybrid = hybrid, swap_file = swap_file)
  }
}

#' R6 class for ECoG data Repository
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
    info = function(print = TRUE){
      id = self$subject$subject_id
      epoch_info = self$epochs$get('epoch_name')
      if(length(epoch_info)){
        epoch_param = self$epochs$get('epoch_params');

        epoch_info = 'Epoch: ' %&% epoch_info %&% '\n' %&%
          ' - Electrodes: ' %&% deparse_selections(self$epochs$get('electrodes')) %&% '\n' %&%
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

      if(print){
        cat(sprintf('<ECoG Repository> [%s]\n\n%s\n%s\n%s\n%s\n', id, epoch_info, ref_name, wave_info, volt_info))
      }
      invisible(list(
        id = id, epoch_info = epoch_info, ref_name = ref_name, wave_info = wave_info, volt_info = volt_info
      ))
    },
    print = function(...){
      # To compatible with globals package
      cat(pryr::address(self))
      invisible()
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
    epoch = function(epoch_name, pre, post, electrodes = NULL, frequency_range = NULL, data_type = 'power', referenced = T, func = NULL, quiet = FALSE){
      if(is.null(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = self$subject$filter_valid_electrodes(electrodes)
      }
      electrodes = electrodes[paste(electrodes) %in% self$raw$keys()]
      assertthat::assert_that(length(electrodes) > 0, msg = 'No electrode loaded.')

      self$epochs$set('epoch_name', epoch_name)
      self$epochs$set('epoch_params', c(pre, post))
      self$epochs$set('epoch_data', load_meta(
        meta_type = 'epoch',
        subject_id = self$subject$subject_id,
        meta_name = epoch_name
      ))
      self$epochs$set('electrodes', electrodes)
      self$epochs$set('frequency_range', frequency_range)
      freqs = load_meta(subject_id = self$subject$subject_id, meta_type = 'frequencies')
      frequency_range %?<-% range(freqs$Frequency)
      freq_subset = freqs$Frequency %within% frequency_range
      if(!sum(freq_subset)){
        logger('Frequency range is invalid, looking for the nearest frequency', level = 'WARNING')
        freq_subset[which.min(abs(freqs$Frequency - frequency_range[1]))] = T
      }

      progress = progress(title = 'Loading data...', max = (length(electrodes) + 1) * length(data_type), quiet = quiet)
      on.exit({progress$close()})

      epoch_data = self$epochs$get('epoch_data')

      re = list()
      subject_id = self$subject$id

      raws = self$raw

      # progress$inc('Finalizing...')
      # Get dimension names
      # 1. Trial
      epochs = load_meta(
        meta_type = 'epoch',
        meta_name = epoch_name,
        project_name = self$subject$project_name,
        subject_code = self$subject$subject_code
      )
      trial_order = order(epochs$Trial)
      dn_trial = epochs$Trial[trial_order]
      dn_freqs = freqs$Frequency[freq_subset]
      n_dt = length(data_type)
      n_elec = length(electrodes)
      dimnames_wave = list(
        Trial = dn_trial,
        Frequency = dn_freqs,
        Time = seq(-pre, post, by = 1 / subject$sample_rate),
        Electrode = electrodes
      )
      dimnames_volt = list(
        Trial = dn_trial,
        Time = seq(-pre, post, by = 1 / subject$preprocess_info('srate')),
        Electrode = electrodes
      )
      count = 1

      # collapse results
      if(!is.function(func)){

        if('power' %in% data_type){
          lapply_async(electrodes, function(e){
            electrode = raws$get(as.character(e))
            electrode$epoch(
              epoch_name = epoch_name,
              pre = pre,
              post = post,
              types = 'power',
              raw = !referenced
            ) ->
              elc

            power = elc$power$subset(Frequency = freq_subset, drop = T, data_only = T)
            rm(elc)
            power = as.vector(power)
            return(power)
          }, .call_back = function(i){
            progress$inc(sprintf('Step %d (of %d) electrode %d (power)', count, n_dt, electrodes[i]))
          }) ->
            results
          count = count + 1
          gc()

          names(results) = paste0('V', seq_along(electrodes))
          results = do.call('data.frame', results)

          # Generate tensor for power
          power = ECoGTensor$new(0, dim = c(1,1,1,1), varnames = names(dimnames_wave), hybrid = F)

          # erase data
          power$set_data(NULL)
          # reset dim and dimnames
          power$dim = vapply(dimnames_wave, length, FUN.VALUE = 0, USE.NAMES = F)
          power$dimnames = dimnames_wave

          # generate local cache for power
          file = tempfile()
          fst::write_fst(results, file, compress = 20)
          rm(results)
          gc()

          # change tensor file path
          power$swap_file = file
          power$hybrid = T
          power$use_index = TRUE

          nm = ifelse(referenced, 'power', 'raw_power')
          self[[nm]] = power
        }

        if('phase' %in% data_type){
          lapply_async(electrodes, function(e){
            electrode = raws$get(as.character(e))
            electrode$epoch(
              epoch_name = epoch_name,
              pre = pre,
              post = post,
              types = 'phase',
              raw = !referenced
            ) ->
              elc

            phase = elc$phase$subset(Frequency = freq_subset, drop = T, data_only = T)
            rm(elc)
            phase = as.vector(phase)
            return(phase)
          }, .call_back = function(i){
            progress$inc(sprintf('Step %d (of %d) electrode %d (phase)', count, n_dt, electrodes[i]))
          }) ->
            results
          count = count + 1
          gc()

          names(results) = paste0('V', seq_along(electrodes))
          results = do.call('data.frame', results)

          # Generate tensor for phase
          phase = ECoGTensor$new(0, dim = c(1,1,1,1), varnames = names(dimnames_wave), hybrid = F)

          # erase data
          phase$set_data(NULL)
          # reset dim and dimnames
          phase$dim = vapply(dimnames_wave, length, FUN.VALUE = 0, USE.NAMES = F)
          phase$dimnames = dimnames_wave

          # generate local cache for phase
          file = tempfile()
          fst::write_fst(results, file, compress = 20)
          rm(results)
          gc()

          # change tensor file path
          phase$swap_file = file
          phase$hybrid = T
          phase$use_index = TRUE

          nm = ifelse(referenced, 'phase', 'raw_phase')
          self[[nm]] = phase
        }

        if('volt' %in% data_type){
          lapply_async(electrodes, function(e){
            electrode = raws$get(as.character(e))
            electrode$epoch(
              epoch_name = epoch_name,
              pre = pre,
              post = post,
              types = 'volt',
              raw = !referenced
            ) ->
              elc
            volt = elc$volt$get_data()
            rm(elc)
            volt = as.vector(volt)
            return(volt)
          }, .call_back = function(i){
            progress$inc(sprintf('Step %d (of %d) electrode %d (voltage)', count, n_dt, electrodes[i]))
          }) ->
            results
          count = count + 1
          gc()

          names(results) = paste0('V', seq_along(electrodes))
          results = do.call('data.frame', results)

          # Generate tensor for voltage
          volt = ECoGTensor$new(0, dim = c(1,1,1), varnames = names(dimnames_volt), hybrid = F)

          # erase data
          volt$set_data(NULL)
          # reset dim and dimnames
          volt$dim = vapply(dimnames_volt, length, FUN.VALUE = 0, USE.NAMES = F)
          volt$dimnames = dimnames_volt

          # generate local cache for phase
          file = tempfile()
          fst::write_fst(results, file, compress = 20)
          rm(results)
          gc()

          # change tensor file path
          volt$swap_file = file
          volt$hybrid = T
          volt$use_index = TRUE

          nm = ifelse(referenced, 'volt', 'raw_volt')
          self[[nm]] = volt
        }

        return(invisible())
      }else{

        lapply_async(electrodes, function(e){
          electrode = raws$get(as.character(e))
          electrode$epoch(
            epoch_name = epoch_name,
            pre = pre, post = post, types = data_type, raw = !referenced
          ) -> elc;

          if(is(elc$power, 'ECoGTensor')){
            elc$power = elc$power$subset(Frequency = freq_subset, drop = F)
          }
          if(is(elc$phase, 'ECoGTensor')){
            elc$phase = elc$phase$subset(Frequency = freq_subset, drop = F)
          }

          elc = func(elc)
          return(elc)
        }, .call_back = function(i){
          progress$inc(sprintf('Preparing electrode - %d', electrodes[i]))
        }) ->
          results
        gc()

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


