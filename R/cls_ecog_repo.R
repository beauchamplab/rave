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
    coef = NULL,
    volt = NULL,
    epochs = NULL,
    reference = NULL,
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
    initialize = function(subject, autoload = T){
      self$raw = Map$new()
      self$reference = Map$new()
      self$epochs = Map$new()
      if(R6::is.R6(subject) && 'Subject' %in% class(subject)){
        self$subject = subject
      }else{
        assertthat::assert_that('character' %in% class(subject),
                                msg = 'Param <subject> needs to be either subject ID or a Subject instance')
        subject = str_split_fixed(subject, '\\\\|/', 2)
        self$subject = Subject$new(project_name = subject[1], subject_code = subject[2])
      }
      if(autoload){
        self$load_electrodes()
      }

      self$reference$set('.is_referenced', list(spectrum = F, voltage = F))
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
    load_electrodes = function(electrodes, quiet = FALSE){
      if(missing(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = electrodes[electrodes %in% self$subject$valid_electrodes]
      }
      electrodes = electrodes[!paste(electrodes) %in% self$raw$keys()]

      subject_id = self$subject$subject_id

      if(length(electrodes) > 0){
        logger('Loading electrodes')
        progress = progress(title = 'Checking data...', max = length(electrodes), quiet = quiet)
        for(e in electrodes){
          e_str = paste(e)
          progress$inc(sprintf('Electrode - %s', e_str))
          self$raw$set(key = e_str, value = Electrode$new(subject = self$subject, electrode = e))
        }
        logger('Loaded.')
        progress$close()
      }
      invisible()
    },
    epoch = function(epoch_name, pre, post, electrodes = NULL, data_type = 'spectrum', func = NULL, quiet = FALSE){
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

      progress = progress(title = 'Loading data...', max = (length(electrodes) + 1) * length(names), quiet = quiet)
      on.exit({progress$close()})

      epoch_data = self$epochs$get('epoch_data')

      re = list()
      subject_id = self$subject$id

      raws = self$raw
      lapply_async(electrodes, function(e){
        electrode = raws$get(as.character(e))
        electrode$epoch(
          epoch_name = epoch_name,
          pre = pre, post = post, type = data_type
        ) -> elc;
        rhdf5::H5close();

        if(is.function(func)){
          elc = func(elc)
        }
        return(elc)
      }, .call_back = function(i){
        progress$inc(sprintf('Electrode - %d', electrodes[i]))
      }) ->
        results

      progress$inc('Finalizing...')

      # collapse results
      if(!is.function(func)){
        env = environment()

        # spectrum
        if('spectrum' %in% data_type){
          sample_spec = results[[1]]$spectrum$data[,,,1]
          .dim = dim(sample_spec)
          .dimnames = dimnames(sample_spec)
          self$coef = ECoGTensor$new(
            data = vapply(seq_len(length(results)), function(i){
              data = env$results[[i]]$spectrum$data
              env$results[[i]]$spectrum = NULL
              dimnames(data) = NULL
              return(data)
            }, FUN.VALUE = sample_spec),
            dimnames = c(.dimnames, list(Electrode = electrodes)),
            varnames = c(names(.dimnames), 'Electrode')
          )
        }

        if('voltage' %in% data_type){
          sample_volt = results[[1]]$voltage$data[,,1]
          .dim = dim(sample_volt)
          .dimnames = dimnames(sample_volt)
          self$volt = Tensor$new(
            data = vapply(seq_len(length(results)), function(i){
              data = env$results[[i]]$voltage$data
              env$results[[i]]$voltage = NULL
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

        return(self)
      }else{
        re[[name]] = results
        return(re)
      }

    },
    load_reference = function(ref_name){

      # Revert back if referenced (from No-ref -> ref)
      last_refname = self$reference$get('.reference_name')
      if(length(last_refname) && last_refname != ref_name){
        ref = self$reference$get('.is_referenced')
        data_type = names(ref)[unlist(ref)]
        if(length(data_type)){
          self$calc_reference(original = T, data_type = data_type)
        }
      }



      ref_table = load_meta(
        meta_type = 'references',
        project_name = self$subject$project_name,
        subject_code = self$subject$subject_code,
        meta_name = ref_name
      )
      self$reference$set('.reference_name', ref_name)
      if(!is.null(ref_table)){
        self$reference$set('.table', ref_table)
        if(ref_name != 'noref'){
          lapply(unique(ref_table$Reference), function(ref){
            e = Electrode$new(subject = self$subject, electrode = ref, is_reference = T)
            self$reference$set(ref, e)
          })
        }
        return(T)
      }
      return(F)
    },

    calc_reference = function(original = F, ref_name = NULL, data_type = 'spectrum'){

      epoch_name = self$epochs$get(key = 'epoch_name')
      epoch_params = self$epochs$get(key = 'epoch_params')
      assertthat::assert_that(!is.null(epoch_name), msg = 'Must epoch data first.')

      if(is.character(ref_name)){
        self$load_reference(ref_name)
      }

      # if original, then return electrodes with no ref, otherwise, return referenced channels
      referenced = self$reference$get('.is_referenced')

      # step 1: load data if not exists
      if('spectrum' %in% data_type && is.null(self$coef)){
        # load spectrum data and epoch it
        self$epochs$set(key = 'signature', value = 'JUNK')
        self$epoch(epoch_name = epoch_name, pre = epoch_params[1], post = epoch_params[2], data_type = 'spectrum')
        referenced$spectrum = FALSE
      }
      if('voltage' %in% data_type && is.null(self$volt)){
        self$epochs$set(key = 'signature', value = 'JUNK')
        self$epoch(epoch_name = epoch_name, pre = epoch_params[1], post = epoch_params[2], data_type = 'voltage')
        referenced$voltage = FALSE
      }

      # step 2:
      # if referenced but original data needed convert it back (this is rare)
      # if not referenced but is needed, reference it
      sign_wave = 0
      sign_volt = 0
      if(referenced$spectrum && original){
        sign_wave = +1
      }
      if(!referenced$spectrum && !original){
        sign_wave = -1
      }
      if(referenced$voltage && original){
        sign_volt = +1
      }
      if(!referenced$voltage && !original){
        sign_volt = -1
      }

      # Step 3: load reference signals
      dt = NULL
      if('spectrum' %in% data_type && sign_wave!=0){
        dt = c('spectrum')
        referenced$spectrum = !referenced$spectrum
      }
      if('voltage' %in% data_type && sign_volt!=0){
        dt = c(dt, 'voltage')
        referenced$voltage = !referenced$voltage
      }
      if(length(dt)){
        ref_table = self$reference$get('.table')
        refs = unique(ref_table$Reference)
        refs = refs[!refs %in% 'noref']
        progress = progress('Collect Reference Information', max = length(refs)); on.exit({progress$close()})

        ref = lapply(refs, function(ref){
          progress$inc(message = 'Calculating ' %&% ref)
          e = self$reference$get(ref)
          future::future({
            e$epoch(epoch_name = epoch_name, pre = epoch_params[1], post = epoch_params[2], type = dt)
          })
        })
        ref = future::values(ref)
        names(ref) = refs
      }else{
        # no need to do anything
        return(invisible())
      }


      # Step 4: reference tensor
      lapply(refs, function(r){
        es = ref_table$Electrode[ref_table$Reference == r]
        ind = self$coef$dimnames$Electrode %in% es

        if(length(ind)){
          if(length(ref[[r]][['voltage']])){
            self$volt$data[,,ind] = self$volt$data[,,ind] + sign_volt * as.vector(ref[[r]][['voltage']]$data)
          }
          if(length(ref[[r]][['spectrum']])){
            self$coef$data[,,,ind] = self$coef$data[,,,ind] + sign_wave * as.vector(ref[[r]][['spectrum']]$data)
          }
        }

        NULL
      })

      self$reference$set('.is_referenced', referenced)
      invisible()
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
    baseline = function(from, to, electrodes = NULL, print.time = FALSE){
      assertthat::assert_that(!is.null(self$coef), msg = 'Please epoch power spectrum first.')

      start = Sys.time()

      if(!length(electrodes)){
        electrodes = self$coef$dimnames$Electrode
        re = self$coef$data
        bl = self$coef$subset(
          Time = Time %within% c(from, to),
          drop = F,
          data_only = T
        )
      }else{
        re = self$coef$subset(Electrode = Electrode %in% electrodes, drop = F, data_only = T)
        # re is trial x freq x time x electrodes
        bl = self$coef$subset(
          Electrode = Electrode %in% electrodes,
          Time = Time %within% c(from, to),
          drop = F,
          data_only = T
        )
      }

      re = Mod(re) ^2


      ntimepts = dim(bl)[3]
      bl = rutabaga::collapse(Mod(bl)^2, c(1,2,4)) / ntimepts
      dimnames(bl) = NULL


      # microbenchmark::microbenchmark(
      #   {
      #     vapply(seq_len(dim(re)[3]), function(ti){
      #       (re[,,ti,] / as.vector(bl) - 1) * 100
      #     }, FUN.VALUE = bl, USE.NAMES = F) ->
      #       l
      #   },
      #   aperm((aperm(re, c(1,2,4,3)) / as.vector(bl) - 1) * 100, c(1,2,4,3)),
      #   times = 10L
      # )
      #
      # vapply(seq_len(dim(re)[3]), function(ti){
      #   (re[,,ti,] / as.vector(bl) - 1) * 100
      # }, FUN.VALUE = bl) ->
      #   l
      # l = aperm((aperm(re, c(1,2,4,3)) / as.vector(bl) - 1) * 100, c(1,2,4,3))

      re = ECoGTensor$new(
        data = aperm((aperm(re, c(1,2,4,3)) / as.vector(bl) - 1) * 100, c(1,2,4,3)),
        dimnames = c(self$coef$dimnames[1:3], list(Electrode = electrodes)),
        dim = c(self$coef$dim[1:3], length(electrodes)),
        varnames = self$coef$varnames
      )

      end = Sys.time()
      if(print.time){
        logger(sprintf('Baseline calculation - %.0f ms', as.numeric(end-start) * 1000))
      }
      return(re)
    }
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
  )
)


