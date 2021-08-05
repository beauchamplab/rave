# Documented with roxygen 7.0.0 on 2019-11-10



#' @title R6 class for \code{iEEG/ECoG} data Repository
#' @author Zhengjia Wang
#' @description A repository to keep subject information, including electrode
#' instances, reference information, epoch data, and offers method to epoch 
#' data.
#' @examples 
#' \dontrun{
#' 
#' # Two ways to create instances
#' repo <- ECoGRepository$new('demo/YAB')
#' 
#' subject <- Subject$new(project_name = 'demo', subject_code = 'YAB')
#' repo <- ECoGRepository$new(subject)
#' 
#' # Create an instance without auto collecting references, only load 
#' # interesting electrodes
#' repo <- ECoGRepository$new('demo/YAB', autoload = FALSE)
#' repo$load_electrodes(c(14,15))
#' 
#' # Create an instance with non-default reference
#' repo <- ECoGRepository$new('demo/YAB', reference = 'bipolar')
#' 
#' # Epoch data according to epoch file "epoch_YABaOutlier.csv" in meta folder
#' # epoch_name should be "epoch_(name).csv"
#' repo$epoch(epoch_name = 'YABaOutlier', pre = 1, post = 2, 
#'            electrodes = 14, referenced = TRUE, data_type = "power")
#' repo$power
#' #> Dimension:  287 x 16 x 301 x 1 
#' #> - Trial: 1, 2, 3, 4, 5, 6,...
#' #> - Frequency: 2, 12, 22, 32, 42...
#' #> - Time: -1, -0.99, -0.98,...
#' #> - Electrode: 14
#' 
#' }
#' @export
ECoGRepository <- R6::R6Class(
  classname = 'ECoGRepository',
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    
    #' @field subject \code{\link[rave]{Subject}} instance
    subject = NULL,
    
    #' @field raw dictionary to store \code{\link[rave]{Electrode}} instances
    raw = NULL,         # Map to store electrodes
    
    #' @field reference dictionary to store references for electrodes
    reference = NULL,   # Map to store references
    
    #' @field epochs dictionary to store epoch data
    epochs = NULL,      # map to store epoch infos
    
    #' @field raw_volt environment, stores pre-referenced analog traces
    raw_volt = NULL,
    
    #' @field raw_power environment, stores pre-referenced power spectrum
    raw_power = NULL,
    
    #' @field raw_phase environment, stores pre-referenced phase data
    raw_phase = NULL,
    
    #' @field volt environment, stores referenced analog traces
    volt = NULL,
    
    #' @field power environment, stores referenced power spectrum
    power = NULL,
    
    #' @field phase environment, stores referenced phase data
    phase = NULL,
    
    #' @description obtain the information 
    #' @param print logical, whether to print the information, default is true
    #' @return character of the information
    info = function(print = TRUE){
      id = self$subject$subject_id
      epoch_info = self$epochs$epoch_name
      if(length(epoch_info)){
        epoch_param = self$epochs$epoch_params;
        
        epoch_info = paste0(
          'Epoch: ' , epoch_info , '\n' ,
          ' - Electrodes: ' , deparse_selections(self$epochs$electrodes) , '\n' ,
          sprintf(' - From %.2f to %.2f (sec)\n', -epoch_param[1], epoch_param[2])
        )
      }else{
        epoch_info = '(Not epoched yet.)\n'
      }
      
      ref_name = self$reference$.reference_name
      ref_name %?<-% '(No reference table)'
      ref_name = paste0('Reference table: ' , ref_name)
      
      refed = self$reference$.is_referenced
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
    
    #' @description print memory address
    #' @param ... ignored
    #' @return none
    print = function(...){
      # To compatible with globals package
      cat(env_address(self))
      invisible()
    },
    
    #' @description constructor
    #' @param subject character such as \code{"project/subject"} or 
    #' \code{\link[rave]{Subject}} instance
    #' @param reference character, reference name, default is \code{"default"}, 
    #' which refers to \code{"reference_default.csv"} in subject meta folder
    #' @param autoload logical, whether to auto-load reference for all 
    #' electrodes, default is yes.
    #' @return An \code{ECoGRepository} instance
    initialize = function(subject, reference = 'default', autoload = TRUE){
      
      catgl('Initializing a Data Repository')
      
      self$raw = dipsaus::fastmap2()
      self$reference = dipsaus::fastmap2()
      self$epochs = dipsaus::fastmap2()
      if(R6::is.R6(subject) && 'Subject' %in% class(subject)){
        self$subject = subject
      }else{
        stopifnot2('character' %in% class(subject),
                    msg = 'Param <subject> needs to be either subject ID or a Subject instance')
        subject = stringr::str_split_fixed(subject, '\\\\|/', 2)
        self$subject = Subject$new(project_name = subject[1], subject_code = subject[2], reference = reference, strict = FALSE)
      }
      
      # load electrodes
      if(autoload){
        self$load_electrodes(reference = reference)
      }
      invisible()
    },
    
    
    #' @description get \code{\link[rave]{Electrode}} instances
    #' @param electrode integers, referring to electrode numbers
    #' @param name character, \code{"raw"}, \code{"power"}, \code{"raw_phase"}, 
    #' etc.
    #' @return list of environments containing electrode instances
    get_electrode = function(electrode, name = 'raw'){
      e_str = as.character(electrode)
      re = lapply(name, function(nm){
        self[[nm]]$get(e_str)
      })
      names(re) = name
      return(re)
    },
    
    #' @description load electrodes; usually don't need to directly call this 
    #' method if \code{autoload} is true when initializing the repository
    #' @param electrodes electrode number (integer)
    #' @param reference name of reference
    #' @return none
    load_electrodes = function(electrodes, reference = 'default'){
      if(missing(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = self$subject$filter_valid_electrodes(electrodes)
      }
      
      
      # Set up reference
      self$load_reference(reference, electrodes = electrodes)
      
      electrodes = electrodes[!paste(electrodes) %in% names(self$raw)]
      
      ref_table = self$reference$ref_table
      
      if(length(electrodes) > 0){
        progress = progress(title = 'Loading reference', 
                            max = length(electrodes))
        
        # subject_obj = self$subject
        
        lapply(electrodes, function(e){
          e_str = paste(e)
          progress$inc(sprintf('Electrode - %s', e_str))
          # get reference
          ref = ref_table$Reference[ref_table$Electrode == e]
          ref = .subset2(self$reference, 'get')(ref, ref)

          e_obj = Electrode$new(subject = self$subject, electrode = e, 
                                reference_by = ref, is_reference = FALSE)
          self$raw[[e_str]] = e_obj
        })
        progress$close()
        # self_ref = self$reference
        # self_sub = self$subject
        # rave:::future_assign_lapply(
        #   x = electrodes,
        #   varnames = as.character(electrodes),
        #   expr = {
        #     ref = ref_table$Reference[ref_table$Electrode == e]
        #     ref = self_ref$get(ref, ref)
        #     e_obj = Electrode$new(subject = self_sub, electrode = e, reference_by = ref, is_reference = F)
        #     e_obj
        #   }, elname = 'e',
        #   nworkers = rave_options('max_worker'),
        #   assign.env = self$raw$private$env
        # )
      }
      invisible()
    },
    
    
    #' @description slice the data according to epoch table
    #' @param epoch_name the name of epoch; for example, \code{"YABa"} refers
    #' to \code{"epoch_YABa.csv"} in subject meta folder.
    #' @param pre positive number in seconds, how long should the time be 
    #' kept before the onset
    #' @param post positive number in seconds, how long should the time be 
    #' kept after onset
    #' @param electrodes integers, electrode numbers
    #' @param frequency_range experimental, frequency range to include
    #' @param data_type data types to epoch; default is \code{"power"}, which 
    #' is power spectrum, or amplitude. Other choices are \code{"phase"} 
    #' for phase data and \code{"volt"} for voltage or analog signal traces.
    #' @param referenced whether to load data referenced or without reference
    #' @param func experimental, function to apply to each electrodes
    #' @param quiet whether to suppress output messages, default is no
    #' @return none. However the results are stored in public fields.
    epoch = function(epoch_name, pre, post, electrodes = NULL,
                     frequency_range = NULL, data_type = 'power',
                     referenced = TRUE, func = NULL, quiet = FALSE
    ){
      # self = .private$repo;referenced = T;epoch_name='YABa';pre=1;post=2;electrodes=preload_info$electrodes;frequency_range=NULL;data_type = 'phase';quiet=F
      if(is.null(electrodes)){
        electrodes = self$subject$valid_electrodes
      }else{
        electrodes = self$subject$filter_valid_electrodes(electrodes)
      }
      electrodes = electrodes[paste(electrodes) %in% names(self$raw)]
      stopifnot2(length(electrodes) > 0, msg = 'No electrode loaded.')
      
      self$epochs$epoch_name = epoch_name
      self$epochs$epoch_params = c(pre, post)
      self$epochs$epoch_data = load_meta(
        meta_type = 'epoch',
        subject_id = self$subject$subject_id,
        meta_name = epoch_name
      )
      self$epochs$electrodes = electrodes
      self$epochs$frequency_range = frequency_range
      freqs = load_meta(subject_id = self$subject$subject_id, meta_type = 'frequencies')
      frequency_range %?<-% range(freqs$Frequency)
      freq_subset = freqs$Frequency %within% frequency_range
      if(!sum(freq_subset)){
        catgl('Frequency range is invalid, looking for the nearest frequency', level = 'WARNING')
        freq_subset[which.min(abs(freqs$Frequency - frequency_range[1]))] = T
      }
      
      # progress = progress(title = 'Loading data...', 
      #                     max = length(data_type), 
      #                     shiny_auto_close = TRUE,
      #                     quiet = quiet)
      
      epoch_data = self$epochs$epoch_data
      
      re = list()
      subject_id = self$subject$id
      
      raws = self$raw
      
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
      
      # to store raw
      
      # collapse results
      if(!is.function(func)){
        
        if('power' %in% data_type){
          
          rave_setup_workers()
          
          # electrodes = 14:15; self=env$.private$repo; epoch_name = "YABaOutlier"; pre=1;post=2;referenced=TRUE;raws=self$raw; freqs = load_meta(subject_id = self$subject$subject_id, meta_type = 'frequencies'); freq_subset = freqs$Frequency %within% c(0,100000)
          results = dipsaus::lapply_async2(electrodes, function(e){
            # progress$inc(sprintf('Step %d (of %d) electrode %d (power)', count, n_dt, e))
            electrode = raws[[as.character(e)]]
            elc = electrode$epoch( epoch_name = epoch_name, pre = pre, post = post,
                                   types = 'power', raw = !referenced )
            
            power = elc$power; rm(elc)
            if(!all(freq_subset)){
              power$temporary = TRUE
              power = power$subset(Frequency = freq_subset, 
                                   drop = FALSE, data_only = FALSE)
              power$to_swap_now(use_index = FALSE)
              power$temporary = FALSE
            }
            gc()
            power
          }, callback = function(e){
            sprintf('Loading power| - electrode %d', e)
          }, plan = FALSE)
          
          # assign('xxxx', results, envir = globalenv())
          # base::print('--------')
          # base::print(results[[1]]$swap_file)
          power = join_tensors(results, temporary = FALSE)
          # base::print(power$swap_file)
          
          count = count + 1
          # gc()
          
          rm(results)
          gc()
          
          
          # set to be read-only
          power$read_only = TRUE
          
          nm = ifelse(referenced, 'power', 'raw_power')
          self[[nm]] = power
        }
        
        if('phase' %in% data_type){
          
          rave_setup_workers()
          results = dipsaus::lapply_async2(electrodes, function(e){
            electrode = raws[[as.character(e)]]
            elc = electrode$epoch(epoch_name = epoch_name, pre = pre, post = post,
                                  types = 'phase', raw = !referenced)
            phase = elc$phase; rm(elc)
            # if(!all(freq_subset)){
            #   phase = phase$subset(Frequency = freq_subset, drop = F, data_only = F)
            #   phase$to_swap_now(use_index = FALSE)
            # }
            # phase
            if(!all(freq_subset)){
              phase$temporary = TRUE
              phase = phase$subset(Frequency = freq_subset, drop = FALSE, data_only = FALSE)
              phase$to_swap_now(use_index = FALSE)
              phase$temporary = FALSE
            }
            
            gc()
            # phase = elc$phase$subset(Frequency = freq_subset, drop = T, data_only = T)
            # rm(elc)
            # phase = as.vector(phase)
            return(phase)
          }, callback = function(e){
            sprintf('Loading phase| - electrode %d', e)
          }, plan = FALSE, future.chunk.size = 1)
          phase = join_tensors(results, temporary = FALSE)
          count = count + 1
          # gc()
          
          # names(results) = paste0('V', seq_along(electrodes))
          # results = do.call('data.frame', results)
          #
          # # Generate tensor for phase
          # phase = ECoGTensor$new(0, dim = c(1,1,1,1), varnames = names(dimnames_wave), hybrid = F)
          #
          # # erase data
          # phase$set_data(NULL)
          # # reset dim and dimnames
          # phase$dim = vapply(dimnames_wave, length, FUN.VALUE = 0, USE.NAMES = F)
          # phase$dimnames = dimnames_wave
          #
          # # generate local cache for phase
          # file = tempfile()
          # raveio::save_fst(results, file, compress = 20)
          # rm(results)
          # gc()
          #
          # # change tensor file path
          # phase$swap_file = file
          # phase$hybrid = T
          # phase$use_index = TRUE
          
          # set to be read-only
          phase$read_only = TRUE
          
          nm = ifelse(referenced, 'phase', 'raw_phase')
          self[[nm]] = phase
        }
        
        if('volt' %in% data_type){
          
          rave_setup_workers()
          results = dipsaus::lapply_async2(electrodes, function(e){
            electrode = raws[[as.character(e)]]
            elc = electrode$epoch( epoch_name = epoch_name, pre = pre, post = post,
                                   types = 'volt', raw = !referenced )   
            volt = elc$volt
            volt$to_swap_now(use_index = FALSE)
            volt$temporary = FALSE
            return(volt)
          }, callback = function(e){
            sprintf('Loading voltage| - electrode %d', e)
          }, plan = FALSE, future.chunk.size = 1)
          
          volt = join_tensors(results, temporary = FALSE)
          count = count + 1
          # gc()
          #
          # names(results) = paste0('V', seq_along(electrodes))
          # results = do.call('data.frame', results)
          #
          # # Generate tensor for voltage
          # volt = Tensor$new(0, dim = c(1,1,1), varnames = names(dimnames_volt), hybrid = F)
          #
          # # erase data
          # volt$set_data(NULL)
          # # reset dim and dimnames
          # volt$dim = vapply(dimnames_volt, length, FUN.VALUE = 0, USE.NAMES = F)
          # volt$dimnames = dimnames_volt
          #
          # # generate local cache for volt
          # file = tempfile()
          # raveio::save_fst(results, file, compress = 20)
          # rm(results)
          # gc()
          #
          # # change tensor file path
          # volt$swap_file = file
          # volt$hybrid = T
          # volt$use_index = TRUE
          
          # set to be read-only
          volt$read_only = TRUE
          
          nm = ifelse(referenced, 'volt', 'raw_volt')
          self[[nm]] = volt
        }
        
        return(invisible())
      }else{
        
        results <- dipsaus::lapply_async2(electrodes, function(e){
          electrode = raws[[as.character(e)]]
          electrode$epoch(
            epoch_name = epoch_name,
            pre = pre, post = post, types = data_type, raw = !referenced
          ) -> elc;
          
          if(inherits(elc$power, 'ECoGTensor')){
            elc$power = elc$power$subset(Frequency = freq_subset, drop = FALSE)
          }
          if(inherits(elc$phase, 'ECoGTensor')){
            elc$phase = elc$phase$subset(Frequency = freq_subset, drop = FALSE)
          }
          
          elc = func(elc)
          return(elc)
        }, call_back = function(e){
          sprintf('Preparing electrode - %d', e)
        }, future.chunk.size = 1) 
        gc()
        
        return(results)
      }
      
    },
    
    
    #' @description load references
    #' @param ref_name reference name
    #' @param electrodes electrode numbers
    #' @return none
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
      self$reference$ref_table = ref_table
      
      # load partial references, also avoid invalid electrodes
      ref_table = ref_table[ref_table$Electrode %in% electrodes, ]
      
      # load current cached references
      cached_ref = file.path(self$subject$dirs$channel_dir, 'cache', 'cached_reference.csv')
      if(file.exists(cached_ref)){
        cached_ref = read.csv(cached_ref, stringsAsFactors = F)
        cached_ref = merge(ref_table, cached_ref, by = 'Electrode', all.x = TRUE)
        sel = cached_ref$Reference.x != cached_ref$Reference.y
        ref_table = ref_table[sel, ]
        if(!sum(sel)){
          return(invisible())
        }
      }
      
      
      # Trick: use lazy assign to allocate reference, this is hack to R6 but avoiding evaluations
      # ref_env = self$reference$private$env
      unique_refs = unique(ref_table$Reference)
      
      progress = progress(title = 'Loading reference', 
                          max = length(unique_refs))
      lapply(unique_refs, function(ref){
        progress$inc(ref)
        # delayedAssign(ref, {
        #   Electrode$new(subject = self$subject, electrode = ref, is_reference = T)
        # }, assign.env = ref_env)
        self$reference[[ref]] = Electrode$new(
          subject = self$subject, electrode = ref, is_reference = TRUE)
      })
      progress$close()
      invisible()
    },
    
    
    #' @description baseline signals (deprecated)
    #' @param from,to,electrodes,print.time internally used
    #' @return data after baseline. Please use \code{\link[rave]{baseline}} 
    #' instead
    baseline = function(from, to, electrodes = NULL, print.time = FALSE){
      stopifnot2(!is.null(self$power), msg = 'Please epoch power spectrum first.')
      
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
      bl = collapse(bl, keep = c(1,2,4)) / ntimepts
      
      
      re = ECoGTensor$new(
        data = aperm((aperm(
          self$power$subset(
            Electrode = Electrode %in% electrodes,
            drop = FALSE,
            data_only = TRUE
          ),
          c(1, 2, 4, 3)
        ) / as.vector(bl) - 1) * 100, c(1, 2, 4, 3)),
        dimnames = c(self$power$dimnames[1:3], list(Electrode = electrodes)),
        dim = c(self$power$dim[1:3], length(electrodes)),
        varnames = self$power$varnames
      )
      
      end = Sys.time()
      if(print.time){
        catgl(sprintf('Baseline calculation - %.0f ms', as.numeric(end-start) * 1000))
      }
      return(re)
    }
    
  )
)


