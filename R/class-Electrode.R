# Documented 2019-10-12


#' @title R6 Class for Electrode
#' @author Zhengjia Wang
#' @description Stores single electrode or reference signals
#' @examples 
#' \dontrun{
#' # Electrode with no reference
#' e1 <- Electrode$new('demo/YAB', electrode = 14, reference_by = 'noref')
#' e1$reference
#' #> Subject: demo/YAB
#' #> Electrode: noref (Reference)
#' 
#' # Add Common Average Reference in rave/data/reference/ref_13-63,65-84.h5
#' e2 <- Electrode$new('demo/YAB', electrode = 14, 
#'                     reference_by = 'ref_13-63,65-84')
#' 
#' # Electrode with bipolar reference by another electrode
#' e3 <- Electrode$new('demo/YAB', electrode = 14, reference_by = 'ref_15')
#' 
#' # Alternative way
#' reference <- Electrode$new('demo/YAB', electrode = 15, is_reference = TRUE)
#' e4 <- Electrode$new('demo/YAB', electrode = 14, reference_by = reference)
#' 
#' # e3, e4 are the same in scientific meaning. To test it, epoch them
#' power3 <- e3$epoch('YABaOutlier', 1, 2, 'power', 
#'                    raw = FALSE, hybrid = FALSE)$power
#' power4 <- e4$epoch('YABaOutlier', 1, 2, 'power',
#'                    raw = FALSE, hybrid = TRUE)$power
#' 
#' # Compare e3 and e4, result difference should be 0
#' range(power3$get_data() - power4$get_data())
#' #> 0
#' 
#' # With or without hybrid, the size will be different
#' # No hybrid, totally in memory
#' lobstr::obj_size(power3)
#' #> 12 MB
#' # Hybrid, data is swapped to hard-drive
#' lobstr::obj_size(power4)
#' #> 908 kB
#' }
#' @export
Electrode <- R6::R6Class(
  classname = 'Electrode',
  portable = FALSE,
  cloneable = FALSE, 
  parent_env = asNamespace('rave'),
  private = list(
    subject = NULL,
    is_reference = FALSE
  ),
  public = list(
    
    #' @field electrode electrode number in integer
    electrode = NULL,
    
    #' @field raw_power stores pre-epoch power spectrum with no reference
    raw_power = NULL, # before reference
    
    #' @field raw_phase stores pre-epoch phase with no reference
    raw_phase = NULL, # before reference
    
    #' @field raw_volt stores pre-epoch analog traces with no reference
    raw_volt = NULL, # Voltage before ref
    
    #' @field phase stores pre-epoch phase after reference
    phase = NULL,  # referenced phase
    
    #' @field power stores pre-epoch power spectrum after reference
    power = NULL,  # referenced power
    
    #' @field volt stores pre-epoch analog traces after reference
    volt = NULL,   #
    
    #' @field preload which of the three data are pre-loaded
    preload = NULL,
    
    #' @field reference character or \code{Electrode} instance indicating the 
    #' reference for current electrode
    reference = NULL,
    
    #' @field has_power,has_phase,has_volt whether power, phase, or voltage
    #' data exist in file system before and after reference
    has_power = c(TRUE, TRUE),
    has_phase = c(TRUE, TRUE),
    has_volt = c(TRUE, TRUE),
    
    #' @description print electrode information
    #' @return none
    info = function(){
      cat('Subject: ', self$subject_id, '\nElectrode: ', self$electrode, ifelse(self$reference_electrode, ' (Reference)',''), '\n', sep = '')
      if(length(self$power) > 0){
        cat('Blocks: [', paste(self$blocks, collapse = ', '), ']\n')
      }
    },
    
    #' @description overrides default print method
    #' @param ... ignored
    #' @return none
    print = function(...){
      self$info()
    },
    
    
    #' @description switch reference (experimental)
    #' @param new_reference An electrode instance
    #' @return none
    switch_reference = function(new_reference){
      stopifnot2(inherits(new_reference, 'Electrode'), msg = 'new_reference MUST be a reference electrode instance.')
      self$reference = new_reference
      self$preload = NULL
      .subset2(self$power, 'reset')()
      .subset2(self$phase, 'reset')()
      .subset2(self$volt, 'reset')()
      invisible()
    },
    
    #' @description get referenced data
    #' @param type which data to reference, default is power
    #' @param ram whether to load data to memory
    #' @return If \code{ram} is true, then returns a list of matrices. The 
    #' length of the list equals the number of blocks, and each matrix is 
    #' frequency by time points. If \code{ram} is false, then returns an 
    #' environment with each element a \code{\link[raveio]{LazyH5}} or 
    #' \code{\link[raveio]{LazyFST}} instance.
    referenced = function(type = 'power', ram = TRUE){
      
      if(is.character(self$reference) || setequal(names(self[[type]]), self$blocks)){
        env = as.list(self[[type]])
        if(ram){
          sapply(self$blocks, function(b){
            env[[b]][]
          }, simplify = FALSE, USE.NAMES = TRUE) ->
            env
        }
        return(env)
      }
      
      # otherwise we need to reference itself
      
      raw_type = paste0('raw_', type)
      if('Electrode' %in% class(self$reference) && self$reference$electrode == 'noref'){
        # not very usual, basically cached as referenced but need to load noref version
        
        for( b in self$blocks ){
          self[[type]][[b]] = self[[raw_type]][[b]]
        }
        env = as.list(self[[type]])
        
        if(ram){
          sapply(self$blocks, function(b){
            env[[b]][]
          }, simplify = FALSE, USE.NAMES = TRUE) ->
            env
        }
        
        return(env)
      }
      switch (type,
              'volt' = {
                lapply(self$blocks, function(b){
                  self$volt[[b]] = self$raw_volt[[b]][] - self$reference$raw_volt[[b]][]
                  # self$volt[[b]] = self$raw_volt[[b]][] - self$reference$raw_volt[[b]][]
                })
                return(as.list(self$volt))
              },
              # default need to have power and phase
              {
                lapply(self$blocks, function(b){
                  coef = (sqrt(self$raw_power[[b]][]) * exp(1i * self$raw_phase[[b]][])) -
                    (sqrt(self$reference$raw_power[[b]]) * exp(1i * self$reference$raw_phase[[b]]))
                  self$power[[b]] =  Mod(coef)^2
                  self$phase[[b]] = Arg(coef)
                  rm(coef)
                  invisible()
                })
                return(as.list(self[[type]]))
              }
      )
      
    },
    
    #' @description remove data from memory
    #' @param types data types to clean
    #' @param force whether to remove pre-loaded data types
    #' @return none
    clean = function(types = c('power', 'phase', 'volt'), force = FALSE){
      if(is(self$reference, 'Electrode')){
        if(!force){
          types = types[!types %in% self$preload]
        }else{
          self$preload = NULL
        }
        
        for(type in types){
          .subset2(self[[type]], 'reset')()
        }
      }
      invisible()
    },
    
    
    #' @description constructor
    #' @param subject \code{\link[rave]{Subject}} instance or characters like 
    #' \code{"proj/sub"}
    #' @param electrode number, integer
    #' @param reference_by reference signals, choices are character, or 
    #' \code{\link[rave]{Electrode}} instance; default is \code{"noref"}, 
    #' meaning no reference to the electrode
    #' @param preload data to load along with constructor
    #' @param is_reference is current instance a reference?
    #' @return An \code{\link[rave]{Electrode}} instance
    initialize = function(subject, electrode, reference_by = 'noref', 
                          preload = NULL, is_reference = FALSE){
      if(!(R6::is.R6(subject) && 'Subject' %in% class(subject))){
        stopifnot2('character' %in% class(subject),
                    msg = 'Param <subject> needs to be either subject ID or a Subject instance')
        subject = stringr::str_split_fixed(subject, '\\\\|/', 2)
        subject = Subject$new(project_name = subject[1], subject_code = subject[2], strict = FALSE)
      }
      private$subject = subject
      subject_id = subject$subject_id
      
      # Initialize data
      self$electrode = electrode
      self$raw_power = dipsaus::fastmap2()
      self$raw_phase = dipsaus::fastmap2()
      self$raw_volt = dipsaus::fastmap2()
      self$phase = dipsaus::fastmap2()
      self$power = dipsaus::fastmap2()
      self$volt = dipsaus::fastmap2()
      self$preload = preload
      
      
      private$is_reference = is_reference
      cache_dir = subject$dirs$cache_dir
      
      blocks = subject$preprocess_info('blocks', customized = FALSE)
      
      # Get reference info
      if(!is_reference){
        
        # check cached reference table first
        file = file.path(cache_dir, 'cache', 'cached_reference.csv')
        ref = NULL
        if(file.exists(file)){
          tbl = read.csv(file, stringsAsFactors = FALSE)
          if(electrode %in% tbl$Electrode){
            ref = tbl$Reference[tbl$Electrode == electrode]
          }
        }
        if(is.null(ref)){
          # try to load cached refs from h5 files
          # Since we allow users to have non-strict mode, and missing data is allowed.
          file = file.path(cache_dir, 'power', sprintf("%d.h5", electrode))
          if(!file.exists(file)){
            file = file.path(cache_dir, 'phase', sprintf("%d.h5", electrode))
          }
          if(!file.exists(file)){
            file = file.path(cache_dir, 'voltage', sprintf("%d.h5", electrode))
          }
          ref = load_h5(file, '/reference', ram = TRUE)
        }
        
        
        need_ref = FALSE
        if(is.character(reference_by) && reference_by != ref){
          reference_by = Electrode$new(subject, electrode = reference_by, reference_by = NULL, is_reference = TRUE)
        }
        if(inherits(reference_by, 'Electrode')){
          stopifnot2(reference_by$reference_electrode, msg = 'This is not a reference signal.')
          need_ref = (reference_by$electrode != ref)
        }
        if(need_ref){
          self$reference = reference_by
        }else{
          rm(reference_by)
          reference_by = ref
          self$reference = ref
        }
      }
      
      # tmp function to load data
      # t: type: power, phase, or volt
      # b: block
      # e: electrode
      load_data_ = function(t, b, e){
        if(t == 'voltage'){
          ts = 'volt'
          fst_need_transpose = FALSE
          fst_need_drop = TRUE
          ram_raw = any(sprintf('raw_%s', c(ts, t)) %in% preload)
          ram_ref = any(c(ts, t) %in% preload)
        }else{
          ts = t
          fst_need_transpose = TRUE
          fst_need_drop = FALSE
          ram_raw = (sprintf('raw_%s', ts) %in% preload)
          ram_ref = (ts %in% preload)
        }
        h5_path = file.path(cache_dir, t, sprintf("%d.h5", e))
        fst_path_raw = file.path(cache_dir, 'cache', t, 'raw', b, sprintf("%d.fst", e))
        fst_path_ref = file.path(cache_dir, 'cache', t, 'ref', b, sprintf("%d.fst", e))
        has_file_raw = file.exists(h5_path) || file.exists(fst_path_raw)
        has_file_ref = file.exists(h5_path) || file.exists(fst_path_ref)
        
        h = self[[paste0('has_', ts)]] = self[[paste0('has_', ts)]] & c(has_file_raw, has_file_ref)
        if(h[1]){
          # import raw data
          self[[sprintf('raw_%s', ts)]][[b]] = load_fst_or_h5(
            fst_path = fst_path_raw,
            h5_path = h5_path,
            h5_name = sprintf('/raw/%s/%s', t, b),
            fst_need_transpose = fst_need_transpose,
            fst_need_drop = fst_need_drop,
            ram = ram_raw
          )
        }
        
        if(!need_ref && h[2]){
          # load referenced cache
          self[[ts]][[b]] = load_fst_or_h5(
            fst_path = fst_path_ref,
            h5_path = h5_path,
            h5_name = sprintf('/ref/%s/%s', t, b),
            fst_need_transpose = fst_need_transpose,
            fst_need_drop = fst_need_drop,
            ram = ram_ref
          )
        }
        
      }
      
      # For each block, link data
      for(b in blocks){
        if(!is_reference){
          # Power, load raw
          load_data_(t = 'power', b = b, e = electrode)
          
          # original code: load from non-cached
          #
          # self$raw_power[[b]] = load_h5(file, sprintf('/raw/power/%s', b), ram = ('raw_power' %in% preload))
          # if(!need_ref){
          #   self$power[[b]] = load_h5(file, sprintf('/ref/power/%s', b), ram = ('power' %in% preload))
          # }
          #
          # # test code: uncomment the lines below
          # #
          #
          # preload = c('power'); preload = c(preload, paste0('raw_', preload))
          # range(self$raw_power[[b]] - load_h5(h5_path, sprintf('/raw/power/%s', b))[])
          # range(self$power[[b]] - load_h5(h5_path, sprintf('/ref/power/%s', b))[])
          
          
          
          # phase: load from cache or, if cache miss, load from original h5
          # h5_path = file.path(cache_dir, 'phase', sprintf("%d.h5", electrode))
          # self$raw_phase[[b]] = load_fst_or_h5(
          #   fst_path = file.path(cache_dir, 'cache', 'phase', 'raw', b, sprintf("%d.fst", electrode)),
          #   h5_path = h5_path,
          #   h5_name = sprintf('/raw/phase/%s', b),
          #   fst_need_transpose = T,
          #   fst_need_drop = F,
          #   ram = ('raw_power' %in% preload)
          # )
          #
          # if(!need_ref){
          #   self$phase[[b]] = load_fst_or_h5(
          #     fst_path = file.path(cache_dir, 'cache', 'phase', 'ref', b, sprintf("%d.fst", electrode)),
          #     h5_path = h5_path,
          #     h5_name = sprintf('/ref/phase/%s', b),
          #     fst_need_transpose = T,
          #     fst_need_drop = F,
          #     ram = ('phase' %in% preload)
          #   )
          # }
          load_data_(t = 'phase', b = b, e = electrode)
          
          # # original code: load from non-cached
          # file = file.path(cache_dir, 'phase', sprintf("%d.h5", electrode))
          # self$raw_phase[[b]] = load_h5(file, sprintf('/raw/phase/%s', b), ram = ('raw_phase' %in% preload))
          # if(!need_ref){
          #   self$phase[[b]] = load_h5(file, sprintf('/ref/phase/%s', b), ram = ('phase' %in% preload))
          # }
          # # test code: uncomment the lines below
          # #
          #
          # preload = c('phase'); preload = c(preload, paste0('raw_', preload))
          # range(self$raw_phase[[b]] - load_h5(h5_path, sprintf('/raw/phase/%s', b), ram = ('raw_phase' %in% preload))[])
          # range(self$phase[[b]] - load_h5(h5_path, sprintf('/ref/phase/%s', b))[])
          
          
          
          # voltage: load from cache or, if cache miss, load from original h5
          # h5_path = file.path(cache_dir, 'voltage', sprintf("%d.h5", electrode))
          # self$raw_volt[[b]] = load_fst_or_h5(
          #   fst_path = file.path(cache_dir, 'cache', 'voltage', 'raw', b, sprintf("%d.fst", electrode)),
          #   h5_path = h5_path,
          #   h5_name = sprintf('/raw/voltage/%s', b),
          #   fst_need_transpose = F,
          #   fst_need_drop = T,
          #   ram = ('raw_volt' %in% preload)
          # )
          #
          # if(!need_ref){
          #   self$volt[[b]] = load_fst_or_h5(
          #     fst_path = file.path(cache_dir, 'cache', 'voltage', 'ref', b, sprintf("%d.fst", electrode)),
          #     h5_path = h5_path,
          #     h5_name = sprintf('/ref/voltage/%s', b),
          #     fst_need_transpose = F,
          #     fst_need_drop = T,
          #     ram = ('voltage' %in% preload)
          #   )
          # }
          load_data_(t = 'voltage', b = b, e = electrode)
          
          # # original code: load from non-cached
          # file = file.path(cache_dir, 'voltage', sprintf("%d.h5", electrode))
          # self$raw_volt[[b]] = load_h5(file, sprintf('/raw/voltage/%s', b), ram = ('raw_volt' %in% preload))
          # if(!need_ref){
          #   self$volt[[b]] = load_h5(file, sprintf('/ref/voltage/%s', b), ram = ('volt' %in% preload))
          # }
          # # test code: uncomment the lines below
          # #
          #
          # range(self$raw_volt[[b]] - load_h5(h5_path, sprintf('/raw/voltage/%s', b))[])
          # range(self$volt[[b]] - load_h5(h5_path, sprintf('/ref/voltage/%s', b))[])
          
          # If reference_by is an instance of electrode, reference it
          if(need_ref){
            if('volt' %in% preload){
              self$volt[[b]] = self$raw_volt[[b]][] - reference_by$raw_volt[[b]][]
            }
            if(any(c('power', 'phase') %in% preload)){
              coef = sqrt(self$raw_power[[b]][]) * exp(1i * self$raw_phase[[b]][])
              ref_coef = sqrt(reference_by$raw_power[[b]][]) * exp(1i * reference_by$raw_phase[[b]][])
              coef = coef - ref_coef
              if('phase' %in% preload)  self$phase[[b]] = Arg(coef)
              if('power' %in% preload)  self$power[[b]] = (Mod(coef))^2
              rm(list = c('coef', 'ref_coef'))
            }
          }
        }else{
          # this is reference signal, only load raw_*
          file = file.path(cache_dir, 'reference', sprintf("%s.h5", electrode))
          if(file.exists(file)){
            # try to load coef from cache
            fst_coef = file.path(cache_dir, 'cache', 'reference', 'coef', b, sprintf("%s.fst", electrode))
            fst_phase = file.path(cache_dir, 'cache', 'reference', 'phase', b, sprintf("%s.fst", electrode))
            if(file.exists(fst_coef) && file.exists(fst_phase)){
              # load from cached reference
              self$raw_power[[b]] = t(as.matrix(raveio::load_fst(fst_coef)))^2
              self$raw_phase[[b]] = t(as.matrix(raveio::load_fst(fst_phase)))
              # test, result should be 0 0
              # coef = load_h5(file, name = paste0('/wavelet/coef/', b), ram = T)
              # range(self$raw_power[[b]] - (coef[,,1])^2)
              # range(self$raw_phase[[b]] - (coef[,,2]))
            }else{
              coef = load_h5(file, name = paste0('/wavelet/coef/', b), ram = TRUE)
              self$raw_power[[b]] = (coef[,,1])^2
              self$raw_phase[[b]] = (coef[,,2])
            }
            
            # volt
            self$raw_volt[[b]] = load_fst_or_h5(
              fst_path = file.path(cache_dir, 'cache', 'reference', 'voltage', b, sprintf("%s.fst", electrode)),
              h5_path = file,
              h5_name = paste0('/voltage/', b),
              fst_need_transpose = FALSE,
              fst_need_drop = TRUE,
              ram = TRUE
            )
            # test
            # range(self$raw_volt[[b]] - load_h5(file, name = '/voltage/' %&% b, ram = T))
          }else{
            # File not exist, usually this happens to norefs or bipolar refs, therefore, extract first one
            es = stringr::str_extract_all(electrode, '[0-9,\\-]+')
            es = unlist(es)
            es = dipsaus::parse_svec(es)
            es = subject$filter_all_electrodes(es)
            if(length(es)){
              # Bipolar ref
              h5_path = sprintf("%d.h5", es[1])
              fst_path = sprintf("%d.fst", es[1])
              
              self$raw_power[[b]] = load_fst_or_h5(
                fst_path = file.path(cache_dir, 'cache', 'power', 'raw', b, fst_path),
                h5_path = file.path(cache_dir, 'power', h5_path),
                h5_name = paste0('/raw/power/', b),
                fst_need_transpose = TRUE,
                fst_need_drop = FALSE,
                ram = TRUE
              )
              # test: should be 0 0
              # range(self$raw_power[[b]] - load_h5(file.path(cache_dir, 'power', h5_path), '/raw/power/' %&% b, ram = T))
              
              # original code
              # self$raw_power[[b]] = load_h5(file.path(cache_dir, 'power', h5_path), '/raw/power/' %&% b, ram = T)
              
              
              self$raw_phase[[b]] = load_fst_or_h5(
                fst_path = file.path(cache_dir, 'cache', 'phase', 'raw', b, fst_path),
                h5_path = file.path(cache_dir, 'phase', h5_path),
                h5_name = paste0('/raw/phase/', b),
                fst_need_transpose = TRUE,
                fst_need_drop = FALSE,
                ram = TRUE
              )
              # test: should be 0 0
              # range(self$raw_phase[[b]] - load_h5(file.path(cache_dir, 'phase', h5_path), '/raw/phase/' %&% b, ram = T))
              
              # original code
              # self$raw_phase[[b]] = load_h5(file.path(cache_dir, 'phase', h5_path), '/raw/phase/' %&% b, ram = T)
              
              
              self$raw_volt[[b]] = load_fst_or_h5(
                fst_path = file.path(cache_dir, 'cache', 'voltage', 'raw', b, fst_path),
                h5_path = file.path(cache_dir, 'voltage', h5_path),
                h5_name = paste0('/raw/voltage/', b),
                fst_need_transpose = FALSE,
                fst_need_drop = TRUE,
                ram = TRUE
              )
              
              # test: should be 0 0
              # range(self$raw_volt[[b]] - load_h5(file.path(cache_dir, 'voltage', h5_path), '/raw/voltage/' %&% b, ram = T))
              
              # original code
              # self$raw_volt[[b]] = load_h5(file.path(cache_dir, 'voltage', h5_path), '/raw/voltage/' %&% b, ram = T)
            }else{
              # Noref or bad electrodes
              # this is a special reference where power, volt, phase = 0
              self$raw_power[[b]] = 0
              self$raw_phase[[b]] = 0
              self$raw_volt[[b]] = 0
            }
          }
        }
      }
      
    },
    
    #' @description epoch electrode
    #' @param epoch_name epoch name, for example, \code{epoch_name="default"}
    #' refers to epoch file \code{"epoch_default.csv"} in subject meta folder
    #' @param pre seconds before trial onset to load
    #' @param post seconds after trial onset to load
    #' @param types characters, data types to load; choices are \code{"volt"},
    #' \code{"power"}, and \code{"phase"}
    #' @param raw whether epoch pre-referenced data?
    #' @param hybrid whether to fast-cache the data on hard-drive? See also 
    #' \code{\link[raveio]{Tensor}}
    #' @return list of data after epoch
    epoch = function(epoch_name, pre, post, types = c('volt', 'power', 'phase'),
                     raw = FALSE, hybrid = TRUE){
      
      # epoch_name='YABa';pre=1;post=2;name=types='power';raw=FALSE;private=self$private;rave_id = 'TEMP'
      
      instance_id = add_to_session(getDefaultReactiveDomain(), key = 'rave_instance', val = 'TEMP', override = FALSE)
      instance_id %?<-% 'TEMP'
      cache_root_dir = file.path(subject_cache_dir(), instance_id)
      # prepare data
      epochs = load_meta(meta_type = 'epoch', meta_name = epoch_name, project_name = private$subject$project_name, subject_code = private$subject$subject_code)
      freqs = private$subject$frequencies
      trial_order = order(epochs$Trial)
      ep = lapply(seq_len(nrow(epochs)), function(i){
        epochs[i,]
      }) 
      
      re = list()
      params = list( pre = pre, post = post )
      
      electrode = self$electrode
      
      if(!raw){
        # reference first
        lapply(types, function(type){
          self$referenced(type = type, ram = FALSE)
          NULL
        })
        on.exit({self$clean(types = types)})
      }
      
      re = list(raw = raw)
      
      # Voltage
      if('volt' %in% types){
        name = ifelse(raw, 'raw_volt', 'volt')
        srate = private$subject$preprocess_info(key = 'srate')
        pre = round(params$pre * srate)
        post = round(params$post * srate)
        time_points = seq(-pre, post) / srate
        # Epoch
        dim_1 = length(ep)
        dim_2 = nrow(freqs)   # Freq
        dim_3 = post + pre + 1     # Time
        # sample = matrix(rep(0, dim_1 * dim_3), c(dim_1, dim_3))
        indices <- lapply(ep, function(row){
          block = row[['Block']]
          i = round(row[['Time']] * srate)
          return(list(
            ind = i + seq(-pre, post),
            block = row$Block
          ))
        })
        
        # env = environment()
        bvec = sapply(indices,'[[', 'block')
        
        # placehold = array(NA, dim = c(dim_1, dim_3, 1))
        # lapply(unique(bvec), function(b){
        #   sel = bvec == b
        #   subinds = as.vector(sapply(indices[sel], '[[', 'ind'))
        #   a = self[[name]][[b]][subinds]
        #   dim(a) = c(dim_3, sum(sel))
        #   env$placehold[trial_order[sel],, 1] = aperm(a, c(2, 1))
        #   NULL
        # })
        
        placehold = do.call(c, lapply(unique(bvec), function(b){
          sel = bvec == b
          subinds = as.vector(sapply(indices[sel], '[[', 'ind'))
          vec_len = length(self[[name]][[b]])
          bad_part = (subinds <= 0) | (subinds > vec_len)
          has_bad_part = any(bad_part)
          if(has_bad_part){
            subinds[bad_part] = 1
          }
          a = as.vector(self[[name]][[b]][subinds])
          if(has_bad_part){
            a[bad_part] = NA#median(a[!bad_part], na.rm = TRUE)
          }
          a
        }))
        dim(placehold) = c(dim_3, length(placehold) / dim_3)
        
        placehold = t(placehold)
        # reorder
        order = do.call(c, lapply(unique(bvec), function(b){ which(bvec == b) }))
        if(is.unsorted(order)){
          placehold = placehold[order,,drop = FALSE]
        }
        
        if(self$reference_electrode){
          
          swap_path = file.path(cache_root_dir, self$subject_id, self$electrode, epoch_name)
        }else{
          if(!length(self$reference) || is.character(self$reference)){
            refname = self$reference
          }else{
            refname = self$reference$electrode
          }
          if(refname == ''){ refname = 'noref' }
          
          swap_path = file.path(cache_root_dir, self$subject_id, self$electrode, epoch_name, refname)
        }
        
        dir.create(swap_path, recursive = TRUE, showWarnings = FALSE)
        
        # assign dim names
        tmp <- Tensor$new(
          data = placehold,
          dim = c(dim(placehold), 1),
          dimnames = list(
            epochs$Trial[trial_order],
            time_points,
            electrode
          ),
          varnames = c('Trial', 'Time', 'Electrode'),
          hybrid = hybrid, use_index = FALSE, multi_files = FALSE,
          temporary = FALSE, swap_file = file.path(swap_path, name))
        # tmp$to_swap_now(use_index = FALSE)
        tmp$temporary = FALSE
        re[['volt']] = tmp
        
        rm(placehold)
      }
      types = types[types != 'volt']
      if(any(c('power', 'phase') %in% types)){
        for(name in types){
          dname = ifelse(raw, paste0('raw_', name), name)
          srate = private$subject$sample_rate
          pre = round(params$pre * srate)
          post = round(params$post * srate)
          time_points = seq(-pre, post) / srate
          # Epoch
          dim_1 = length(ep)
          dim_2 = nrow(freqs)   # Freq
          dim_3 = post + pre + 1     # Time
          # sample = matrix(rep(0, dim_2 * dim_3), c(dim_2, dim_3))
          indices <- lapply(ep, function(row){
            block = row[['Block']]
            i = round(row[['Time']] * srate)
            return(list(
              ind = i + seq(-pre, post),
              block = row$Block
            ))
          })
          
          # env = environment()
          bvec = sapply(indices,'[[', 'block')
          # placehold = array(NA, dim = c(dim_1, dim_2, dim_3, 1))
          
          # lapply(unique(bvec), function(b){
          #   sel = bvec == b
          #   subinds = as.vector(sapply(indices[sel], '[[', 'ind'))
          #   a = self[[dname]][[b]][,subinds]
          #   dim(a) = c(nrow(a), dim_3, sum(sel))
          #   env$placehold[trial_order[sel],,, 1] = aperm(a, c(3, 1, 2))
          #   NULL
          # })
          
          placehold = do.call(cbind, lapply(unique(bvec), function(b){
            sel = bvec == b
            subinds = as.vector(sapply(indices[sel], '[[', 'ind'))
            
            ncols = dim(self[[dname]][[b]])[[2]]
            bad_part = (subinds <= 0) | (subinds > ncols)
            has_bad_part = any(bad_part)
            if(has_bad_part){
              subinds[bad_part] = 1
            }
            a = self[[dname]][[b]][,subinds, drop = FALSE]
            if(has_bad_part){
              a[,bad_part] = NA#median(a[,!bad_part], na.rm = TRUE)
            }
            a
          }))
          dim(placehold) = c(dim(placehold)[1], dim_3, dim(placehold)[2] / dim_3, 1)
          
          placehold = aperm(placehold, c(3, 1, 2, 4))
          # reorder
          order = do.call(c, lapply(unique(bvec), function(b){ which(bvec == b) }))
          if(is.unsorted(order)){
            placehold = placehold[order,,,,drop = F]
          }
          
          # assign dim names
          dimnames = list(
            epochs$Trial[trial_order],
            freqs$Frequency,
            time_points,
            electrode
          )
          
          
          
          if(self$reference_electrode){
            
            swap_path = file.path(
              cache_root_dir, self$subject_id,
              self$electrode, epoch_name
            )
          }else{
            if(!length(self$reference) || is.character(self$reference)){
              refname = self$reference
            }else{
              refname = self$reference$electrode
            }
            if(refname == ''){ refname = 'noref' }
            
            swap_path = file.path(
              cache_root_dir, self$subject_id,
              self$electrode, epoch_name, refname
            )
          }
          
          dir.create(swap_path, recursive = TRUE, showWarnings = FALSE)
          
          tmp <- ECoGTensor$new(
            data = placehold, dimnames = dimnames,
            dim = sapply(dimnames, length),
            varnames = c('Trial', 'Frequency', 'Time', 'Electrode'),
            hybrid = hybrid, use_index = FALSE, multi_files = FALSE,
            temporary = FALSE, swap_file = file.path(swap_path, dname))
          # tmp$to_swap_now(use_index = FALSE)
          tmp$temporary = FALSE
          re[[name]] = tmp
          
        }
        rm(placehold)
      }
      
      
      re
    }
  ),
  active = list(
    #' @field blocks character vector of block names (read-only)
    blocks = function(){
      private$subject$preprocess_info('blocks', customized = F)
    },
    
    #' @field subject_id character of subject ID (read-only)
    subject_id = function(){
      private$subject$subject_id
    },
    
    #' @field reference_electrode whether this is a reference (read-only)
    reference_electrode = function(){
      private$is_reference
    }
  )
)
