#' Electrode class (atom)
#' Contains electrode data (matrix: frequency by time)
#' Provide util functions to epoch data
NULL

#' @export
as_printable = function(env){
  assertthat::assert_that(is.environment(env), msg = 'env MUST be an environment.')
  cls = c(class(env), 'rave_printable')
  cls = unique(cls[!cls %in% ''])
  class(env) = cls
  return(env)
}

#' @export
print.rave_printable = function(env){
  assertthat::assert_that(is.environment(env), msg = 'env MUST be an environment.')
  base::print(
    paste0('<environment key=[', paste(ls(env), collapse = ', '), ']>')
  )
  return(env)
}

#' @import rhdf5
#' @export
Electrode <- R6::R6Class(
  classname = 'Electrode',
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    subject = NULL,
    is_reference = F
  ),
  public = list(
    electrode = NULL,
    raw_power = NULL, # before reference
    raw_phase = NULL, # before reference
    raw_volt = NULL, # Voltage before ref
    phase = NULL,  # referenced phase
    power = NULL,  # referenced power
    volt = NULL,   #
    preload = NULL,
    reference = NULL,


    print = function(...){

      cat('Subject: ', self$subject_id, '\nElectrode: ', self$electrode, ifelse(self$reference_electrode, ' (Reference)',''), '\n', sep = '')
      if(length(self$power) > 0){
        cat('Blocks: [', paste(self$blocks, collapse = ', '), ']\n')
      }
      invisible(self)
    },

    switch_reference = function(new_reference){
      assertthat::assert_that(is(new_reference, 'Electrode'), msg = 'new_reference MUST be a reference electrode instance.')
      self$reference = new_reference
      self$preload = NULL
      rm(list = ls(self$power, all.names = T), envir = self$power)
      rm(list = ls(self$phase, all.names = T), envir = self$phase)
      rm(list = ls(self$volt, all.names = T), envir = self$volt)
    },

    referenced = function(type = 'power', ram = T){

      if(is.character(self$reference) || setequal(names(self[[type]]), self$blocks)){
        env = self[[type]]
        if(ram){
          sapply(self$blocks, function(b){
            env[[b]][]
          }, simplify = F, USE.NAMES = T) ->
            env
        }
        return(env)
      }

      # otherwise we need to reference itself

      raw_type = 'raw_' %&% type
      switch (type,
        'volt' = {
          lapply(self$blocks, function(b){
            self$volt[[b]] = self$raw_volt[[b]][] - self$reference$raw_volt[[b]][]
          })
          return(self$volt)
        },
        # default need to have power and phase
        {
          lapply(self$blocks, function(b){
            coef = (sqrt(self$raw_power[[b]][]) * exp(1i * self$raw_phase[[b]][])) -
              sqrt(self$reference$raw_power[[b]]) * exp(1i * self$reference$raw_phase[[b]])
            self$power[[b]] = Mod(coef)^2
            self$phase[[b]] = Arg(coef)
            rm(coef)
            invisible()
          })
          return(self[[type]])
        }
      )

    },

    clean = function(types = c('power', 'phase', 'volt'), force = F){
      if(is(self$reference, 'Electrode')){
        if(!force){
          types = types[!types %in% self$preload]
        }else{
          self$preload = NULL
        }

        for(type in types){
          env = self[[types]]
          rm(list = ls(env, all.names = T), envir = env)
        }
      }
    },

    initialize = function(subject, electrode, reference_by = 'noref', preload = NULL, is_reference = FALSE){
      if(!(R6::is.R6(subject) && 'Subject' %in% class(subject))){
        assertthat::assert_that('character' %in% class(subject),
                                msg = 'Param <subject> needs to be either subject ID or a Subject instance')
        subject = str_split_fixed(subject, '\\\\|/', 2)
        subject = Subject$new(project_name = subject[1], subject_code = subject[2])
      }
      private$subject = subject
      subject_id = subject$subject_id

      # Initialize data
      self$electrode = electrode
      self$raw_power = as_printable(new.env())
      self$raw_phase = as_printable(new.env())
      self$raw_volt = as_printable(new.env())
      self$phase = as_printable(new.env())
      self$power = as_printable(new.env())
      self$volt = as_printable(new.env())
      self$preload = preload


      private$is_reference = is_reference
      cache_dir = subject$dirs$cache_dir

      blocks = subject$preprocess_info('blocks', customized = F)

      # Get reference info
      if(!is_reference){
        file = file.path(cache_dir, 'power', sprintf("%d.h5", electrode))
        ref = load_h5(file, '/reference', ram = T)
        need_ref = F
        if(is.character(reference_by) && reference_by != ref){
          reference_by = Electrode$new(subject, electrode = reference_by, reference_by = NULL, is_reference = T)
        }
        if(methods::is(reference_by, 'Electrode')){
          assertthat::assert_that(reference_by$reference_electrode, msg = 'This is not a reference signal.')
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



      # For each block, link data
      for(b in blocks){
        if(!is_reference){
          # power
          file = file.path(cache_dir, 'power', sprintf("%d.h5", electrode))
          self$raw_power[[b]] = load_h5(file, sprintf('/raw/power/%s', b), ram = ('raw_power' %in% preload))
          if(!need_ref){
            self$power[[b]] = load_h5(file, sprintf('/ref/power/%s', b), ram = ('power' %in% preload))
          }

          # phase
          file = file.path(cache_dir, 'phase', sprintf("%d.h5", electrode))
          self$raw_phase[[b]] = load_h5(file, sprintf('/raw/phase/%s', b), ram = ('raw_phase' %in% preload))
          if(!need_ref){
            self$phase[[b]] = load_h5(file, sprintf('/ref/phase/%s', b), ram = ('phase' %in% preload))
          }

          # voltage
          file = file.path(cache_dir, 'voltage', sprintf("%d.h5", electrode))
          self$raw_volt[[b]] = load_h5(file, sprintf('/raw/voltage/%s', b), ram = ('raw_volt' %in% preload))
          if(!need_ref){
            self$volt[[b]] = load_h5(file, sprintf('/ref/voltage/%s', b), ram = ('volt' %in% preload))
          }

          # If reference_by is an instance of electrode, reference it
          if(need_ref){
            if('volt' %in% preload){
              self$volt[[b]] = self$raw_volt[[b]][] - reference_by$raw_volt[[b]][]
            }
            if(any(c('power', 'phase') %in% preload)){
              coef = sqrt(self$raw_power[[b]][]) * exp(1i * self$raw_phase[[b]][])
              ref_coef = sqrt(reference_by$raw_power[[b]][]) * exp(1i * reference_by$raw_phase[[b]][])
              coef = coef - ref_coef
              if('phase' %in% preload)  self$phase[[b]] = force(Arg(coef))
              if('power' %in% preload)  self$power[[b]] = force((Mod(coef))^2)
              rm(list = c('coef', 'ref_coef'))
            }
          }
        }else{
          # this is reference signal, only load raw_*
          file = file.path(cache_dir, 'reference', sprintf("%s.h5", electrode))
          if(file.exists(file)){
            coef = load_h5(file, name = '/wavelet/coef/' %&% b, ram = T)
            self$raw_power[[b]] = (coef[,,1])^2
            self$raw_phase[[b]] = (coef[,,2])
            self$raw_volt[[b]] = load_h5(file, name = '/voltage/' %&% b, ram = T)
          }else{
            # File not exist, usually this happens to norefs or bipolar refs, therefore, extract first one
            es = str_extract_all(electrode, '[0-9,\\-]+')
            es = unlist(es)
            es = rave:::parse_selections(es)
            es = subject$filter_all_electrodes(es)
            if(length(es)){
              # Bipolar ref
              fname = sprintf("%d.h5", es[1])
              self$raw_power[[b]] = load_h5(file.path(cache_dir, 'power', fname), '/raw/power/' %&% b)[]
              self$raw_phase[[b]] = load_h5(file.path(cache_dir, 'phase', fname), '/raw/phase/' %&% b)[]
              self$raw_volt[[b]] = load_h5(file.path(cache_dir, 'voltage', fname), '/raw/voltage/' %&% b)[]
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

    epoch = function(epoch_name, pre, post, types = c('volt', 'power', 'phase'), raw = F){
      # prepare data
      epochs = load_meta(meta_type = 'epoch', meta_name = epoch_name, project_name = private$subject$project_name, subject_code = private$subject$subject_code)
      freqs = private$subject$frequencies
      trial_order = order(epochs$Trial)
      lapply(1:nrow(epochs), function(i){
        epochs[i,]
      }) ->
        ep

      re = list()
      params = list(
        pre = pre,
        post = post
      )

      electrode = self$electrode

      if(!raw){
        # reference first
        lapply(types, function(type){
          self$referenced(type = type, ram = F)
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
        sample = matrix(rep(0, dim_2 * dim_3), c(dim_2, dim_3))
        lapply(ep, function(row){
          block = row[['Block']]
          i = round(row[['Time']] * srate)
          return(list(
            ind = i + seq(-pre, post),
            block = row$Block
          ))
        }) ->
          indices

        env = environment()
        bvec = sapply(indices,'[[', 'block')

        placehold = array(NA, dim = c(dim_1, dim_3, 1))
        lapply(unique(bvec), function(b){
          sel = bvec == b
          subinds = as.vector(sapply(indices[sel], '[[', 'ind'))
          a = self[[name]][[b]][subinds]
          dim(a) = c(dim_3, sum(sel))
          env$placehold[trial_order[sel],, 1] = aperm(a, c(2, 1))
          NULL
        })

        # assign dim names
        re[['volt']] = rave:::Tensor$new(data = placehold, dimnames = list(
          epochs$Trial[trial_order],
          time_points,
          electrode
        ), varnames = c('Trial', 'Time', 'Electrode'))
        rm(placehold)
      }
      types = types[types != 'volt']
      if(any(c('power', 'phase') %in% types)){
        for(name in types){
          dname = ifelse(raw, 'raw_' %&% name, name)
          srate = private$subject$sample_rate
          pre = round(params$pre * srate)
          post = round(params$post * srate)
          time_points = seq(-pre, post) / srate
          # Epoch
          dim_1 = length(ep)
          dim_2 = nrow(freqs)   # Freq
          dim_3 = post + pre + 1     # Time
          sample = matrix(rep(0, dim_2 * dim_3), c(dim_2, dim_3))
          lapply(ep, function(row){
            block = row[['Block']]
            i = round(row[['Time']] * srate)
            return(list(
              ind = i + seq(-pre, post),
              block = row$Block
            ))
          }) ->
            indices

          env = environment()
          bvec = sapply(indices,'[[', 'block')
          placehold = array(NA, dim = c(dim_1, dim_2, dim_3, 1))

          lapply(unique(bvec), function(b){
            sel = bvec == b
            subinds = as.vector(sapply(indices[sel], '[[', 'ind'))
            a = self[[dname]][[b]][,subinds]
            dim(a) = c(nrow(a), dim_3, sum(sel))
            env$placehold[trial_order[sel],,, 1] = aperm(a, c(3, 1, 2))
            NULL
          })
          # assign dim names
          re[[name]] = rave:::ECoGTensor$new(data = placehold, dimnames = list(
            epochs$Trial[trial_order],
            freqs$Frequency,
            time_points,
            electrode
          ), varnames = c('Trial', 'Frequency', 'Time', 'Electrode'))

        }
        rm(placehold)
      }


      re
    },
    get_data = function(block_num, time = NULL,
                        frequencies = NULL, name = 'power',
                        time_point = NULL,
                        use_ff = F){
      if(is.null(time_point) && !is.null(time)){
        time_point = round(time_point * private$subject$sample_rate)
      }
      if(name %in% c('power', 'phase', 'coef')){
        dat = private$coef[[block_num]][frequencies, time_point]
        switch(
          name,
          coef = {
            return(dat)
          },
          'power' = {
            return((base::Mod(dat))^2)
          },
          'phase' = {
            return(base::Arg(dat))
          }
        )
      }

      return(NULL)
    }
  ),
  active = list(
    blocks = function(){
      private$subject$preprocess_info('blocks', customized = F)
    },
    subject_id = function(){
      private$subject$subject_id
    },
    reference_electrode = function(){
      private$is_reference
    }
  )
)
