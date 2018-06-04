#' Electrode class (atom)
#' Contains electrode data (matrix: frequency by time)
#' Provide util functions to epoch data
NULL

#' @import rhdf5
#' @export
Electrode <- R6::R6Class(
  classname = 'Electrode',
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    dir = NULL,
    # power = list(),
    # phase = list(),
    # cumsum = list(),
    coef = list(),
    volt = list(),
    cache = list(),
    subject = NULL
  ),
  public = list(
    electrode = NULL,
    subject_id = NULL,
    is_reference = F,

    print = function(...){
      cat('Subject: ', self$subject_id, '\nElectrode: ', self$electrode, '\n', sep = '')
      if(length(private$power) > 0){
        cat('Number of blocks: ', length(private$power), '\n')
        cat('Number of frequencies: ', dim(private$power[[1]])[1], '\n')
      }
      invisible(self)
    },

    initialize = function(subject, electrode, is_reference = F){
      if(!(R6::is.R6(subject) && 'Subject' %in% class(subject))){
        assertthat::assert_that('character' %in% class(subject),
                                msg = 'Param <subject> needs to be either subject ID or a Subject instance')
        subject = str_split_fixed(subject, '\\\\|/', 2)
        subject = Subject$new(project_name = subject[1], subject_code = subject[2])
      }
      private$subject = subject
      subject_id = subject$subject_id

      private$dir = get_dir(subject_code = subject$subject_code, project_name = subject$project_name)
      self$subject_id = subject_id

      self$is_reference = is_reference
      cache_dir = private$dir$cache_dir
      pre_dir = private$dir$preprocess_dir

      if(is_reference){
        self$electrode = 0
        wave_file = file.path(cache_dir, 'reference', sprintf("%s.h5", electrode))
        volt_file = wave_file
        volt_group = '/voltage/'
      }else{
        self$electrode = electrode
        # import file
        wave_file = file.path(cache_dir, 'spectrum', sprintf("%d.h5", electrode))
        volt_file = file.path(pre_dir, sprintf("electrode_%d.h5", electrode))
        volt_group = '/notch/'
      }

      # Get blocks
      blocks = subject$preprocess_info('blocks', customized = F)
      for(b in blocks){
        private$coef[[b]] = load_h5(wave_file, name = '/wavelet/coef/' %&% b)
        private$volt[[b]] = load_h5(volt_file, name = volt_group %&% b)
      }
    },
    # fast_epoch = function(epochs, freqs = NULL, pre = 1, post = 2){
    #   freqs %?<-% private$subject$frequencies
    #   pre = round(pre * private$subject$sample_rate)
    #   post = round(post * private$subject$sample_rate)
    #   time_points = seq(-pre, post) / private$subject$sample_rate
    #   trial_order = order(epochs$Trial)
    #   lapply(1:nrow(epochs), function(i){
    #     epochs[i,]
    #   }) ->
    #     ep
    #
    #   # Epoch
    #   dim_2 = nrow(freqs)   # Freq
    #   dim_3 = post + pre + 1     # Time
    #   sample = matrix(rep(0, dim_2 * dim_3), c(dim_2, dim_3))
    #
    #   logger('Epoching electrode: ', self$electrode, ' (', self$subject_id, ')')
    #
    #
    #   lapply(ep, function(row){
    #     block = row[['Block']]
    #     i = round(row[['Time']] * private$subject$sample_rate)
    #     # logger('Epoching: Block - ', block, ' On-set Point - ', i)
    #     return(list(
    #       ind = i + seq(-pre, post),
    #       block = row$Block
    #     ))
    #   }) ->
    #     indices
    #   placehold = array(NA, dim = c(length(ep), dim_2, dim_3, 1))
    #   env = environment()
    #   bvec = sapply(indices,'[[', 'block')
    #   lapply(unique(bvec), function(b){
    #     sel = bvec == b
    #     subinds = as.vector(sapply(indices[sel], '[[', 'ind'))
    #     coef = private$coef[[b]][,subinds,]
    #     a = coef[,,1] * exp(1i * coef[,,2])
    #     dim(a) = c(nrow(a), dim_3, sum(sel))
    #     env$placehold[trial_order[sel],,, 1] = aperm(a, c(3, 1, 2))
    #     NULL
    #   })
    #   # assign dim names
    #   data = ECoGTensor$new(
    #     data = placehold,
    #     dimnames = list(
    #       Trial = epochs$Trial[trial_order],
    #       Frequency = freqs$Frequency,
    #       Time = time_points,
    #       Electrode = electrode
    #     ),
    #     varnames = c('Trial', 'Frequency', 'Time', 'Electrode'))
    # },
    epoch = function(epoch_name, pre, post, type = c('voltage', 'spectrum')){
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

      epo = function(srate, data_type){
        pre = round(params$pre * srate)
        post = round(params$post * srate)
        time_points = seq(-pre, post) / srate
        # Epoch
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

        if(data_type == 'spectrum'){
          placehold = array(NA, dim = c(length(ep), dim_2, dim_3, 1))
          lapply(unique(bvec), function(b){
            sel = bvec == b
            subinds = as.vector(sapply(indices[sel], '[[', 'ind'))
            coef = private$coef[[b]][,subinds,]
            a = coef[,,1] * exp(1i * coef[,,2])
            dim(a) = c(nrow(a), dim_3, sum(sel))
            env$placehold[trial_order[sel],,, 1] = aperm(a, c(3, 1, 2))
            NULL
          })
          # assign dim names
          data = rave:::ECoGTensor$new(data = placehold, dimnames = list(
            epochs$Trial[trial_order],
            freqs$Frequency,
            time_points,
            electrode
          ), varnames = c('Trial', 'Frequency', 'Time', 'Electrode'))
        }else{
          placehold = array(NA, dim = c(length(ep), dim_3, 1))
          lapply(unique(bvec), function(b){
            sel = bvec == b
            subinds = as.vector(sapply(indices[sel], '[[', 'ind'))
            a = private$volt[[b]][subinds]
            dim(a) = c(dim_3, sum(sel))
            env$placehold[trial_order[sel],, 1] = aperm(a, c(2, 1))
            NULL
          })

          # assign dim names
          data = rave:::Tensor$new(data = placehold, dimnames = list(
            epochs$Trial[trial_order],
            time_points,
            electrode
          ), varnames = c('Trial', 'Time', 'Electrode'))
        }
        return(data)

      }

      if('voltage' %in% type){
        srate = private$subject$preprocess_info('srate')
        logger('Epoching electrode: ', self$electrode, ' (', self$subject_id, ') - voltage')
        re[['voltage']] = epo(srate, data_type = 'voltage')
      }

      if('spectrum' %in% type){
        srate = private$subject$sample_rate
        logger('Epoching electrode: ', self$electrode, ' (', self$subject_id, ') - spectrum')
        re[['spectrum']] = epo(srate, data_type = 'spectrum')
      }

      # private$cache[[cache_name]] = data
      # if(length(private$cache) > 3){
      #   private$cache[[names(private$cache)[1]]] <- NULL
      # }
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
      return(names(private$power))
    }
  )
)
