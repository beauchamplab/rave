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
    power = list(),
    phase = list(),
    cumsum = list(),
    cache = list(),
    subject = NULL
  ),
  public = list(
    electrode = NULL,
    subject_id = NULL,

    print = function(...){
      cat('Subject: ', self$subject_id, '\nElectrode: ', self$electrode, '\n', sep = '')
      if(length(private$power) > 0){
        cat('Number of blocks: ', length(private$power), '\n')
        cat('Number of frequencies: ', dim(private$power[[1]])[1], '\n')
      }
      invisible(self)
    },

    initialize = function(subject, electrode){
      if(!(R6::is.R6(subject) && 'Subject' %in% class(subject))){
        assertthat::assert_that('character' %in% class(subject),
                                msg = 'Param <subject> needs to be either subject ID or a Subject instance')
        subject = Subject$new(subject_id = subject)
      }
      private$subject = subject
      subject_id = subject$subject_id

      private$dir = get_dir(subject_id = subject_id)
      self$electrode = electrode
      self$subject_id = subject_id

      # import file
      cache_dir = private$dir$cache_dir
      file = file.path(cache_dir, sprintf("%d.h5", electrode))
      file_ls = h5ls(file)

      # load power data
      rows = (file_ls$group == '/wavelet/power')
      if(sum(rows) > 0){
        blocks = file_ls$name[rows]
        cached_dir = private$dir$cache_dir

        for(name in c('power', 'phase', 'cumsum')){
          li = get(name, envir = private)
          for(b in blocks){
            cached_name = sprintf('%d_%s.%s', self$electrode, b, name)
            cached_file = file.path(cached_dir, sprintf('%s.ff', cached_name))
            if(file.exists(cached_file)){
              li[[b]] = rave:::load_ff(name = cached_name, directory = cached_dir)
            }else{
              li[[b]] =
                rave:::as_ff(
                  h5read(file, sprintf('/wavelet/%s/%s', name, b)),
                  directory = cached_dir,
                  name = cached_name
                )
            }
          }
          assign(name, li, envir = private)
        }


        # phase
        # for(b in blocks){
        #   cached_name = sprintf('%d.phase', self$electrode)
        #   cached_file = file.path(cached_dir, sprintf('%s.ff', cached_name))
        #   if(file.exists(cached_file)){
        #     private$phase[[b]] = rave:::load_ff(name = cached_name, directory = cached_dir)
        #   }else{
        #     private$phase[[b]] =
        #       rave:::as_ff(
        #         h5read(file, sprintf('/wavelet/phase/%s', b)),
        #         directory = cached_dir,
        #         name = cached_name
        #       )
        #   }
        # }
      }
    },
    fast_epoch = function(epochs, freqs, pre, post, name = 'power'){
      pre = round(pre * private$subject$sample_rate)
      post = round(post * private$subject$sample_rate)
      time_points = seq(-pre, post) / private$subject$sample_rate
      lapply(1:nrow(epochs), function(i){
        epochs[i,]
      }) ->
        ep

      # Epoch
      tmp = get(name, envir = private)
      dim_2 = nrow(freqs)   # Freq
      dim_3 = post + pre + 1     # Time
      sample = matrix(rep(0, dim_2 * dim_3), c(dim_2, dim_3))

      logger('Epoching electrode: ', self$electrode, ' (', self$subject_id, ') - ', name)
      vapply(ep, function(row){
        block = row[['Block']]
        i = round(row[['Onset']] * private$subject$sample_rate)
        # logger('Epoching: Block - ', block, ' On-set Point - ', i)
        tmp[[block]][, i + seq(-pre, post)]
      }, sample) ->
        data
      # assign dim names
      data = rave:::ECoGTensor$new(
        data = aperm(data, c(3,1,2)),
        dimnames = list(
          epochs$Stimulus,
          freqs$Frequency,
          time_points
        ),
        varnames = c('Trial', 'Frequency', 'Time'))
    },
    epoch = function(epoch_name, pre, post, name = 'power'){
      pre = round(pre * private$subject$sample_rate)
      post = round(post * private$subject$sample_rate)
      time_points = seq(-pre, post) / private$subject$sample_rate

      epochs = load_meta(subject_id = self$subject_id, meta_type = 'epoch', meta_name = epoch_name)
      freqs = load_meta(subject_id = self$subject_id, meta_type = 'frequencies')

      lapply(1:nrow(epochs), function(i){
        epochs[i,]
      }) ->
        ep

      # Epoch
      tmp = get(name, envir = private)
      dim_2 = dim(tmp[[1]])[1]   # Freq
      dim_3 = post + pre + 1     # Time
      sample = matrix(rep(0, dim_2 * dim_3), c(dim_2, dim_3))

      logger('Epoching electrode: ', self$electrode, ' (', self$subject_id, ') - ', name)
      vapply(ep, function(row){
        block = row[['Block']]
        i = round(row[['Onset']] * private$subject$sample_rate)
        # logger('Epoching: Block - ', block, ' On-set Point - ', i)
        tmp[[block]][, i + seq(-pre, post)]
      }, sample) ->
        data

      data = aperm(data, c(3,1,2))

      # assign dim names
      data = rave:::ECoGTensor$new(data = data, dimnames = list(
        epochs$Stimulus,
        freqs$Frequency,
        time_points
      ), varnames = c('Trial', 'Frequency', 'Time'))

      # private$cache[[cache_name]] = data
      # if(length(private$cache) > 3){
      #   private$cache[[names(private$cache)[1]]] <- NULL
      # }
      data
    },
    get_data = function(block_num, time = NULL,
                        frequencies = NULL, name = 'power',
                        time_point = NULL,
                        use_ff = T){
      if(is.null(time_point) && !is.null(time)){
        time_point = round(time_point * private$subject$sample_rate)
      }
      if(name %in% c('power', 'cumsum', 'phase')){
        if(!use_ff){
          cache_dir = private$dir$cache_dir
          file = file.path(cache_dir, sprintf("%d.h5", self$electrode))
          file_ls = h5ls(file)
          re = h5read(file, name = sprintf('/wavelet/%s/%s', name, block_num),
                      index = list(frequencies, time_point))
          H5close()
          return(re)
        }else{
          li = get(name, envir = private)
          if(is.null(frequencies)){
            frequencies = 1:(dim(li[[block_num]])[1])
          }
          if(length(time_point) == 0){
            return(li[[block_num]][frequencies, ])
          }else{
            return(li[[block_num]][frequencies, time_point])
          }
        }
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
