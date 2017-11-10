#' @include options.R
NULL

.find_file = function(fne, postfix, directory){
  all_files = list.files(directory)
  reg <- stringr::str_to_upper(stringr::str_c('^', fne, '\\.', postfix, '$'))
  fname <- cbind(data.frame(all_files, stringsAsFactors = F),
                 stringr::str_match(stringr::str_to_upper(all_files), reg))
  fname <- fname[!is.na(fname[, 2]), ]
  if(length(fname) > 0 ){
    return(fname)
  }else{
    stop(sprintf('Cannot find any file matching %s.%s', fne, postfix))
  }
}

#' @export Subject
#' @exportClass Subject
Subject <- R6::R6Class(
  'Subject',

  portable = FALSE,

  private = list(
    .id = NULL,
    .electrodes = NULL,
    .frequencies = NULL,
    .time_points = NULL,
    .trials = NULL,
    .cache_dir = ''
  ),

  active = list(
    id = function(){
      private$.id
    },
    electrodes = function(){
      private$.electrodes
    },
    frequencies = function(){
      private$.frequencies
    },
    time_points = function(){
      private$.time_points
    },
    trials = function(){
      private$.trials
    },
    cache_dir = function(){
      private$.cache_dir
    },
    valid_electrodes = function(){
      elec = private$.electrodes
      (elec$Number)[elec$Valid != 0]
    }
  ),

  public = list(
    data_environment = NULL,
    meta = NULL,

    electrode_label_by_index = function(ind){

      sapply(ind, function(i){
        private$.electrodes$Label[private$.electrodes$Number == i]
      }) %>% unlist
    },

    electrode_index_by_label = function(labels){
      sapply(labels, function(i){
        private$.electrodes$Number[private$.electrodes$Label == i]
      }) %>% unlist
    },

    filter_valid_electrodes = function(electrodes){
      electrodes[electrodes %in% self$valid_electrodes]
    },

    read.csv = function(table_name, postfix = 'csv', func = utils::read.csv, stringsAsFactors = F, ...){
      dir <- file.path(rave_opts$get_options('data_dir'), private$.id, 'ecog', 'meta')
      dat <- func(file.path(dir, sprintf('%s.%s', table_name, postfix)), stringsAsFactors = stringsAsFactors, ...)
      assign(table_name, dat, envir = self$meta)
    },

    initialize = function(subject_id){
      # get options
      opt <- rave_opts$get_options()

      self$meta = new.env()

      # Assign to members
      private$.id <- subject_id

      dir <- file.path(opt$data_dir, subject_id, 'ecog')

      # Load electrodes
      self$read.csv('electrodes')
      private$.electrodes <- self$meta$electrodes[order(self$meta$electrodes$Number), ]


      # frequency info
      self$read.csv('frequencies')
      private$.frequencies <- self$meta$frequencies

      # Time labels
      self$read.csv('time_points')
      private$.time_points <- self$meta$time_points

      # Exp trial information
      self$read.csv('trials')
      private$.trials <- self$meta$trials


      # set cache dir
      private$.cache_dir <- file.path(dir, 'cached')
      if(!file.exists(private$.cache_dir)){
        dir.create(private$.cache_dir, recursive = T)
      }

      # if use rhdf5, create all.h5 in cached
      if(rave_opts$get_options('use_rhdf5') == 'TRUE' && requireNamespace('rhdf5')){
        h5file = file.path(
          dir, 'cached', 'all.h5'
        )
        logger('Checking ', h5file)
        if(!file.exists(h5file)){
          logger('Generating', h5file)
          assertthat::assert_that(rhdf5::h5createFile(h5file), msg = 'Cannot create all.h5 in HDF5 settings. To disable HDF5, use: rave_opts$set_options(use_rhdf5 = "FALSE")')
        }

        # check if data exists
        if(!'data' %in% rhdf5::h5ls(h5file)$name){
          logger('Generating Cache')
          data_dim = c(
            nrow(self$meta$trials),
            nrow(self$meta$frequencies),
            nrow(self$meta$time_points),
            nrow(self$meta$electrodes)
          )
          dimnames = list(
            paste0(self$meta$trials$Stimulus, '_', self$meta$trials$Type),
            self$meta$frequencies$Frequency,
            self$meta$time_points$Time,
            self$meta$electrodes$Label
          )
          rhdf5::h5createDataset(file = h5file,
                                 dataset = 'data',
                                 dims = data_dim,
                                 storage.mode = 'double',
                                 chunk = c(1, data_dim[2:3], 1),
                                 level = 7)
          rhdf5::h5createDataset(file = h5file,
                                 dataset = 'cumsum',
                                 dims = data_dim,
                                 storage.mode = 'double',
                                 chunk = c(1, data_dim[2:3], 1),
                                 level = 7)

          # HDF5Array::HDF5RealizationSink(data_dim,
          #                                dimnames = dimnames,
          #                                file = h5file,
          #                                name = 'data',
          #                                chunk_dim = c(1, data_dim[2:3], 1),
          #                                level = 7)
          # HDF5Array::HDF5RealizationSink(data_dim,
          #                                dimnames = dimnames,
          #                                file = h5file,
          #                                name = 'cumsum',
          #                                chunk_dim = c(1, data_dim[2:3], 1),
          #                                level = 7)

          rhdf5::h5createDataset(file = h5file,
                                 dataset = 'cached_index',
                                 dims = data_dim[4],
                                 storage.mode = 'integer')

        }
      }

      # Assign electrodes
      self$data_environment <- MultiElectrodes$new(self)
    }
  )
)


#' Merge subjects, generate
merge_subject <- function(X, new_id = 'TMP_MERGED'){
  stop(shiny::safeError('merge_subject NOT implemented yet!'))
  dir <- file.path(rave_opts$get_options('data_dir'), new_id, 'ecog')
  if(file.exists(dir)){
    shiny::safeError(sprintf('Directory already exists - %s', dir))
  }else{
    dir.create(path = dir, recursive = T)
    dir.create(path = file.path(dir, 'meta'))
    dir.create(path = file.path(dir, 'cached'))

    # Generate meta files

  }
}


#' @export
#' @exportClass SingleElectrode
SingleElectrode <- R6::R6Class(
  'SingleElectrode',

  cloneable = FALSE,
  portable = FALSE,

  private = list(
    load_from_cache_ff = function(){
      cache_dir <- file.path(self$cache_path, self$electrode)
      caches <- stringr::str_c(cache_dir, '/', c(
        'data.RData', 'cumsum.RData', 'data.ff', 'cumsum.ff'
      ))

      for(f in caches){
        if(!file.exists(f)){
          return(FALSE)
        }
      }
      logger('Try to load from cache...')

      self$data <- load_ff('data', directory = cache_dir)
      self$cumsum <- load_ff('cumsum', directory = cache_dir)
      return(TRUE)
    },

    load_raw = function(){
      # get options
      opt <- rave_opts$get_options()

      # load electrode data
      # ecogdata.content_regex := ^e([0-9]+)[^0-9]*$
      #   ecogdata.content_format := mat
      dir <- file.path(opt$data_dir, self$subject$id, 'ecog')
      postfix = opt$content_format
      file <- .find_file(opt$content_regex, postfix, dir)
      file <- file[file[,3] == electrode, 1]
      func <- get(sprintf('import_from_%s', stringr::str_to_lower(postfix)))
      func(
        file.path(dir, file),
        'content'
      ) ->
        content

      # content = trial x freq x time
      c_dim <- dim(content)
      if(length(c_dim) == 4){
        c_dim = c_dim[1:3]
      }
      content <- array(content, dim = c(c_dim, 1), dimnames = list(
        paste0(self$subject$trials$Stimulus, '_', self$subject$trials$Type),
        self$subject$frequencies$Frequency,
        self$subject$time_points$Time,
        self$subject$electrode_label_by_index(electrode)
      ))


      ################### Generate summary
      # cumsum
      calc_cumsum <- function(i){
        apply(content[i, , , 1], 1, cumsum) %>%
          t
      }
      1:dim(content)[1] %>%
        vapply(calc_cumsum, content[1, , , 1]) %>%
        aperm(c(3,1,2)) ->
        cumsum
      dim(cumsum) <- dim(content)
      dimnames(cumsum) <- dimnames(content)

      self$data = content
      self$cumsum = cumsum
    }
  ),

  public = list(
    subject = NULL,
    electrode = NULL,
    data = NULL,
    cumsum = NULL,
    cache_path = NULL,
    valid = FALSE,
    activate = NULL,
    deactivate = NULL,

    initialize = function(subject, electrode){

      self$cache_path <- subject$cache_dir
      self$electrode <- electrode
      self$subject <- subject

      # check if the electrode is valid or not
      electrodes <- subject$electrodes
      self$valid = (as.numeric(electrodes[electrodes$Number == electrode, 'Valid']) != 0)

      if(self$valid){
        # try to load electrode from cache
        use_rhdf5 = rave_opts$get_options('use_rhdf5')

        if(use_rhdf5 == 'TRUE' && requireNamespace('rhdf5')){
          h5file = file.path(subject$cache_dir, 'all.h5')
          assertthat::assert_that(file.exists(h5file))

          # check cached_index
          cached_index = rhdf5::h5read(h5file, 'cached_index')
          if(electrode %in% cached_index){
            self$data = HDF5Array::HDF5Array(file = h5file, name = 'data')[,,,electrode]
            self$cumsum = HDF5Array::HDF5Array(file = h5file, name = 'cumsum')[,,,electrode]
          }else{
            private$load_raw()
            logger('Creating Cache - Electrode ', electrode, ' at all.h5')
            h5f = rhdf5::H5Fopen(h5file)
            rhdf5::h5writeDataset.array(self$data, h5f, name = 'data',
                           index = list(NULL, NULL, NULL, electrode))
            rhdf5::h5writeDataset.array(self$cumsum, h5f, name = 'cumsum',
                           index = list(NULL, NULL, NULL, electrode))


            cached_index[electrode] = electrode
            h5f$'cached_index' = cached_index
            rhdf5::H5Fflush(h5f)
            rhdf5::H5Fclose(h5f)
          }

          self$activate = function(){}
          self$deactivate = function(){}
        }else{
          re = private$load_from_cache_ff()
          if(!private$load_from_cache_ff()){
            private$load_raw()
            cache_dir = file.path(cache_path, electrode)

            logger('Creating Cache - Electrode ', electrode, ' at ', cache_dir)
            # assign
            self$data <- as_ff(self$data, name = 'data',
                               directory = cache_dir, finonexit = FALSE)
            self$cumsum <- as_ff(self$cumsum, name = 'cumsum',
                                 directory = cache_dir, finonexit = FALSE)
          }
          self$activate = function(){
            ff::open.ff(self$data)
            ff::open.ff(self$cumsum)
          }
          self$deactivate = function(){
            ff::close.ff(self$data)
            ff::close.ff(self$cumsum)
          }
        }

      }else{
        self$activate = function(){}
        self$deactivate = function(){}
      }
    }
  )
)

#' @export
#' @exportClass MultiElectrodes
MultiElectrodes <- R6::R6Class(
  'MultiElectrodes',

  cloneable = FALSE,
  portable = FALSE,

  private = list(
    valid_electrodes = c(),
    storage = list(),             # Stores single electrodes
    index = c(),                  # helps indexing electrodes
    n = 0,
    dims = NULL
  ),

  public = list(
    # I have to put them here
    # Why not put them in private and use active binding?
    # Because of some memory/CPU issues

    subject = NULL,
    electrodes = NULL,
    data = NULL,
    cumsum = NULL,

    initialize = function(subject){
      self$subject <- subject

      elec <- subject$electrodes
      private$valid_electrodes <- elec$Number[elec$Valid != 0]
    },
    deactivate = function(){
      # Don't know why if I set data/cumsum to 0, it will crush
      # self$bind_electrodes(as.integer(private$index[1]))
      self$data <- matrix()
      self$cumsum <- matrix()

      gc()
    },
    activate = function(){
      # TODO
    },
    bind_electrodes = function(electrodes = NULL, debug = FALSE, to_ram = FALSE){
      if(is.null(electrodes)){
        electrodes = self$electrodes
      }

      if(rave_opts$get_options('use_rhdf5') == 'TRUE' && requireNamespace('rhdf5')){
        h5file = file.path(self$subject$cache_dir, 'all.h5')

        if(to_ram){
          self$data = as.array(HDF5Array::HDF5Array(
            file = h5file, name = 'data')[,,,electrodes]
          )
          self$cumsum = as.array(
            HDF5Array::HDF5Array(file = h5file, name = 'cumsum')[,,,electrodes])
        }else{
          self$data = HDF5Array::HDF5Array(file = h5file, name = 'data')[,,,electrodes]
          self$cumsum = HDF5Array::HDF5Array(file = h5file, name = 'cumsum')[,,,electrodes]
        }

        return()
      }


      electrodes = electrodes[electrodes %in% private$valid_electrodes]
      self$electrodes = electrodes

      ticker <- 0.5 / (length(electrodes) + 1)
      if(debug){
        withProgress = function(expr, value = NULL, message = 'Importing Electrodes', detail = ''){
          eval(expr, envir = self)
        }
        incProgress = function(...){}
      }else{
        withProgress = shiny::withProgress
        incProgress = shiny::incProgress
      }

      withProgress(value = ticker, message = 'Importing Electrodes', detail = '', {


        sample <- private$storage[[1]]$data
        self$data <- vapply(
          electrodes,
          FUN = function(e){
            incProgress(ticker, detail = sprintf('Electrode Data - %s', self$subject$electrode_label_by_index(e)))
            s_e = private$storage[[toString(e)]]
            s_e$data[,,,1]
          },
          FUN.VALUE = sample[,,,1]
        )
        dname = dimnames(self$data)
        dname[[4]] <- subject$electrode_label_by_index(electrodes)
        dimnames(self$data) <- dname

        # Do I need Cache? We can calculate cumsum because there will be enough mems
        # Trial Freq Time Elec
        # dims <- dim(self$data)
        # cumsums <- mclapply(1:dim[4], function(e){
        #   aperm(vapply(1:dims[1], function(trial){
        #     t(apply(self$data[trial,,,e], 1, cumsum))
        #   }, self$data[1,,,1]), c(3,1,2))
        # })
        # self$cumsum <- vapply(1:dims[4],
        #                       function(e){cumsums[[e]]},
        #                       FUN.VALUE = self$data[,,,1])

        sample <- private$storage[[1]]$cumsum
        self$cumsum <- vapply(
          electrodes,
          FUN = function(e){
            incProgress(ticker, detail = sprintf('Electrode Cumsum - %s', self$subject$electrode_label_by_index(e)))
            s_e = private$storage[[toString(e)]]
            s_e$cumsum[,,,1]
          },
          FUN.VALUE = sample[,,,1]
        )
        dname = dimnames(self$data)
        dname[[4]] <- subject$electrode_label_by_index(electrodes)
        dimnames(self$cumsum) <- dname

      })

      return()
    },
    load = function(electrodes, async = FALSE){  # async depricated

      if(rave_opts$get_options('use_rhdf5') == 'TRUE' && requireNamespace('rhdf5')){
        self$electrodes = self$subject$filter_valid_electrodes(electrodes)
        return()
      }
      # load electrodes & cache
      # get new electrodes
      new_e <- electrodes[(!electrodes %in% private$index) & (electrodes %in% private$valid_electrodes)]

      for(e in new_e){
        single_electrode <- SingleElectrode$new(subject = subject, electrode = e)

        if(single_electrode$valid){
          # add
          private$n <- private$n + 1
          private$index[private$n] <- e
          private$storage[[toString(e)]] <- single_electrode
        }

      }


      return(NULL)


    }
  )

)
