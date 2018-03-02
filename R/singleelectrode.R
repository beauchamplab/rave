
#' @import rhdf5
#' @export
#' @exportClass SingleElectrode
SingleElectrode <- R6::R6Class(
  'SingleElectrode',

  cloneable = FALSE,
  portable = FALSE,

  private = list(
  ),

  public = list(
    subject = NULL,
    electrode = NULL,
    power = NULL,
    phase = NULL,
    cut = NULL,
    cache_path = NULL,


    initialize = function(subject, electrode){
      # locate source file
      data_dir = rave_opts$get_options('data_dir')
      subject_dir = file.path(data_dir, subject, 'rave')
      cache_path = file.path(subject_dir, 'cache', sprintf('chl_%d.h5', electrode))

      # assume the file is ready
      self$cache_path = tools::file_path_as_absolute(cache_path)

      # load power
      self$power = rhdf5::h5read(file = cache_path, name = 'power')

      # TODO: load phase

      # load meta
      # TODO load time cut
      # Here I use ad-hoc template


    }
  )
)
#'
#' #' @export
#' #' @exportClass MultiElectrodes
#' MultiElectrodes <- R6::R6Class(
#'   'MultiElectrodes',
#'
#'   cloneable = FALSE,
#'   portable = FALSE,
#'
#'   private = list(
#'     valid_electrodes = c(),
#'     storage = list(),             # Stores single electrodes
#'     index = c(),                  # helps indexing electrodes
#'     n = 0,
#'     dims = NULL
#'   ),
#'
#'   public = list(
#'     # I have to put them here
#'     # Why not put them in private and use active binding?
#'     # Because of some memory/CPU issues
#'
#'     subject = NULL,
#'     electrodes = NULL,
#'     data = NULL,
#'     cumsum = NULL,
#'
#'     initialize = function(subject){
#'       self$subject <- subject
#'
#'       elec <- subject$electrodes
#'       private$valid_electrodes <- elec$Number[elec$Valid != 0]
#'     },
#'     deactivate = function(){
#'       # Don't know why if I set data/cumsum to 0, it will crush
#'       # self$bind_electrodes(as.integer(private$index[1]))
#'       self$data <- matrix()
#'       self$cumsum <- matrix()
#'
#'       gc()
#'     },
#'     activate = function(){
#'       # TODO
#'     },
#'     bind_electrodes = function(electrodes = NULL, debug = FALSE, to_ram = FALSE){
#'       if(is.null(electrodes)){
#'         electrodes = self$electrodes
#'       }
#'
#'       if(rave_opts$get_options('use_rhdf5') == 'TRUE' && requireNamespace('rhdf5')){
#'         h5file = file.path(self$subject$cache_dir, 'all.h5')
#'
#'         if(to_ram){
#'           self$data = as.array(HDF5Array::HDF5Array(
#'             file = h5file, name = 'data')[,,,electrodes]
#'           )
#'           self$cumsum = as.array(
#'             HDF5Array::HDF5Array(file = h5file, name = 'cumsum')[,,,electrodes])
#'         }else{
#'           self$data = HDF5Array::HDF5Array(file = h5file, name = 'data')[,,,electrodes]
#'           self$cumsum = HDF5Array::HDF5Array(file = h5file, name = 'cumsum')[,,,electrodes]
#'         }
#'
#'         return()
#'       }
#'
#'
#'       electrodes = electrodes[electrodes %in% private$valid_electrodes]
#'       self$electrodes = electrodes
#'
#'       ticker <- 0.5 / (length(electrodes) + 1)
#'       if(debug){
#'         withProgress = function(expr, value = NULL, message = 'Importing Electrodes', detail = ''){
#'           eval(expr, envir = self)
#'         }
#'         incProgress = function(...){}
#'       }else{
#'         withProgress = shiny::withProgress
#'         incProgress = shiny::incProgress
#'       }
#'
#'       withProgress(value = ticker, message = 'Importing Electrodes', detail = '', {
#'
#'
#'         sample <- private$storage[[1]]$data
#'         self$data <- vapply(
#'           electrodes,
#'           FUN = function(e){
#'             incProgress(ticker, detail = sprintf('Electrode Data - %s', self$subject$electrode_label_by_index(e)))
#'             s_e = private$storage[[toString(e)]]
#'             s_e$data[,,,1]
#'           },
#'           FUN.VALUE = sample[,,,1]
#'         )
#'         dname = dimnames(self$data)
#'         dname[[4]] <- subject$electrode_label_by_index(electrodes)
#'         dimnames(self$data) <- dname
#'
#'         # Do I need Cache? We can calculate cumsum because there will be enough mems
#'         # Trial Freq Time Elec
#'         # dims <- dim(self$data)
#'         # cumsums <- mclapply(1:dim[4], function(e){
#'         #   aperm(vapply(1:dims[1], function(trial){
#'         #     t(apply(self$data[trial,,,e], 1, cumsum))
#'         #   }, self$data[1,,,1]), c(3,1,2))
#'         # })
#'         # self$cumsum <- vapply(1:dims[4],
#'         #                       function(e){cumsums[[e]]},
#'         #                       FUN.VALUE = self$data[,,,1])
#'
#'         sample <- private$storage[[1]]$cumsum
#'         self$cumsum <- vapply(
#'           electrodes,
#'           FUN = function(e){
#'             incProgress(ticker, detail = sprintf('Electrode Cumsum - %s', self$subject$electrode_label_by_index(e)))
#'             s_e = private$storage[[toString(e)]]
#'             s_e$cumsum[,,,1]
#'           },
#'           FUN.VALUE = sample[,,,1]
#'         )
#'         dname = dimnames(self$data)
#'         dname[[4]] <- subject$electrode_label_by_index(electrodes)
#'         dimnames(self$cumsum) <- dname
#'
#'       })
#'
#'       return()
#'     },
#'     load = function(electrodes, async = FALSE){  # async depricated
#'
#'       if(rave_opts$get_options('use_rhdf5') == 'TRUE' && requireNamespace('rhdf5')){
#'         self$electrodes = self$subject$filter_valid_electrodes(electrodes)
#'         return()
#'       }
#'       # load electrodes & cache
#'       # get new electrodes
#'       new_e <- electrodes[(!electrodes %in% private$index) & (electrodes %in% private$valid_electrodes)]
#'
#'       for(e in new_e){
#'         single_electrode <- SingleElectrode$new(subject = subject, electrode = e)
#'
#'         if(single_electrode$valid){
#'           # add
#'           private$n <- private$n + 1
#'           private$index[private$n] <- e
#'           private$storage[[toString(e)]] <- single_electrode
#'         }
#'
#'       }
#'
#'
#'       return(NULL)
#'
#'
#'     }
#'   )
#'
#' )
