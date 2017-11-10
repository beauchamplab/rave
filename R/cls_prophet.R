#' @include options.R
NULL

# Dipterix knows everything

Dipterix <- R6::R6Class(
  'Prophet',

  private = list(

    # Cached list
    session_pool = list(),
    subjects = list(),
    ids = c()

  ),

  public = list(

    observer = NULL,
    last_subject = NULL,

    reset = function(){
      # TODO sometimes reset doesn't work, need to find which objects preventing gc
      self$observer = NULL
      self$last_subject = NULL
      private$subjects = list()
      private$ids = c()
      private$session_pool = list()
    },

    find_all_subjects = function(){
      s <- list.files(rave_opts$get_options('data_dir'))
      s
    },

    init_subject = function(subject_id){
      if(length(subject_id) == 1 &&
         !is.na(subject_id) &&
         subject_id != '' &&
         !subject_id %in% private$ids
      ){
        new_subject <- Subject$new(subject_id)
        private$ids <- c(private$ids, subject_id)
        private$subjects[[subject_id]] <- new_subject
      }
    },

    get_subject = function(subject_id, temp = FALSE){
      if(is.null(subject_id) || subject_id == ''){
        return(NULL)
      }
      if(!subject_id %in% private$ids){
        self$init_subject(subject_id)
      }
      if(!temp){
        self$last_subject <- subject_id
      }

      return(private$subjects[[subject_id]])
    },

    assign_observer = function(observer){
      # Must be Observatory
      self$observer <- observer
    },

    add_session = function(session = shiny::getDefaultReactiveDomain()){
      session_id = get_session_id(session)
      private$session_pool[[session_id]] = session
    },

    get_session = function(session_id){
      private$session_pool[[session_id]]
    },

    reload_sessions = function(except_session = shiny::getDefaultReactiveDomain()){
      session_ids = names(private$session_pool)
      session_ids = session_ids[!session_ids %in% get_session_id(except_session)]

      for(id in session_ids){
        session = private$session_pool[[id]]
        if(!is.null(session) && !session$closed){
          session$userData$refresh()
        }else{
          private$session_pool[[id]] = NULL
        }
      }
    },

    clear_session = function(session = shiny::getDefaultReactiveDomain()){
      session_id = get_session_id(session)

      private$session_pool[[session_id]] = NULL
      data_repository$remove_session(session_id)
    }
  )
)


#' @export
prophet <- Dipterix$new()


#' @export
find_subject_ids <- function(){
  return(prophet$find_all_subjects())
}


#' @export
get_subject <- function(subject_id, temp = TRUE){
  prophet$get_subject(subject_id = subject_id, temp = temp)
}
