#' @include utils_syscheck.R
NULL

#' @import htmltools
#' @import shiny
#' 
#' @importFrom grid grid.newpage
#'
#' @importFrom crayon make_style
#'
#' @importFrom DT formatRound
#' @importFrom DT datatable
#'
#' @importFrom rlang quo
#' @importFrom rlang quos
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom rlang quo_squash
#' @importFrom rlang eval_tidy
#' @importFrom rlang :=
#' @importFrom rlang is_quosure
#' @importFrom rlang as_quosure
#' @importFrom rlang fn_body
#' @importFrom digest digest
#'
#' @importFrom future plan
#' @importFrom future future
#' @importFrom future value
#' @importFrom future values
#' @importFrom future multisession
#' @importFrom future multiprocess
#' @importFrom future futureAssign
#' @importFrom future availableCores
#' @importFrom future cluster
#' @importFrom future resolved
#'
#' @importFrom hdf5r is_hdf5
#' @importFrom hdf5r H5File
#'
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard tabBox
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard menuSubItem
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard tabItems
#'
#' @importFrom methods is
#' @importFrom methods setMethod
#' @importFrom methods signature
#'
#' @importFrom servr random_port
#' 
#' @importFrom dipsaus collapse
#' @importFrom dipsaus cat2
#' @importFrom dipsaus time_delta
#' @importFrom dipsaus get_ram 
#' @importFrom dipsaus sync_shiny_inputs
#' @importFrom dipsaus to_ram_size
#' @importFrom dipsaus mem_limit2
#' @importFrom dipsaus col2hexStr
#' @importFrom dipsaus eval_dirty
#' @importFrom dipsaus %?<-%
#' @importFrom dipsaus progress2
#' @importFrom dipsaus actionButtonStyled
#' @importFrom dipsaus compoundInput2
#' 
NULL



tags = htmltools::tags
div = htmltools::div

### For dev use only:
gl <- function(..., .envir = parent.frame()){
  glue::glue(..., .envir = .envir)
}

catgl <- function(..., .envir = parent.frame(), level = 'DEBUG'){
  dipsaus::cat2(gl(..., .envir = .envir), level = level)
}

soft_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  catgl('Function {call[[1]]} is soft-Deprecated. Details: \n{deparse(call)}', level = 'WARNING')
}

### Stores internal settings (session-based)
.conf_env <- new.env(parent = emptyenv())

get_conf <- function(key, default = NULL){
  soft_deprecated()
  if(exists(key, envir = .conf_env)){
    return(.conf_env[[key]])
  }else{
    default
  }
}

set_conf <- function(key, val, remove_if_null = TRUE){
  soft_deprecated()
  if(remove_if_null && (missing(val) || is.null(val))){
    rm(list = key, envir = .conf_env, inherits = FALSE)
  }else{
    .conf_env[[key]] = val
  }
}
