#' #' Code to launch Rserve
#' #' @import Rserve
#' rave_server <- function(
#'   port, config.file = system.file('conf/rserve.conf', package = 'rave'),
#'   remote_access = F, enable_control = F
#' ){
#'   port = as.integer(port)
#'
#'   ra = ifelse(remote_access, 'yes', 'no')
#'   ec = ifelse(enable_control, 'yes', 'no')
#'
#'   Rserve::Rserve(port = port, wait = F, args = c(
#'     '--version', '--RS-encoding', 'UTF-8', '--RS-conf', config.file, '--RS-enable-remote', ra,
#'     '--RS-enable-control', ec
#'   )) ->
#'     version
#'
#'
#'   assign(paste0('server', port, as.numeric(Sys.time()), collapse = '_'),
#'          list(
#'            port = port,
#'            created = Sys.time(),
#'            remote = remote_access,
#'            control = enable_control,
#'            active = TRUE,
#'            version = version
#'          ),
#'          envir = .rserve)
#' }



# connect_server <- function(
#   port, host = '127.0.0.1', ...
# ){
#
# }
#
#
# conn <- RSclient::RS.connect(host = '127.0.0.1', port = 18003L, proxy.wait = T)



