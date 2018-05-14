#' #' @import magrittr
#' #' @import stringr
#' #' @export
#' load_modules <- function(lookup_file = NULL){
#'   if(is.null(lookup_file) || !file.exists(lookup_file)){
#'     lookup_file = rave_options('module_lookup_file')
#'   }
#'   assertthat::assert_that(file.exists(lookup_file), msg = sprintf('Module look-up file not found! [%s]', lookup_file))
#'   module_info = read.csv(lookup_file, stringsAsFactors = F)
#'
#'   lapply(1:nrow(module_info), function(i){
#'     a = as.list(module_info[i,])
#'     a$module_id = str_to_lower(a$module_id)
#'     if(!is.na(a$Author)){
#'       a$Author = as.vector(str_split(a$Author, ', ', simplify = T))
#'     }
#'     if(a$Active){
#'       module = ModuleEnvir$new(
#'         module_id = a$ModuleID,
#'         label_name = a$Name,
#'         script_path = a$ScriptPath,
#'         author = a$Author,
#'         version = a$Version
#'       )
#'     }else{
#'       module = NULL
#'     }
#'     return(module)
#'   }) ->
#'     modules
#'   return(unlist(modules))
#' }
