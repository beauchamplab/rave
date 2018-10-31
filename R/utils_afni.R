# load afniio.R


#' Read from AFNIio.R, return an environment wrapper for all functions
#' @export
afni_tools <- function(){
  wrapper_env = new.env(parent = baseenv())
  parse_tools = function(){
    `.__env` = new.env(parent = wrapper_env)
    `.__env`$`<<-` = function(lhs, rhs){
      nm = as.character(substitute(lhs))
      wrapper_env[[nm]] = rhs
    }
    `.__expr` = parse(file = system.file('third_party/AFNIio.R', package = 'rave'))
    eval(`.__expr`, envir = `.__env`)
    `.__env`
  }
  re = parse_tools()
  return(re)
}
