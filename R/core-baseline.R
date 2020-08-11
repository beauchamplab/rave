#' Baseline signals
#' @param el \code{\link[raveio]{Tensor}} or \code{\link[raveio]{ECoGTensor}} object
#' @param from baseline start time
#' @param to baseline end time
#' @param method mean or median, default is mean
#' @param unit "\%" percent signal change or "dB" decibel unit
#' @param data_only return array or tensor object?
#' @param hybrid if return tensor object, swap cache? useful for large dataset
#' @param swap_file by default \code{tempfile()}, or you can specify path
#' @param mem_optimize optimize for large dataset? default is TRUE
#' @param same_dimension logical, true if \code{op} is element-wise operator
#' @param preop function before baseline
#' @param op function for baseline
#' @param data_env internally used
#' @export
baseline <- function(el, from, to, method = 'mean', unit = '%',
                     data_only = F, hybrid = TRUE, swap_file = tempfile(),
                     mem_optimize = TRUE, same_dimension = unit %in% c('%', 'dB'), preop = NULL, op, data_env = getDefaultDataRepository()){
  soft_deprecated()
  if(missing(el)){
    catgl('baseline(el...) is changed now. Please update.', level = 'WARNING')
    
    module_tools = get('module_tools', envir = data_env)
    el = module_tools$get_power()
  }
  stopifnot2(any(c('Tensor') %in% class(el)), msg = 'el must be an Tensor object.')
  stopifnot2('Time' %in% el$varnames, msg = 'Need one dimname to be "Time".')
  
  stopifnot2(unit %in% c('dB', '%', 'C'), msg = 'unit must be %-percent signal change or dB-dB difference, or C to customize')
  
  time_ind = which(el$varnames == 'Time')
  rest_dim = seq_along(el$dim)[-time_ind]
  
  if(unit == 'dB'){
    bs = el$subset({Time = Time %within% c(from, to)})
    
    # log 10 of data, collapse by mean
    bs$set_data(log10(bs$get_data()))
    
    # Use better format of collapse function to avoid crashing
    bs = bs$collapse(keep = rest_dim, method = method)
    
    # for each time points, log10 and substract, Since dB is 10*log10(.)
    op = function(e1, e2){ 10 * (log10(e1) - e2) }
    
    bs = el$operate(by = bs, match_dim = rest_dim, mem_optimize = mem_optimize,
                    fun = op, same_dimension = same_dimension)
  }else if(unit == '%'){
    bs = el$subset(Time = Time %within% c(from, to))
    op = function(e1, e2){ e1 / e2 * 100 - 100 }
    bs = bs$collapse(keep = rest_dim, method = method)
    bs = el$operate(by = bs, match_dim = rest_dim, mem_optimize = mem_optimize,
                    fun = op, same_dimension = same_dimension)
  }else{
    # customizing
    bs = el$subset(Time = Time %within% c(from, to))
    if(is.function(preop)){
      bs$set_data(preop(bs$get_data()))
    }
    bs = bs$collapse(keep = rest_dim, method = method)
    bs = el$operate(by = bs, match_dim = rest_dim, mem_optimize = mem_optimize,
                    fun = op, same_dimension = same_dimension)
  }
  
  if(data_only){
    if('Tensor' %in% class(bs)){
      bs = bs$get_data()
    }
    return(bs)
  }else{
    if('Tensor' %in% class(bs)){
      return(bs)
    }
    ECoGTensor$new(data = bs, dim = dim(el), dimnames = dimnames(el), varnames = el$varnames, hybrid = hybrid, swap_file = swap_file)
  }
}
