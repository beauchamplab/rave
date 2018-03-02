eval_fun <- function(func, args = list(), env = parent.frame()){
  tryCatch({
    if(is.null(env)){
      result = do.call(func, args = args)
    }else{
      body = pryr::fun_body(func)
      f = lazyeval::as.lazy(body, env = env)
      if(length(args)){
        result = lazyeval::lazy_eval(f, data = args)
      }else{
        result = lazyeval::lazy_eval(f)
      }
    }
    return(result)
  }, error = function(e){
    logger(e)
    validate(need(FALSE, paste(e$message, collapse = '\n')))
    return(NULL)
  }) ->
    result
  return(invisible(result))
}
