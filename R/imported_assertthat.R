assert_that <- function(..., env = parent.frame(), msg = NULL){

  f = get_from_package('assert_that', pkg = "assertthat")

  if(is.function(f)){
    return(f(..., env = env, msg = msg))
  }else{
    for (expr in eval(substitute(alist(...)))) {
      res <- tryCatch({
        eval(expr, env)
      }, error = function(e) {
        structure(FALSE, msg = e$message)
      })
      if (!res) {
        if (is.null(msg))
          msg <- attr(res, "msg")

        class <- c("assertError", "simpleError", "error", "condition")
        err = structure(list(message = msg, call = NULL), class = class)
        stop(err)
      }
    }

    return(TRUE)
  }
}


