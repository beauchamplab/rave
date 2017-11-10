exprToFunction <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) {
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }
  makeFunction(body = expr, env = env)
}


makeFunction <- function (args = pairlist(), body, env = parent.frame()) {
  eval(call("function", args, body), env)
}
