parse_call = function(quo, env = NULL){

  expr = rlang::quo_squash(quo)
  env %?<-% rlang::quo_get_env(quo)

  func_expr = expr[[1]]
  func = eval(func_expr, envir = env)
  func_name = str_replace(deparse(func_expr), '^[\\w]+::', '')
  func_env = environment(func)
  func_ns = environmentName(func_env)

  # find function params, format function
  func_args = names(formals(func))

  expr_decomp = as.list(expr)[-1]
  named_nm = func_args[func_args %in% names(expr_decomp)]
  unnamed_nm = func_args[!func_args %in% named_nm & func_args != '...']
  args = expr_decomp[named_nm]
  ct = 1

  if(length(expr_decomp)){
    for(i in 1:length(expr_decomp)){
      carg = expr_decomp[i]
      ncarg = names(carg)
      if(is.null(ncarg) || ncarg == '' || !names(carg) %in% named_nm){
        if(i <= length(unnamed_nm)){
          names(carg) = unnamed_nm[i]
          i = i + 1
        }
        args = c(args, carg)
      }
    }
  }

  func_call = as.call(c(list(func_expr), args))

  re = list(
    expr = func_call,
    env = env,
    .args = args,
    .func = list(
      func_expr = func_expr,
      func_name = func_name,
      func_env = func_env,
      func_ns = func_ns
    ),
    .change_param = function(..., .lazy = TRUE){
      .args = args
      adargs = list(...)
      for(nm in names(adargs)){
        .args[[nm]] = adargs[[nm]]
      }
      expr = as.call(c(list(func_expr), .args))
      if(.lazy){
        quo = rlang::quo({})
        quo = rlang::quo_set_expr(quo, expr)
        quo = rlang::quo_set_env(quo, env)

        return(quo)
      }else{
        return(expr)
      }
    }
  )

  # Compatible with lazyeval (to be removed)
  class(re) <- 'lazy'
  return(re)
}


# parse_shiny_inputs = function(comp, env){
#   comp = parse_call(comp = comp, env = env)
#   comp_info = ui_register_function(sprintf('%s::%s', comp$.func$func_ns, comp$.func$func_name))
#
#   comp$.update_func = comp_info$update_func
#   comp$.value = comp_info$value
#   comp$.default_args = comp_info$default_args
#   comp$.update_value = comp_info$update_value
#
#   arglist = comp$.args
#   comp$.inputId = arglist$inputId
#
#
#   comp
# }
