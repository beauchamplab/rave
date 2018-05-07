to_package_name <- function(module_id){
  pkgName = paste('RAVE', module_id)
  pkgName = str_replace_all(pkgName, '[\\W_]', '')
  return(pkgName)
}

#' @importFrom devtools install
#' @importFrom roxygen2 roxygenise
#' @export
install_rave_module <- function(module_id, ...){
  modules = read.csv(rave_options('module_lookup_file'), stringsAsFactors = F)
  assertthat::assert_that(module_id %in% modules$ModuleID, msg =
                            'Module ID [' %&% module_id %&% '] not found in rave_options("module_lookup_file")')
  row = which(modules$ModuleID == module_id)[1]
  label_name = modules$Name[row]
  script_path = modules$ScriptPath[row]
  packages = modules$Packages[row]
  packages = stringr::str_trim(stringr::str_split(packages, ';', simplify = T))
  packages = packages[!is.blank(packages)]

  pkg = rave:::module_to_package(
    module_id = module_id, label_name = label_name,
    script_path = script_path, load = F, packages = packages
  )

  return(pkg)
}


module_to_package <- function(
  module_id, label_name,
  script_path, packages = NULL,
  load = F, root_dir = NULL, ...
){
  src = readLines(script_path)
  force(module_id);force(label_name);force(packages);force(load);force(root_dir)

  pkgName = to_package_name(module_id)

  global_env = globalenv()
  rm(list = ls(global_env, all.names = T), envir = global_env)

  .this = new.env(parent = global_env)
  .inner = new.env(parent = .this)
  .this$..packages = packages
  .this$..call_stack = list()

  .func = new.env()


  # functions
  .this$require = .this$library = function(package, ...){
    pkg = as.character(substitute(package))
    if(!pkg %in% installed.packages()[,1] && is.character(package)){
      pkg = package
    }
    assertthat::assert_that(pkg %in% installed.packages()[,1], msg = sprintf('Package not exists. Please Install package: %s', pkg))
    .this$..packages = c(.this$package, pkg)
    do.call(base::`library`, args = list(
      package = pkg, character.only = T
    ))
  }

  .this$rave_prepare = .this$rave_ignore = function(...){}

  .this$source = function(file, ...){
    # try to locate file
    module_dir = dirname(script_path)
    fpath = file.path(module_dir, file)
    if(!file.exists(fpath)){
      fpath = file
    }
    assertthat::assert_that(file.exists(fpath), msg = sprintf('Source file does not exist: %s', file))
    .this$..call_stack = c(.this$..call_stack, rev(rlang::parse_quos(paste(readLines(fpath), collapse = '\n'), env = .this)))
  }

  .this$rave_updates = function(...){
    quos = rlang::quos(...)
    exprs = lapply(quos, rlang::quo_get_expr)
    call = rlang::new_call(quote(rave_updates), as.pairlist(exprs))
    call[['.env']] = quote(environment())
    .inner$..rave_updates = function(.env = ..get_runtime_env(name = 'param_env')){}
    body(.inner$..rave_updates) = rlang::quo_squash(rlang::quo({
      with(.env, {!!call})
    }))
  }

  .this$rave_inputs = function(...){
    quos = rlang::quos(...)
    exprs = lapply(quos, rlang::quo_get_expr)
    call = rlang::new_call(quote(rave_inputs), as.pairlist(exprs))
    call[['.env']] = quote(.env)
    .inner$..rave_inputs = function(.env = ..get_runtime_env(name = 'param_env')){}
    body(.inner$..rave_inputs) = call
  }

  .this$rave_execute = function(...){
    quos = rlang::quos(...)
    exprs = lapply(quos, rlang::quo_get_expr)
    call = rlang::new_call(quote(rave_execute), as.pairlist(exprs))
    call[['.env']] = quote(.env)
    .inner$..rave_execute = function(.env = ..get_runtime_env()){}
    body(.inner$..rave_execute) = call
  }

  .this$rave_outputs = function(...){
    quos = rlang::quos(...)
    exprs = lapply(quos, rlang::quo_get_expr)
    call = rlang::new_call(quote(rave_outputs), as.pairlist(exprs))
    .inner$..rave_outputs = function(){}
    body(.inner$..rave_outputs) = call
  }


  # parse source
  .this$..call_stack = rev(rlang::parse_quos(paste(src, collapse = '\n'), env = .this))


  while (length(.this$..call_stack)){
    n = length(.this$..call_stack)
    quo = .this$..call_stack[[n]]
    .this$..call_stack[[n]] = NULL

    expr = rlang::quo_squash(quo)
    rave::logger(n); print(expr)
    res = rave::eval_dirty(quo, env = .inner)
  }

  # to create a package for this, the best way is to write down all functions to scripts and cache all variables
  # to a RData file and load them via a function
  nms = names(as.list(.inner, all.names = T))

  sapply(nms, function(key){
    val = .inner[[key]]
    if(is.function(val)){
      .func[[key]] = val
      rm(list = key, envir = .inner)
    }
    NULL
  })

  .func$..get_runtime_env = function(session, wrapper = F, name = 'runtime_env'){}
  body(.func$..get_runtime_env) = rlang::quo_squash(rlang::quo({
    if(missing(session) || is.null(session)){
      session = rave:::getDefaultReactiveDomain()
    }
    ..session_envs[['module_instance']] %?<-% rave::ModuleEnvir$new(
      module_id = !!module_id,
      label_name = !!label_name,
      script_path = system.file('rave_init.R', package = !!pkgName),
      externalpackage = T
    )

    exec_env = ..session_envs[['module_instance']]$get_or_new_exec_env(session = session, parent_env = ..runtime_env)
    if(wrapper){
      return(exec_env)
    }else{
      return(exec_env[[name]])
    }
  }))

  .func$..rave_init = function(clear_env = T){}
  body(.func$..rave_init) = rlang::quo_squash(rlang::quo({
    exec_env = ..get_runtime_env(wrapper = T)
    runtime_env = exec_env$runtime_env
    static_env = exec_env$static_env

    if(clear_env){
      .list = names(as.list(runtime_env, all.names = T))
      if(length(.list)){
        rm(list = .list, envir = runtime_env)
      }
    }

    if(!environmentIsLocked(static_env)){
      # Step 2: re-direct functions so that we can call any functions
      # This is only for RAVE. However, may not be necessary
      # Will see if I can remove this ugly step
      pkg = do.call('loadNamespace', list(package = !!pkgName))

      ..rdata_file = system.file('vars.RData', package = !!pkgName)
      base::load(file = ..rdata_file, envir = static_env)

      lapply(names(as.list(pkg, all.names=T)), function(nm){
        fun = pkg[[nm]]
        if(is.function(fun)){
          environment(fun) = runtime_env
        }
        static_env[[nm]] = fun
        invisible()
      })

      # lock static_env, this step is now take place in execenv -> load_script
      # lockEnvironment(static_env)
    }


    list(
      static = static_env,
      runtime = runtime_env
    )
  }))

  # create package
  create_package(module_id = module_id,
                 env = .func,
                 data = .inner,
                 dependencies = .this$..packages,
                 load = load,
                 root_dir = root_dir, ...) ->
    re

  return(re)
}



#' @import stringr
create_package <- function(module_id, env = new.env(), data = new.env(),
                           dependencies = NULL, load = T, attach = FALSE, root_dir = NULL, ...) {

  pkgName = to_package_name(module_id)

  if(is.null(root_dir)){
    root_dir = file.path(rave_options('module_root_dir'), 'tempdir')
  }
  dir.create(root_dir, showWarnings = F, recursive = T)
  pack_dir = file.path(root_dir, pkgName)
  inst_dir = file.path(pack_dir, 'inst')
  src_file = file.path(pack_dir, 'R', 'aaa.R')



  # save script to pack_dir
  if(file.exists(pack_dir)){
    unlink(pack_dir, recursive=TRUE, force = T)
  }

  dependencies = unique(c(dependencies, 'rave'))


  env$..dependencies = function(){}

  env$..pkgName = pkgName

  body(env$..dependencies) = {dependencies}
  package.skeleton(name=pkgName, path = root_dir, environment = env, force = T)
  cat("\n..runtime_env = new.env();",
      "..session_envs = new.env();\n\n\n",
      paste0("#' @import ", dependencies),
      'NULL\n',
      sep = '\n', file = src_file, append = T)
  # remove man folder
  unlink(file.path(pack_dir, 'man'), recursive = T, force = T)
  dir.create(inst_dir, showWarnings = F, recursive = T)

  # cache data
  if(!is.environment(data)){
    if(is.list(data)){
      data = list2env(data)
    }else{
      data = new.env()
    }
  }

  if(is.environment(data) && length(as.list(data, all.names = T))){
    nm = names(as.list(data, all.names = T))
    save(list = nm, envir = data, file = file.path(inst_dir, 'vars.RData'))
  }
  cat(
    '..rave_init(clear_env = T)',
    '..rave_inputs()',
    '..rave_updates()',
    '..rave_outputs()',
    '..rave_execute()',
    # TODO: add ..rave_check()
    sep = '\n', file = file.path(inst_dir, 'rave_init.R')
  )


  disc = file.path(pack_dir, 'DESCRIPTION')
  nsp = file.path(pack_dir, 'NAMESPACE')
  tmp = c(
    '# Generated by roxygen2: do not edit by hand',
    readLines(disc),
    'Imports:',
    paste(
      sapply(dependencies, function(p){
        if(p %in% (installed.packages()[,1])){
          ver = utils::packageVersion(p)
          sprintf('\t%s (>= %s)', p, as.character(ver))
        }else{
          p
        }
      }),
      collapse = ',\n'
    )
  )
  writeLines(tmp, disc)
  tmp = c('# Generated by roxygen2: do not edit by hand', readLines(nsp))
  writeLines(tmp, nsp)


  roxygen2::roxygenise(pack_dir, clean = T, roclets = c("collate", "namespace"))
  # devtools::build(pack_dir)
  pkg = devtools::install(pack_dir, reload = T, quick = T, local = T, dependencies = T, upgrade_dependencies = T,
                          keep_source = T, quiet = T)


  if(base::isNamespaceLoaded(pkgName)){
    tryCatch({
      detach(paste0('package:', pkgName), unload = T, character.only = T)
    }, error = function(e){})
    base::unloadNamespace(pkgName)
  }
  if(load){
    e = do.call('loadNamespace', args = list(package = pkgName))
    if(attach){
      do.call('library', args = list(
        package = pkgName,
        character.only = T
      ))
    }
  }else{
    e = pkgName
  }


  invisible(e)
}



# e = module_to_package(load = T)
# b = e$..rave_init_vars()
# ls(b)
