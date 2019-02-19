to_package_name <- function(module_id){
  pkgName = paste('RAVE', module_id)
  pkgName = str_replace_all(pkgName, '[\\W_]', '')
  return(pkgName)
}

#' @export
load_modules <- function(){
  modules = read.csv(rave_options('module_lookup_file'), stringsAsFactors = F)

  #1. filter out all deactived packages
  modules = modules[modules$Active, ]

  assertthat::assert_that(nrow(modules) > 0, msg = 'Is there any module specified in ' %&% rave_options('module_lookup_file') %&% ' ?')

  #2. check if all compiled packages exists
  pkg_ids = unique(modules$PackageID)
  pkg_ids = pkg_ids[!is.na(pkg_ids) & !is.blank(pkg_ids)]

  gt_md = function(x){
    m = as.list(x)
    if(is_invalid(m$PackageID, .invalids = c('null', 'na', 'blank'))){
      module = ModuleEnvir$new(
        module_id = m$ModuleID,
        label_name = m$Name,
        script_path = m$ScriptPath,
        version = m$Version,
        author = m$Author,
        packages = m$Packages
      )
    }else{
      pkg = to_package_name(m$PackageID)
      module = do.call('::', list(pkg, 'rave_module'))(module_id = m$ModuleID, launch = F)
    }
    return(module)
  }

  #3 Build module list
  groups = modules$GroupName
  sel = !is.na(groups) & !is.blank(groups)
  if(sum(sel)){
    sapply(unique(groups[sel]), function(gid){
      g = sel & groups == gid
      apply(modules[g, ], 1, gt_md)
    }, simplify = F, USE.NAMES = T) ->
      module_inst
  }else{
    module_inst = list()
  }

  if(sum(!sel)){
    sapply(modules$ModuleID[!sel], function(mid){
      g = (!sel) & modules$ModuleID == mid
      apply(modules[g, ], 1, gt_md)
    }, simplify = F, USE.NAMES = F) ->
      module_alone
  }else{
    module_alone = list()
  }

  module_inst[['______']] = module_alone

  module_inst
}


#
# install_rave_modules <- function(package_id){
#   modules = read.csv(rave_options('module_lookup_file'), stringsAsFactors = F)
#
#   assertthat::assert_that(package_id %in% modules$PackageID, msg =
#                             'Group ID [' %&% package_id %&% '] not found in rave_options("module_lookup_file")')
#
#   sel = package_id %in% modules$PackageID
#   modules = apply(as.matrix(modules[sel, c('PackageID', 'GroupName', 'ModuleID', 'Name', 'ScriptPath', 'Author', 'Version', 'Packages')]), 1, function(m){
#     m = as.list(m)
#     m$Packages = str_trim(unlist(str_split(m$Packages, ';')))
#     m$GroupID = m$PackageID
#     m
#   })
#
#   names(modules) = lapply(modules, function(x){x$ModuleID})
#
#
#   rave_components = list()
#   function_env = new.env()
#   global_vars = new.env()
#
#   for(ii in 1:length(modules)){
#     re = module_parser(modules[[ii]]$ModuleID, modules[[ii]]$ScriptPath, .func = function_env)
#     re$rave_components$meta = modules[[ii]]
#     rave_components[[modules[[ii]]$ModuleID]] = as.list(re$rave_components)
#     list2env(as.list(re$data_env, all.names = T), global_vars)
#   }
#   dependencies = unique(unlist(lapply(modules, function(x){x$Packages})))
#
#   # write to package
#   pkgName = create_package(
#     package_id = package_id,
#     function_env = function_env,
#     global_vars = global_vars,
#     rave_components = list2env(rave_components),
#     dependencies = dependencies
#   )
#
#   return(pkgName)
#   # env = loadNamespace('RAVEbeauchamplab'); module_id = 'condition_explorer'
#   # module = env$rave_module(module_id = module_id)
#
# }
#
#
# create_package <- function(
#   package_id, function_env = new.env(), global_vars = new.env(), rave_components = new.env(),
#   dependencies = NULL, root_dir = NULL
# ){
#   ## Define package name
#   pkgName = to_package_name(package_id)
#
#   ## Prepare directory
#   if(is.null(root_dir)){
#     root_dir = file.path(rave_options('module_root_dir'), 'tempdir')
#   }
#   dir.create(root_dir, showWarnings = F, recursive = T)
#   pack_dir = file.path(root_dir, pkgName)
#   inst_dir = file.path(pack_dir, 'inst')
#   src_file = file.path(pack_dir, 'R', 'aaa.R')
#
#
#
#   # save script to pack_dir
#   if(file.exists(pack_dir)){
#     unlink(pack_dir, recursive=TRUE, force = T)
#   }
#
#   dependencies = unique(c(dependencies, 'rave'))
#
#
#   function_env$..dependencies = function(){}
#   body(function_env$..dependencies) = {dependencies}
#
#   function_env$.onLoad = function(libname, pkgname){
#     file = system.file('rave_components.RData', package = pkgname)
#     if(file.exists(file)){
#       # env = new.env()
#       # module_ids = names(as.list(env, all.names = T))
#
#       rave_env = do.call(':::', list(pkgname, '..rave_env'))
#       base::load(system.file('rave_components.RData', package = pkgname), envir = rave_env)
#
#       # lapply(module_ids, function(mid){
#       #   sapply(c("execute", "updates", "outputs", "inputs" ), function(comp_name){
#       #     junk = function(){}
#       #     formals(junk) = eval(env[[mid]][[comp_name]]$formals)
#       #     body(junk) = env[[mid]][[comp_name]]$body
#       #     environment(junk) = rave_env
#       #     junk
#       #   }, simplify = F, USE.NAMES = T) ->
#       #     comp
#       #   comp[['meta']] = env[[mid]]$meta
#       #   rave_env[[mid]] = comp
#       #   NULL
#       # })
#
#       invisible()
#     }
#   }
#
#   function_env$..rave_init = function(module_id){}
#   body(function_env$..rave_init) = rlang::quo_squash(rlang::quo({
#     exec_env = ..get_runtime_env(module_id = module_id, wrapper = T)
#
#     if(!environmentIsLocked(exec_env$static_env)){
#       ..rdata_file = system.file('vars.RData', package = !!pkgName)
#       base::load(file = ..rdata_file, envir = exec_env$static_env)
#
#       # re-direct functions so that we can call any functions
#       pkg = do.call('loadNamespace', list(package = !!pkgName))
#
#       lapply(names(as.list(pkg, all.names=T)), function(nm){
#         fun = pkg[[nm]]
#         if(is.function(fun)){
#           environment(fun) = exec_env$runtime_env
#         }
#         exec_env$static_env[[nm]] = fun
#       })
#     }
#   }))
#
#   function_env$..get_runtime_env = function(session, module_id, wrapper = F, name = 'runtime_env'){}
#   body(function_env$..get_runtime_env) = rlang::quo_squash(rlang::quo({
#     if(missing(session) || is.null(session)){
#       session = getDefaultReactiveDomain()
#     }
#     rave_comp = ..rave_env[[module_id]]
#
#     new_module = F
#     if(is.null(..module_env[[module_id]])){
#       ..module_env[[module_id]] = ModuleEnvir$new(
#         module_id = module_id,
#         label_name = rave_comp[['meta']][['Name']],
#         script_path = system.file(sprintf('module_%s.R', module_id), package = !!pkgName),
#         packages = !!pkgName
#       )
#       new_module = T
#     }
#
#     module = ..module_env[[module_id]]
#     exec_env = module$get_or_new_exec_env(session = session, parent_env = ..runtime_env)
#
#     if(wrapper){
#       return(exec_env)
#     }else{
#       return(exec_env[[name]])
#     }
#   }))
#
#
#   ## Initialize new package
#   package.skeleton(name=pkgName, path = root_dir, environment = function_env, force = T)
#   # remove man folder
#   unlink(file.path(pack_dir, 'man'), recursive = T, force = T)
#   dir.create(inst_dir, showWarnings = F, recursive = T)
#
#   ## Save global variables
#   if(!is.environment(global_vars)){
#     if(is.list(global_vars)){
#       global_vars = list2env(global_vars)
#     }else{
#       global_vars = new.env()
#     }
#   }
#
#   if(is.environment(global_vars) && length(as.list(global_vars, all.names = T))){
#     nm = names(as.list(global_vars, all.names = T))
#     save(list = nm, envir = global_vars, file = file.path(inst_dir, 'vars.RData'))
#   }
#
#   ## Save rave components
#   nm = names(as.list(rave_components, all.names = T))
#   if(is.list(rave_components)){
#     rave_components = list2env(rave_components)
#   }
#   save(list = nm, envir = rave_components, file = file.path(inst_dir, 'rave_components.RData'))
#
#   ## For each modules, write init file sprintf('module_%s.R', module_id)
#
#   lapply(names(rave_components), function(module_id){
#
#     fpath = file.path(inst_dir, sprintf('module_%s.R', module_id))
#
#     rlang::quo({
#       env = do.call('loadNamespace', list(package = !!pkgName))
#       env$..rave_init(module_id = !!module_id)
#
#       !!rave_components[[module_id]]$inputs$body
#
#       !!rave_components[[module_id]]$outputs$body
#
#       !!rave_components[[module_id]]$updates$body
#
#       !!rave_components[[module_id]]$execute$body
#
#     }) ->
#       quo
#
#     cat(deparse(rlang::quo_squash(quo)), sep = '\n', file = fpath)
#   })
#
#
#   ## Write package environments as well as rave_module function
#   deparse(quote({
#     rave_module = function(module_id, launch = T){
#       ..get_runtime_env(module_id = module_id)
#       module = ..module_env[[module_id]]
#       if(launch){
#         init_app(list(module))
#       }else{
#         return(module)
#       }
#     }
#   })[[2]]) ->
#     rave_module_func
#
#
#   cat(sprintf("\n..package_name = '%s';", pkgName),
#       "..rave_env = new.env();",
#       '..runtime_env = new.env();',
#       "..module_env = new.env();\n\n\n",
#       "#' @export",
#       rave_module_func,
#       '\n\n\nNULL',
#       paste0("#' @import ", dependencies),
#       'NULL\n',
#       sep = '\n', file = src_file, append = T)
#
#
#   ## Edit NAMESPACE and DESCRIPTION
#   disc = file.path(pack_dir, 'DESCRIPTION')
#   nsp = file.path(pack_dir, 'NAMESPACE')
#   tmp = c(
#     '# Generated by roxygen2: do not edit by hand',
#     readLines(disc),
#     'Imports:',
#     paste(
#       sapply(dependencies, function(p){
#         if(package_installed(p)){
#           ver = utils::packageVersion(p)
#           sprintf('\t%s (>= %s)', p, as.character(ver))
#         }else{
#           p
#         }
#       }),
#       collapse = ',\n'
#     )
#   )
#   writeLines(tmp, disc)
#   tmp = c('# Generated by roxygen2: do not edit by hand', readLines(nsp))
#   writeLines(tmp, nsp)
#
#
#   roxygen2::roxygenise(pack_dir, clean = T, roclets = c("collate", "namespace"))
#
#   ## Build package
#   # devtools::build(pack_dir)
#   pkg = devtools::install(pack_dir, reload = T, quick = T, local = T, dependencies = T, upgrade_dependencies = T,
#                           keep_source = T, quiet = T)
#
#   if(base::isNamespaceLoaded(pkgName)){
#     tryCatch({
#       detach(paste0('package:', pkgName), unload = T, character.only = T)
#     }, error = function(e){})
#     base::unloadNamespace(pkgName)
#   }
#
#   pkgName
# }
#
#
# module_parser <- function(ModuleID, ScriptPath, .rave_comp = new.env(), .func = new.env()){
#
#
#   global_env = globalenv()
#
#   src = readLines(ScriptPath)
#
#
#   .this = new.env(parent = global_env)
#   .inner = new.env(parent = .this)
#   .this$..call_stack = list()
#
#
#   .this$require = .this$library = function(package, ...){
#     pkg = as.character(substitute(package))
#     if(!pkg %in% installed.packages()[,1] && is.character(package)){
#       pkg = package
#     }
#     assertthat::assert_that(pkg %in% installed.packages()[,1], msg = sprintf('Package not exists. Please Install package: %s', pkg))
#     .this$..packages = c(.this$package, pkg)
#     do.call(base::`library`, args = list(
#       package = pkg, character.only = T
#     ))
#   }
#
#   .this$rave_prepare = .this$rave_ignore = function(...){}
#
#   .this$source = function(file, ...){
#     # try to locate file
#     module_dir = dirname(ScriptPath)
#     fpath = file.path(module_dir, file)
#     if(!file.exists(fpath)){
#       fpath = file
#     }
#     assertthat::assert_that(file.exists(fpath), msg = sprintf('Source file does not exist: %s', file))
#     .this$..call_stack = c(.this$..call_stack, rev(rlang::parse_quos(paste(readLines(fpath), collapse = '\n'), env = .this)))
#   }
#
#   .this$rave_updates = function(...){
#     quos = rlang::quos(...)
#     exprs = lapply(quos, rlang::quo_get_expr)
#     call = rlang::new_call(quote(rave_updates), as.pairlist(exprs))
#     call[['.env']] = quote(environment())
#
#     .rave_comp[['updates']] = list(
#       formals = rlang::quo_get_expr(rlang::quo(alist(.env = ..get_runtime_env(module_id = !!ModuleID, name = 'param_env')))),
#       body = rlang::quo_squash(rlang::quo({
#         with(.env, {!!call})
#       }))
#     )
#   }
#
#   .this$rave_inputs = function(...){
#     quos = rlang::quos(...)
#     exprs = lapply(quos, rlang::quo_get_expr)
#     call = rlang::new_call(quote(rave_inputs), as.pairlist(exprs))
#     call[['.env']] = quote(.env)
#
#     .rave_comp[['inputs']] = list(
#       formals = rlang::quo_get_expr(rlang::quo(alist(.env = ..get_runtime_env(module_id = !!ModuleID, name = 'param_env')))),
#       body = call
#     )
#   }
#
#   .this$rave_execute = function(...){
#     quos = rlang::quos(...)
#     exprs = lapply(quos, rlang::quo_get_expr)
#     call = rlang::new_call(quote(rave_execute), as.pairlist(exprs))
#     call[['.env']] = quote(.env)
#
#     .rave_comp[['execute']] = list(
#       formals = rlang::quo_get_expr(rlang::quo(alist(.env = ..get_runtime_env(module_id = !!ModuleID)))),
#       body = call
#     )
#   }
#
#   .this$rave_outputs = function(...){
#     quos = rlang::quos(...)
#     exprs = lapply(quos, rlang::quo_get_expr)
#     call = rlang::new_call(quote(rave_outputs), as.pairlist(exprs))
#     .rave_comp[['outputs']] = list(
#       formals = alist(),
#       body = call
#     )
#   }
#
#
#   # parse source
#   .this$..call_stack = rev(rlang::parse_quos(paste(src, collapse = '\n'), env = .this))
#
#   while (length(.this$..call_stack)){
#     n = length(.this$..call_stack)
#     quo = .this$..call_stack[[n]]
#     .this$..call_stack[[n]] = NULL
#
#     expr = rlang::quo_squash(quo)
#     res = eval_dirty(quo, env = .inner)
#   }
#
#   # to create a package for this, the best way is to write down all functions to scripts and cache all variables
#   # to a RData file and load them via a function
#   nms = names(as.list(.inner, all.names = T))
#
#   sapply(nms, function(key){
#     val = .inner[[key]]
#     if(is.function(val)){
#       .func[[key]] = val
#       rm(list = key, envir = .inner)
#     }
#     NULL
#   })
#
#
#   return(list(
#     rave_components = .rave_comp,
#     data_env = .inner,
#     func_env = .func
#   ))
# }
#

