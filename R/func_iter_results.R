#' @export
do_nothing <- function(...){

}

#' @export
iter_results <- function(
  module, inputId,
  valueList, outputs, param = NULL, plan = NULL, async = FALSE,
  iter_over_electrodes = T, execute = T,
  par.settings = par()
){
  # clear environment
  module = ModuleEnvir$new(
    module_id = '__FAKE',
    label_name = module$label_name,
    script_path = module$script_path,
    author = module$author,
    version = module$version,
    packages = module$packages
  )

  data_env = new.env(parent = getDefaultDataRepository())
  execenv = module$get_or_new_exec_env(data_env = data_env)
  execenv$wrapper_env$cache = function(key,val,...){if(!missing(val)){return(val)}}
  execenv$wrapper_env$cache_input = function(key,val,...){if(!missing(val)){return(val)}}
  execenv$wrapper_env$rave_prepare = function(...){}
  execenv$wrapper_env$rave_ignore = function(...){}
  module$load_script()

  rave_prepare()


  if(iter_over_electrodes){
    all_electrodes = as.integer(unlist(valueList))
    data_env$..all_electrodes = all_electrodes
    all_electrodes = local({valid_electrodes(..all_electrodes)}, envir = data_env)

    if(length(all_electrodes) == 0){
      stop(shiny::safeError('Electrodes not valid.'))
    }
    if(exists('electrodes', envir = data_env)){
      exist_e = all_electrodes[all_electrodes %in% get('electrodes', envir = data_env)]
      rest_e = NULL
    }else{
      exist_e = NULL
      rest_e = NULL
    }

    valueList = as.list(as.numeric(c(exist_e, rest_e)))
  }

  run = function(val){
    if(iter_over_electrodes && !val %in% exist_e){
      cat('Electrode - ', val, ' has not been loaded', sep = '')
      return(NULL)
    }
    assign(inputId, val, envir = execenv$runtime_env)

    tryCatch({
      result = execenv$execute(plan = plan, async = async)
      if(async){
        # NOT IMPLEMENTED
      }
      funcs = lapply(outputs, get, envir = execenv$runtime_env)
      funcs
    }, error = function(e){
      cat(capture.output(traceback(e)), sep = '\n')
    }) ->
      funcs
    return(invisible(funcs))
  }


  if(is.null(param)){
    param = lapply(execenv$private$update, function(x){
      res = lazyeval::lazy_eval(x)
      if(is.null(res[['selected']])){
        res[['value']]
      }else{
        res[['selected']]
      }
    })
  }
  for(varname in names(param)){
    assign(varname, param[[varname]], envir = execenv$static_env)
  }



  label = execenv$input_labels[[inputId]]


  if(execute){
    lapply(valueList, function(val){
      funcs = run(val)

      if(length(funcs)){
        names(funcs) = outputs
        base::cat(label, "-", safe_str_c(val, collapse = ', '))
        lapply(names(funcs), function(nm){

          tryCatch({
            base::cat(execenv$output_labels[[nm]])
            funcs[[nm]]()
          }, error = function(e){
            cat(capture.output(traceback(e)), sep = '\n')
            return(NULL)
          }) ->
            re
          return(re)
        }) ->
          re
        return(re)
      }
      return(NULL)
    }) ->
      re
    return(invisible(re))
  }else{
    re = list(
      call = function(val){
        funcs = run(val)
        if(length(funcs)){
          names(funcs) = outputs
          base::cat(label, "-", safe_str_c(val, collapse = ', '))
          lapply(names(funcs), function(nm){

            tryCatch({
              base::cat(execenv$private$outputs[[nm]]$title)
              par(par.settings)
              funcs[[nm]]()
            }, error = function(e){
              cat(e)
            })

          })
        }
        return(invisible())
      },
      valueList = valueList,
      label = label
    )
  }

}



#' @export
rave_deparse <- function(obj){
  paste0(deparse(obj), collapse = '')
}

# Function to export as report

# export_report <- function(module, inputId, valueList, param = NULL, outputs,
#                           output_format = 'html_document', run_pandoc = T, envir = new.env(), ...){
#   if(is.null(module$rmd_path)){
#     s = readLines(system.file('modules/blank_template', package = 'rave'))
#     ss = readLines(module$script_path)
#     ptrn = "source\\(['\"]([^'\"]*)['\"]"
#     sel = str_detect(ss, ptrn)
#     if(sum(sel)){
#       ss[sel] = sapply(str_match(ss[sel], ptrn)[,2], function(sss){
#         sprintf('source("%s")', tail(unlist(str_split(sss, '/')),1))
#       })
#     }
#     ss = str_c(ss, collapse = '\n')
#     s = sprintf(str_c(s, collapse = '\n'), module$label_name, ss)
#     s = unlist(str_split(s, '\\n'))
#   }else{
#     s = readLines(module$rmd_path)
#   }
#   s = str_replace_all(s, 'rave_prepare\\(', 'do_nothing(')
#   temp_dir = dirname(tools::file_path_as_absolute(module$script_path))
#   temp_fname = 'rave_report.rmd'
#   fpath = file.path(temp_dir, temp_fname)
#
#   content = readLines(system.file('modules/export_template', package = 'rave'))
#   content = str_c(content, collapse = '\n')
#   if(length(param) == 0 || !is.list(param)){
#     param = NULL
#     d_param = 'NULL'
#     str_param = '* (Default parameters)'
#   }else{
#     d_param = rave_deparse(param)
#     str_param = str_c(sapply(names(param), function(nm){sprintf('* `%s`: %s', nm, rave_deparse(param[[nm]]))}), collapse = '\n')
#   }
#   content = (sprintf(
#     content,
#     str_param,
#     inputId,
#     str_c('`', outputs, '`', collapse = ", "),
#     '__tmp_export__',
#     temp_fname,
#     inputId,
#     rave_deparse(valueList),
#     rave_deparse(outputs),
#     d_param
#   ))
#   content = unlist(str_split(content, '\\n'))
#
#   appends = unlist(lapply(valueList, function(val){
#     s = str_c(
#       '',
#       '```{r, results = "asis", echo = F}',
#       'cat("### ", results$label, "-", "%s")',
#       '```',
#       '',
#       '```{r, echo=F, message=F, warning=F, fig.width=10, fig.height=5, out.width="900px", out.height="400px", dpi=300}',
#       'try({results$call(%s)})',
#       '```',
#       '',
#       sep = '\n'
#     )
#     val = rave_deparse(val)
#     str_split(sprintf(s, val, val), '\\n')
#
#   }))
#   writeLines(c(s, content, appends), fpath)
#
#   envir$source = function(file, local = T, ...){
#     # try to find file, if not, try to use the one under modules's dir
#     if(!file.exists(file)){
#       logger('File [', file, '] does not exists, try to look for it.', level = 'INFO')
#       dir = dirname(module$script_path)
#       file = tail(as.vector(str_split(file, '/', simplify = T)), 1)
#       file = file.path(dir, file)
#     }
#     logger('Trying to source [', file, ']')
#     envir$.__tmp_file = file
#     eval(quote(base::source(.__tmp_file, local = T)), envir)
#   }
#   envir$rave_inputs = function(..., .env = NULL){
#     rave_inputs(..., .env = envir)
#   }
#   envir$rave_execute = function(..., .env = NULL){
#     rave_execute(..., .env = envir)
#   }
#   envir$rave_updates = function(..., .env = NULL){
#     rave_updates(..., .env = envir)
#   }
#
#   db = options('rave.logger.disabled')
#   if(!is.null(db)){
#     db = db[[1]]
#   }
#   options(rave.logger.disabled = TRUE)
#   on.exit({
#     options(rave.logger.disabled = db)
#   })
#   output_file = rmarkdown::render(
#     fpath, output_format = output_format, run_pandoc = run_pandoc, envir = envir,
#     ...
#   )
#
# }


