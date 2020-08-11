# Documented on 2019-11-21

as_call2 <- function(..., .list = list(), .drop_nulls = TRUE){
  call = c(list(...), .list)
  if( .drop_nulls ){
    call = call[!vapply(call, is.null, FUN.VALUE = FALSE)]
  }
  as.call(call)
}

#' @title Session-based Module Runtime Environment Class
#' @author Zhengjia Wang
#' @description where all the module functions are executed. It's rarely created
#' manually, use \code{\link[rave]{get_module}} to create module, run with 
#' \code{start_app(m, test.mode=TRUE)}, and then inspect modules.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Load module
#' module <- get_module('ravebuiltins', 'power_explorer')
#' 
#' # Create execute environmen
#' execenv <- module$get_or_new_exec_env()
#' execenv$info()
#' 
#' }
#' 
#' @export
ExecEnvir <- R6::R6Class(
  classname = 'ExecEnvir',
  portable = FALSE,
  cloneable = TRUE,
  private = list(
    data_env = NULL,
    session = NULL,
    inputs = NULL,
    outputs = NULL,
    update = NULL,
    tabsets = NULL,
    executes = NULL
  ),
  public = list(
    #' @field .__rave_context__. context string for current instance, indicating
    #' whether the module is running locally (public, but internally used)
    .__rave_context__. = 'rave_running_local',
    
    #' @field .__rave_package__. current package name to run (public, 
    #' but internally used)
    .__rave_package__. = character(0),
    
    #' @field .__rave_module__. module ID (public, but internally used)
    .__rave_module__. = character(0),
    
    #' @field .__rave_module_instance__. self instance (public, but internally
    #' used)
    .__rave_module_instance__. = NULL,
    
    #' @field module_env \code{\link[rave]{ModuleEnvir}} instance
    module_env = NULL,
    
    #' @field cache_env cache environment to store key-value pairs locally
    cache_env = NULL,
    
    #' @field parent_env the parent/top environment of the module, usually 
    #' global environment or some name-space if the module is implemented as 
    #' an R package
    parent_env = NULL,
    
    #' @field wrapper_env stores all the utility functions. Some functions are 
    #' overridden there such as \code{\link[shiny]{observe}}, 
    #' \code{rave_checks}, or \code{eval_when_ready}. These functions behave 
    #' differently inside or outside of shiny context, and with or without data 
    #' loaded. The environment will be locked once the module is initialized. 
    #' The parent environment is \code{parent_env}
    wrapper_env = NULL,
    
    #' @field static_env stores module static functions. These functions are 
    #' evaluated under \code{parse_env} and then moved here. The environment
    #' is locked after initialization. Its parent environment is 
    #' \code{wrapper_env}
    static_env = NULL,
    
    #' @field param_env stores parameters and most of the user inputs. It can 
    #' also serve as a repository for global variables. Unlike the previous
    #' environments, \code{param_env} is unlocked, but module creators do not 
    #' have access to this environment directly. The parent environment is 
    #' \code{static_env}
    param_env = NULL,
    
    #' @field runtime_env where the main part of module is running. All shiny
    #' \code{\link[shiny]{observe}} and \code{\link[shiny]{observeEvent}} are 
    #' redirected to this environment by default (unless using 
    #' \code{shiny::observe}). All functions in \code{static_env} have access 
    #' to this environment. The parent environment is \code{param_env}
    runtime_env = NULL,
    
    #' @field async_env where asynchronous codes run
    async_env = NULL,
    
    #' @field parse_env environment where modules are parsed. The parent 
    #' environment is \code{runtime_env}. Once all functions are evaluated, 
    #' this environment is not used. However, module creators don't directly 
    #' access this environment once the module is initialized.
    parse_env = NULL,
    
    #' @field ns shiny name-space functions, is equivalent to 
    #' \code{shiny::NS(module_id)}. The goal is to add prefixes to module inputs
    #' so that two modules with the same input ID are named differently
    ns = NULL,
    
    #' @field auto_execute (Deprecated) whether to auto-calculate results
    auto_execute = TRUE,
    
    #' @field manual_inputIds character vector; name list of manually input IDs.
    #' Used when the algorithm takes long to run 
    manual_inputIds = NULL,
    
    #' @field rendering_inputIds character vector; name list of input IDs that 
    #' when one of the corresponding inputs is changed, then \code{rave_execute}
    #' will not get evaluated. Only the outputs are changed.
    rendering_inputIds = NULL,
    
    #' @field input_update expressions to update inputs
    input_update = NULL,
    
    #' @field register_output_events expressions to register outputs
    register_output_events = NULL,
    #' @field register_input_events expressions to register inputs
    register_input_events = NULL,
    
    #' @field execute module main function. The function is dynamically 
    #' generated. Don't call directly.
    execute = NULL,
    
    #' @field async_module (experimental) whether the module contains any 
    #' asynchronous part
    async_module = FALSE,
    
    #' @field global_reactives shiny global \code{reactives}, internal use only
    global_reactives = NULL,
    
    #' @field local_reactives shiny local \code{reactives}, internal use only
    local_reactives = NULL,
    
    #' @field internal_reactives internal reactive values to control some 
    #' elements, internal use only
    internal_reactives = NULL,
    
    #' @field ready_functions functions to run when the module is ready. The 
    #' functions are called at the last step of \code{\link[rave]{shinirize}}. 
    #' Usually it's used along with \code{eval_when_ready}, to make sure  
    #' \code{global_reactives} and \code{local_reactives} getting registered
    #' before functions calls
    ready_functions = NULL,
    
    #' @description (experimental) signal the modules to reload
    #' @return none
    reload = function(){
      if(is.reactivevalues(self$global_reactives)){
        self$global_reactives$force_refresh_all = Sys.time()
        self$global_reactives$has_data = Sys.time()
      }
    },
    
    #' @description garbage collection
    #' @return none
    finalize = function(){
      self$clean()
      catgl(sprintf('[%s] Runtime Environment Removed.', self$module_env$module_id))
    },
    
    #' @description print variables in different layers (environment)
    #' @return none
    info = function(){
      cat('- wrapper environment -\n')
      cat(ls(self$wrapper_env))
      cat('\n- static environment -\n')
      cat(ls(self$static_env))
      cat('\n- param environment -\n')
      cat(ls(self$param_env))
      cat('\n- runtime environment -\n')
      cat(ls(self$runtime_env))
    },
    
    #' @description print the memory address 
    #' @param ... ignored
    #' @return memory address
    print = function(...){
      env_address(self)
    },
    
    #' @description clean the environments to release the resource
    #' @return none
    clean = function(){
      # WARNING: this is not clean, but should be able to clear most of the large objects
      clear_env(self$parse_env)
      clear_env(self$param_env)
      clear_env(self$runtime_env)
      clear_env(self$async_env)
      # clear_env(self$static_env)
      if(!is.null(self$cache_env)){
        self$cache_env$reset()
      }
      invisible()
    },
    
    #' @description constructor
    #' @param session shiny session instance
    #' @param parent_env parent environment of this instance: package name space
    #' or global environment
    initialize = function(session = getDefaultReactiveDomain(),
                          parent_env = NULL){
      if(!is.null(session) && !inherits(session, c('ShinySession', 'session_proxy'))){
        private$session = session
      }
      self$ready_functions = list()
      self$internal_reactives = shiny::reactiveValues()
      self$cache_env = dipsaus::session_map()
      self$cache_env$has_locker = FALSE
      self$.__rave_module_instance__. = self

      # parent_env should be an unlocked environment that can be active binded
      if(!is.environment(parent_env)){
        parent_env = new.env(parent = globalenv(), hash = TRUE)
      }
      self$parent_env = parent_env

      # wrapper has all kind of util functions and it'll be sealed (locked)
      # One thing to notice that non of the functions within wrapper_env are
      # evaluated within itself. All are evaluated under "self", i.e. ExecEnvir
      # The reason why we use wrapper_env is because we don't want anyone to
      # change those functions as they are critical to modules.
      self$wrapper_env = new.env(parent = parent_env)

      # active bindings to data repository which allow us
      # the access to data loaded in data repo.
      rave_module_tools(self$wrapper_env)

      # static_env contains user self-defined functions. once initialized, they can
      # be read-only (in most of the cases).
      self$static_env = new.env(parent = self$wrapper_env)
      self$param_env = new.env(parent = self$static_env)
      self$param_env$..rave_future_env = new.env()

      # runtime_env, all variables will be stored within this environment, is the
      # one that real execute take place
      self$runtime_env = new.env(parent = self$param_env)
      self$static_env$..runtime_env = self$runtime_env
      self$static_env$.env = self$runtime_env
      self$static_env$..param_env = self$param_env
      
      self$async_env = new.env(parent = self$runtime_env)

      # Environment for parsers. All source file will be parsed here
      # it can get access to runtime_env.
      # Old scheme was to parse src in static env and change function environment to
      # runtime_env, this is dangerous. So I come up with this solution
      self$parse_env = new.env(parent = self$runtime_env)


      self$ns = base::I

      bind_wrapper_env(self, self$wrapper_env)

      self$wrapper_env$source = function(file, local = TRUE, ...){
        if(environmentIsLocked(self$static_env)){
          return()
        }

        # Try to use the file under the same dir
        dir = dirname(self$module_env$script_path)
        tmp_file = file.path(dir, file)
        if(file.exists(tmp_file)){
          catgl('Try to source from [', tmp_file, ']')
          self$runtime_env$.__tmp_file = tmp_file
          eval(quote(base::source(.__tmp_file, local = TRUE)), self$runtime_env)
        }else if(file.exists(file)){
          # cat2('File [', tmp_file, '] does not exists, try to look for it.', level = 'INFO')
          self$runtime_env$.__tmp_file = file
          eval(quote(base::source(.__tmp_file, local = TRUE)), self$runtime_env)
        }else{
          catgl('File [', file, '] does not exists.', level = 'ERROR')
          return()
        }

        # Speed up
        # copy_env(self$parse_env, self$static_env, deep = F)
        list2env(as.list(self$runtime_env, all.names = T), envir = self$static_env)


      }

      # advanced usage
      self$wrapper_env$getDefaultReactiveDomain = function(
        session = shiny::getDefaultReactiveDomain()
      ){
        id = self$module_env$module_id
        if(is.null(session)){
          session = private$session
        }
        if(inherits(session, 'ShinySession')){
          return(session$makeScope(id))
        }else{
          return(session)
        }
      }

      self$wrapper_env$getDefaultReactiveInput = function(
        session = self$wrapper_env$getDefaultReactiveDomain()
      ){
        
        if(inherits(session, 'ShinySession')){
          return(session$makeScope(self$module_env$module_id)$input)
        }
        
        if(inherits(session, 'session_proxy')){
          return(session$input)
        }else{
          stop('No module detected, please run "self$register_module(...)" to register module.')
        }
        return(NULL)
      }

      self$wrapper_env$getDefaultReactiveOutput = function(
        session = self$wrapper_env$getDefaultReactiveDomain()
      ){
        
        if(inherits(session, 'ShinySession')){
          return( session$makeScope(self$module_env$module_id)$output )
        }
        
        if(inherits(session, 'session_proxy')){
          return( session$output )
        }else{
          stop('No module detected, please run "self$register_module(...)" to register module.')
        }
        return(NULL)
      }


      # lockEnvironment(self$wrapper_env)

    },
    
    #' @description reset the runtime environment, rarely used
    #' @param inputs reactive value list
    #' @return none
    reset = function(inputs){
      if(shiny::is.reactivevalues(inputs)){
        inputs = shiny::isolate(shiny::reactiveValuesToList(inputs))
      }
      rm(list = ls(self$runtime_env), envir = self$runtime_env)
      for(nm in self$input_ids){
        assign(nm, inputs[[nm]], envir = self$runtime_env)
      }
      invisible()
    },
    
    #' @description (deprecated) copy the instance locally
    #' @param session_id character
    #' @param data_env where the data is stored, default is the environment 
    #' returned by \code{\link[rave]{getDefaultDataRepository}}
    #' @return a copied instance
    copy = function(
      session_id = '__fake_runtime_env__', data_env = getDefaultDataRepository()
    ){
      # deep clone, but sharing the data, module environment
      fakesession = fake_session(rave_id = session_id)

      m = self$module_env
      new_exec = m$get_or_new_exec_env(
        parent_env = data_env, session = fakesession, new = T
      )


      # migrate param_env
      list2env(as.list(self$param_env, all.names = TRUE), new_exec$param_env)
      new_exec$self$module_env$load_script(session = fakesession)

      return(new_exec)
    },
    
    #' @description (deprecated) execute module with given parameter
    #' @param param named list
    #' @param async whether to run the whole module
    #' @param plan future plan
    #' @return runtime environment
    execute_with = function(param, async = FALSE, plan = NULL){
      lapply(names(param), function(nm){
        self$runtime_env[[nm]] = param[[nm]]
        self$param_env[[nm]] = param[[nm]]
      })
      res = self$execute(async = async)
      if(async){
        catgl('Execute_with async not implemented.')
      }
      return(invisible(self$runtime_env))
    },

    #' @description returns names of a list, if names are null, 
    #' returns blank characters
    #' @param x a list
    #' @return the names of the list
    names = function(x){
      if(is.list(x)){
        nm = base::names(x)
        if(length(x) != length(nm) && is.null(nm)){
          nm = rep('', length(x))
        }
      }else{
        nm = base::names(x)
      }
      return(nm)
    },
    
    #' @description register \code{\link[rave]{ModuleEnvir}} instance
    #' @param module_env \code{\link[rave]{ModuleEnvir}} instance. The modules
    #' are shared across different sessions, but to run the module, we need 
    #' to create runtime environment, which is \code{ExecEnvir}
    #' @return none
    register_module = function(module_env){
      
      if(!is.null(self$module_env)){
        catgl('Overriding Module Environment.', level = 'WARNING')
      }
      
      self$module_env = module_env
      self$ns = shiny::NS(module_env$module_id)
      
      # Set context
      if( module_env$from_package ){
        self$.__rave_package__. = module_env$package_name
      }else{
        catgl('Module is not from a known package, try to read from current context...', level = 'INFO')
        ctx = rave_context(disallowed_context = 'default')
        self$.__rave_package__. = ctx$package
      }
      
      self$.__rave_module__. = module_env$module_id
      
      self$register_context(self$.__rave_context__.)
      
      invisible()
    },
    
    #' @description Register 'RAVE' context for current environment 
    #' (internally used)
    #' @param context context string to indicate whether the module is running 
    #' locally
    #' @return None
    register_context = function(context = c('rave_running', 'rave_running_local')){
      context = match.arg(context)
      self$.__rave_context__. = context
      rave_context(senv = self, tenv = self$wrapper_env)
      rave_context(senv = self, tenv = self$static_env)
      rave_context(senv = self, tenv = self$param_env)
      rave_context(senv = self, tenv = self$runtime_env)
      rave_context(senv = self, tenv = self$parse_env)
      invisible()
    },
    
    #' @description parse input components
    #' @param ... shiny input calls, such as \code{textInput('id', 'Name', ...)}
    #' @param .input_panels,.tabsets together define the input layouts
    #' @param .env ignored, debug only
    #' @param .manual_inputs input IDs that won't cause module re-calculate
    #' when inputs are updated
    #' @param .render_inputs input IDs that only trigger render functions when
    #' updated
    #' @return none
    rave_inputs = function(..., .input_panels = list(), .tabsets = list(), 
                           .env = NULL, .manual_inputs = NULL, .render_inputs = NULL){
      .tabsets = .input_panels
      if( !is.list(.tabsets) ){
        .tabsets = list()
      }
      package_name = get_package_name()
      self$manual_inputIds = .manual_inputs
      self$rendering_inputIds = .render_inputs
      
      names(.tabsets) = sapply(names(.input_panels), function(nm){
        s = stringr::str_trim(unlist(stringr::str_split(nm, '\\[|\\]')))
        s = s[s!='']
        s[length(s)]
      })


      .tabsetParams = lapply(names(.input_panels), function(nm){
        s = stringr::str_trim(unlist(stringr::str_split(nm, '\\[|\\]')))
        s = s[s!='']
        re = list(
          collapsed = '-' %in% s,
          headerColor = tryCatch({
            col = NULL
            tmp = s[stringr::str_detect(s, '^#')]
            if(length(tmp) == 1){
              col2rgb(tmp)
              col = tmp
            }
            col
          }, error = function(e){NULL})
        )
        re
      })
      names(.tabsetParams) = names(.tabsets)
      quos = rlang::quos(...)
      parsers = comp_parser()

      x = lapply(quos, parsers$parse_quo)
      names(x) = ids = sapply(x, function(comp){comp$inputId})

      if(!'Local Variables' %in% names(.tabsets)){
        rest_inputs = ids[!ids %in% unlist(.tabsets)]
        if(length(rest_inputs)){
          .tabsets[['Local Variables']] = c(rest_inputs)
        }
      }


      lapply(seq_along(.tabsets), function(ii){
        tabName = names(.tabsets)[ii]
        url = openwetware_url(tabName, type = 'input')
        rlang::quo({
          do.call(box, args = c(
            list(width = 12,
                 title = !!tabName,
                 collapsible = TRUE,
                 box_link = !!url),
            !!.tabsetParams[[tabName]],
            !!lapply(.tabsets[[tabName]], function(inputIds){
              if(length(inputIds) == 1){
                comp = x[[inputIds]]
                return(comp$expr)
              }else{
                n = length(inputIds)
                mod2 = n %% 2
                mod3 = n %% 3
                if(mod3 == 0){
                  flex_basis = rep('flex-basis: 33%;', n)
                }else if(mod2 == 0){
                  flex_basis = rep('flex-basis: 50%;', n)
                }else if(mod3 == 1){
                  flex_basis = rep('flex-basis: 50%;', n)
                  flex_basis[length(flex_basis)] = 'flex-basis: 100%;'
                }else{
                  flex_basis = rep('flex-basis:33%;', n)
                  flex_basis[length(flex_basis) - c(0:1)] = 'flex-basis:50%;'
                }

                return(rlang::quo(
                  do.call(div, args = c(
                    list(class = 'rave-grid-inputs'),
                    !!lapply(seq_len(n), function(jj){
                      inputId = inputIds[[jj]]
                      rlang::quo(
                        do.call(div, args = c(
                          list(style = !!flex_basis[[jj]], !!x[[inputId]]$expr)
                        ))
                      )
                    })
                  ))
                ))
              }
            })
          ))

        })

      }) ->
        ui_inputs

      ui_inputs = rlang::quo({
        do.call(shiny::fluidRow, args = !!ui_inputs)
      })
      ui_inputs = rlang::quo_squash(ui_inputs)

      private$inputs = list(
        quos = ui_inputs,
        comp = x
      )

      self$register_input_events = function(input, output, session, local_data){
        lapply(x, function(comp){
          comp$observers(input, output, session, local_data, self)
        })
      }

      invisible()
    },
    
    
    #' @description parse output components
    #' @param ... shiny output calls, such as \code{plotOutput('id', 'Title')}
    #' @param .output_tabsets,.tabsets together define the output layouts
    #' @param .env debug use
    #' @return none
    rave_outputs = function(..., .output_tabsets = list(), .tabsets = list(), .env = NULL){
      .tabsets = .output_tabsets
      quos = rlang::quos(...)
      stopifnot2(length(quos) > 0, msg = 'No output defined!')
      parsers = comp_parser()
      x = lapply(names(quos), function(nm){
        re = parsers$parse_quo(quos[[nm]])
        re$label = nm
        re
      })

      ids = sapply(x, function(comp){comp$outputId})
      names(x) = ids

      #### Generate UIs for output

      # 1. tabsets in .tabsets
      widths = .tabsets[['width']]
      .tabsets[['width']] = NULL
      widths %?<-% 12L

      if(length(.tabsets)){
        names = names(.tabsets)
        ntabs = length(names)
        if(length(widths) < ntabs){
          widths = rep(widths, ntabs)
        }

        lapply(seq_len(ntabs), function(ii){
          nm = names[[ii]]
          ids = .tabsets[[nm]]

          quo_panels = lapply(seq_along(ids), function(ii){
            comp = ids[ii]
            title = names(comp)
            
            as_call2(
              quote(shiny::tabPanel),
              title = title,
              quote(div(
                class = 'rave-abs-right',
                div(
                  class = 'btn btn-box-tool force-recalculate',
                  shiny::icon('refresh')
                ),
                div_elastic(css_selector = '.tab-pane')
              )),
              as_call2(
                quote(shiny::fluidRow),
                .list = lapply(comp[[1]], function(output_id){
                  comp = x[[output_id]]
                  width = comp[['width']]; width %?<-% 12L
                  as_call2(
                    quote(shiny::column),
                    as_call2(
                      quote(shiny::h4),
                      comp$label
                    ),
                    comp$expr,
                    width = width
                  )
                })
              )
            )
            
            # rlang::quo_squash(rlang::quo({
            #   shiny::tabPanel(
            #     title = !!title,
            #     div(
            #       class = 'rave-abs-right',
            #       div(
            #         class = 'btn btn-box-tool force-recalculate',
            #         shiny::icon('refresh')
            #       ),
            #       div_elastic(css_selector = '.tab-pane')
            #     ),
            #     do.call(shiny::fluidRow, args = !!{lapply(comp[[1]], function(output_id){
            #       comp = x[[output_id]]
            #       width = comp[['width']]; width %?<-% 12L
            #       expr = quote(shiny::column())
            #       expr = c(as.list(expr), list(
            #         shiny::h4(comp$label),
            #         comp$expr,
            #         width = width
            #       ))
            #       as.call(expr)
            #     })}
            #     )
            #   )
            # }))
          })
          
          as_call2(
            quote(tabBox),
            title = nm,
            width = widths[[ii]],
            box_link = openwetware_url(nm, type = 'output'),
            .list = quo_panels
          )
          
          # quo_box = rlang::quo({
          #   do.call(shinydashboard::tabBox,
          #           args = c(
          #             list(title = !!nm, width = !!widths[[ii]]),
          #             !!quo_panels
          #           )
          #   )
          # })
          # quo_box
          
        }) ->
          tab_boxes
      }else{
        tab_boxes = NULL
      }

      left_ids = ids[!ids %in% unlist(.tabsets)]

      single_boxes = lapply(left_ids, function(nm){
        comp = x[[nm]]
        width = comp$width; width %?<-% 12L
        margin = comp$margin
        url = openwetware_url(comp$label, type = 'output')
        if( length(margin) && is.numeric(margin) ){
          as_call2(
            expand_box,
            width = width,
            title = comp$label,
            collapsible = TRUE,
            box_link = url,
            as_call2(
              quote(shiny::div),
              style = sprintf('margin: %.0fpx;', margin),
              comp$expr
            )
          )
          # rlang::quo({
          #   expand_box(
          #     width = width,
          #     title = comp$label,
          #     collapsible = TRUE,
          #     div(
          #       style = sprintf('margin: %.0fpx;', !!margin),
          #       !!comp$expr
          #     )
          #   )
          # })
        }else{
          as_call2(
            expand_box,
            width = width,
            title = comp$label,
            box_link = url,
            collapsible = TRUE,
            comp$expr
          )
          # rlang::quo({
          #   expand_box(
          #     width = width,
          #     title = comp$label,
          #     collapsible = TRUE,
          #     !!comp$expr
          #   )
          # })
        }
        
      })

      ui_comps = as_call2(
        quote(shiny::fluidRow),
        .list = c(tab_boxes, single_boxes)
      )
      # ui_comps = rlang::quo(do.call(shiny::fluidRow, args = c(!!tab_boxes, !!single_boxes)))
      # ui_comps = rlang::quo_squash(ui_comps)

      #### Reactive functions
      private$outputs = list(
        quos = ui_comps,
        comp = x
      )
      self$register_output_events = function(input, output, session, local_data){
        lapply(x, function(comp){
          comp$observers(input, output, session, local_data, self)
        })
      }
      invisible()
    },
    
    #' @description input initialization when \code{iEEG/ECoG} data are imported
    #' @param ... R expressions
    #' @param .env for debug use
    rave_updates = function(..., .env = NULL){
      quos = rlang::quos(...)
      quo_names = names(quos)
      quos = lapply(quos, rlang::quo_squash)
      names(quos) = quo_names
      private$update = quos

      self$input_update = function(input, session = getDefaultReactiveDomain(), init = FALSE){
        start = Sys.time()
        input = dipsaus::drop_nulls(input)
        if(!init){
          # Deprecated, do nothing
          return(invisible())
        }
        # if(is.null(session)){
        #   session = getDefaultReactiveDomain() #private$session
        # }
        var_names = names(private$update)
        n_errors = c(0,0)
        envir = environment()
        errors = NULL
        # passed = TRUE
        for(quo in private$update[var_names == '']){
          tryCatch({
            dipsaus::eval_dirty( quo, env = self$param_env )
          },error = function(e){
            catgl('Error in updating input (initialization)', level = 'ERROR')
            s = capture.output(traceback(e))
            lapply(s, catgl, level = 'ERROR')
            envir$n_errors[1] = envir$n_errors[1] + 1
            envir$errors = c(envir$errors, as.character(e))
            # envir$passed = FALSE
          })

          # if(!passed){
          #   break();
          # }
        }



        for(varname in var_names[var_names != '']){
          # if(!passed){
          #   break;
          # }
          tryCatch({
            comp = private$inputs$comp[[varname]]
            if(is.null(comp)){
              return()
            }
            new_args = dipsaus::eval_dirty(
              private$update[[varname]], data = input, env = self$param_env
            )

            comp$updates(session = session, .args = new_args)
          },error = function(e){
            catgl('Error in updating input ', varname, level = 'ERROR')
            s = capture.output(traceback(e))
            lapply(s, catgl, level = 'ERROR')
            envir$n_errors[2] = envir$n_errors[2] + 1
            envir$passed = FALSE
          })
        }


        end = Sys.time()
        delta = time_diff(start, end)
        catgl(sprintf('Updating inputs takes %.2f %s. Total errors: %d + %d', delta$delta, delta$units, n_errors[1], n_errors[2]))


        # Activate this module if no error occurred during input-update phase
        hist_len = isolate(length(self$global_reactives$view_history))
        if(length(hist_len)){
          if(sum(n_errors)){
            global_reactives$view_history[[hist_len]]$activated = FALSE
          }else{
            global_reactives$view_history[[hist_len]]$activated = TRUE
          }
        }


        return(list(
          n_errors = n_errors,
          init_error_msgs = errors
        ))
      }
      invisible()
    },
    
    #' @description parse, and compile to main function
    #' @param ... R expressions
    #' @param auto whether the module should run automatically
    #' @param async_vars variables further passed to \code{async} module
    #' @param .env debug use
    #' @return none, but \code{ExecEnvir$execute} will be generated.
    rave_execute = function(..., auto = TRUE, .env = NULL, async_vars = NULL){
      quos = rlang::quos_auto_name(rlang::quos(...))
      quos = sapply(quos, rlang::quo_squash, simplify = FALSE, USE.NAMES = TRUE)

      normal_quos = quos[!names(quos) %in% 'async']
      private$executes = c(private$executes, normal_quos)
      async_quo = quos[['async']]
      self$async_module = !is.null(async_quo)
      self$auto_execute = auto

      self$execute = function(async = FALSE, force = FALSE){
        if(!force && !self$auto_execute){
          return()
        }
        self$runtime_env$.is_async = async
        async_future = NULL
        

        if(async){
          if(self$async_module){
            clear_env(self$async_env)
            self$async_env[['..async_quo']] = async_quo
            self$async_env[['..async_var']] = async_vars

            packages = stringr::str_match(search(), '^package:(.+)$')[,2] 
            packages = packages[!is.na(packages)]
            packages = unique(packages, self$module_env$packages)

            self$param_env$..rave_future_obj =
              future::future({
                dipsaus::eval_dirty(..async_quo)#, env = async_env)
                if(is.null(..async_var)){
                  return(environment())
                }else{
                  re = sapply(..async_var, get0, simplify = FALSE, USE.NAMES = TRUE)
                  re
                }
              }, packages = packages, evaluator = future::multiprocess, 
              envir = self$async_env,
              gc = FALSE)
          }
        }else{
          if(length(private$executes)){
            lapply(private$executes, dipsaus::eval_dirty, env = self$runtime_env)
          }
        }

        return(self$param_env[['..rave_future_obj']])
      }
    },
    
    #' @description (experimental) cache R expression in browser 
    #' \code{localStorage}
    #' @param expr R expression
    #' @param session shiny session instance
    set_browser = function(expr, session = getDefaultReactiveDomain()){
      if(is.null(session)){
        session = private$session
      }

      current_key = add_to_session(session)

      children_keys = add_to_session(session, 'rave_linked_by', NULL)
      children_keys = children_keys[!children_keys %in% current_key]
      # children_keys = unique(c(current_key, children_keys))

      module_id = self$module_env$module_id

      lapply(children_keys, function(storage_key){
        session$sendCustomMessage('rave_set_storage', list(
          module_id = module_id,
          expr = expr,
          storage_key = storage_key,
          current_key = current_key
        ))
      })

    },
    
    #' @description generate input panels according to parsed \code{rave_inputs}
    #' @param sidebar_width integer from 1 to 11, the width of the input panels
    #' @return HTML tags
    generate_input_ui = function(sidebar_width = 3L){
      ns = self$ns
      # env = environment()

      more_btns = list(
        # vignette = tags$li(actionLink(self$ns('..vignette'), 'Show Module Description')),
        async = tags$li(actionLink(self$ns('..async_run'), 'Run Algorithm (Async)')),
        export = tags$li(actionLink(self$ns('..incubator'), 'Exports'))
      )

      # exports
      export_func = names(as.list(self$static_env))
      is_export_func = vapply(export_func, function(x){
        is.function(self$static_env[[x]]) && stringr::str_detect(x, 'export_')
      }, FUN.VALUE = logical(1))
      if(length(is_export_func) == 0 || sum(is_export_func) == 0){
        more_btns[['export']] = NULL
      }

      # Async
      if(!self$async_module){
        more_btns[['async']] = NULL
      }

      more_ui = NULL
      
      # TODO: do we really want more...? 
      # if(length(more_btns)){
      #   names(more_btns) = NULL
      #   more_ui = box(
      #     title = 'More...',
      #     collapsed = T,
      #     tags$ul(
      #       class = 'rave-grid-inputs',
      #       tagList(more_btns)
      #     ),
      #     width = 12,
      #     collapsible = T
      #   )
      # }


      if(sidebar_width == 0){
        sidebar_width = '3 hidden';
      }

      div(
        class = sprintf('col-sm-%s rave-input-panel', sidebar_width),
        # dipsaus::eval_dirty(private$inputs$quos, env = new.env(), data = self$parent_env),
        rlang::eval_tidy(private$inputs$quos, data = as.list(self$parent_env)),
        fluidRow(
          uiOutput(self$ns('..params_current')),
          more_ui
        )
      )
    },
    
    #' @description generate outputs labels according to parsed 
    #' \code{rave_outputs}
    #' @param sidebar_width integer from 1 to 11, the width of the input panels,
    #' the output panel width is calculated as \code{12-sidebar_width}
    #' @return HTML tags
    generate_output_ui = function(sidebar_width = 3L){
      ns = self$ns
      # env = environment()
      div(
        class = sprintf('col-sm-%d rave-output-panel', 12L - sidebar_width),
        # dipsaus::eval_dirty(private$outputs$quos, env = new.env(), data = self$parent_env)
        rlang::eval_tidy(private$outputs$quos, data = as.list(self$parent_env))
      )

    },
    
    #' @description (deprecated) check if variable is shared across modules.
    #' Please use \code{cache_input} instead to get variable values.
    #' @param inputId input ID
    is_global = function(inputId){
      tabsets = private$tabsets
      if(length(tabsets) == 0){
        return(FALSE)
      }
      nms = names(tabsets)
      ts = unlist(tabsets[stringr::str_detect(stringr::str_to_lower(nms), 'global')])
      if(length(ts) == 0){
        return(FALSE)
      }
      return(inputId %in% ts)
    }
  ),
  active = list(
    
    #' @field input_ids vector of input IDs (read-only)
    input_ids = function(){
      names(private$inputs$comp)
    },
    
    #' @field input_labels vector of input labels (read-only)
    input_labels = function(){
      re = lapply(private$inputs$comp, function(x){x$args$label})
      names(re) = names(private$inputs$comp)
      return(re)
    },
    
    #' @field output_labels vector of output labels (read-only)
    output_labels = function(){
      re = lapply(private$outputs$comp, function(x){x$label})
      names(re) = names(private$outputs$comp)
      return(re)
    },
    
    #' @field output_ids vector of output IDs (read-only)
    output_ids = function(){
      names(private$outputs$comp)
    }
  )
)


