#' Parse 'RAVE' Module Contents
#' @param content characters, R code to parse into expressions
#' @param env environment to parse the code into, please specify the context
#' @param evaluate whether to evaluate parse expression into \code{env}
#' @param chunks whether to respect code notations and chunk the code into 
#' separate parts
#' @details 
#' If \code{"evaluate=TRUE"}, then the parse code will be evaluated 
#' within \code{env} and returns a logical value: \code{TRUE} means the content has
#' something, otherwise returns \code{FALSE}
#' 
#' If \code{"evaluate=FALSE"}, returns the parsed expression and add attributes
#' about the names of each chunk and whether they are asynchronous 
#' 
#' @return See details.
get_content <- function(
  content, env, evaluate = TRUE, chunks = FALSE
){
  keyword = c('Start', 'End')
  start_line = which(stringr::str_detect(content, paste0('^# >>>>>>>>>>>> ', keyword[1])))
  end_line = which(stringr::str_detect(content, paste0('^# <<<<<<<<<<<< ', keyword[2])))
  has_inputs = FALSE
  if(length(start_line)){
    start_line = start_line[1]
    end_line = end_line[end_line > start_line]
    if(length(end_line)){
      end_line = end_line[1]
      has_inputs = TRUE
    }
  }
  if(!has_inputs){
    return(FALSE)
  }
  content = content[(start_line+1):end_line]
  content = stringr::str_trim(content)
  content = content[content!='']
  # If sep exists, first one MUST be a regex pattern with "^.....$"
  chunk_names = ''
  auto = TRUE
  async_vars = NULL
  if(chunks){
    sep = '^#{6}\'( @[^#]+|)[\\ #]{0,}$'
    sel = stringr::str_detect(content, sep)
    
    if(sum(sel)){
      idx = which(sel)
      if(!1 %in% idx){
        idx = c(1, idx + 1)
        content = c('######\' @auto=TRUE', content)
      }
      chunk_names = stringr::str_match(content[idx], sep)[,2]
      chunk_names = stringr::str_remove_all(chunk_names, '[\\ @]')
      
      # 1. auto = true or false
      auto_chunk = stringr::str_detect(chunk_names, '^auto=')
      if(any(auto_chunk)){
        auto_chunk = stringr::str_to_lower(chunk_names[auto_chunk][1])
        if(stringr::str_detect(auto_chunk, '=false')){
          auto = FALSE
        }
      }
      
      # 2. find async
      async_chunk = stringr::str_detect(chunk_names, '^async(,|$)')
      if(any(async_chunk)){
        async_idx = which(async_chunk)
        async_idx = utils::tail(async_idx, 1)
        chunk_names[-async_idx] = ''
        
        async_chunk = chunk_names[async_idx]
        # try to obtain async_vars
        async_vars = stringr::str_match(async_chunk, 'async_vars=(.*)')[,2]
        if(is.na(async_vars)){
          async_vars = NULL
        }else{
          async_vars = unlist(stringr::str_split(async_vars, ','))
        }
        chunk_names[async_idx] = 'async'
      }else{
        chunk_names[] = ''
      }
      fixes = chunk_names
      fixes[-1] = '}; \n{'
      fixes[1] = '{'
      
      content[idx] = fixes
      content = c(content, '}')
    }else{
      content = c('{', content, '}')
    }
    
  }
  
  text = paste(content, collapse = '\n')
  expr = parse(text = text)
  if(evaluate){
    eval(expr, envir = env)
    return(TRUE)
  }else{
    
    attr(expr, 'chunk_names') = chunk_names
    attr(expr, 'auto') = auto
    attr(expr, 'async_vars') = async_vars
    
    return(expr)
  }
}

#' Prepare environments to parse the module inputs and outputs
#' 
#' Require context: `debug`
#' Inner context: `debug`
#' 
#' @param module_id module ID
#' @param context pre-assign contexts in tool_env and tmp_enc
#' 
#' @noRd
get_comp_env <- function(module_id, parse_context = c("rave_running", "rave_running_local")){
  
  parse_context = match.arg(parse_context)
  # requires at least package name to be present
  ctx = rave_context(disallowed_context = 'default')
  .__rave_module__. = module_id
  
  # Inside of the function, use context - rave_compile
  pkg_name = ctx$package
  
  path = get_path('inst', 'modules', module_id, 'comp.R')
  
  # module code - inputs and outputs
  content = readLines(path)
  
  # package environment
  pkg_env = asNamespace(pkg_name)
  
  # Shared environment when running in `rave_compile` context to temporary
  # persist values
  shared_env = new.env(parent = emptyenv())
  
  # Stores Input Output quos
  input_env = new.env(parent = emptyenv())
  output_env = new.env(parent = emptyenv())
  
  # Stores quo from define_initialization and other quos for rave_updates
  init_env = new.env(parent = emptyenv())
  init_env[['init']] = FALSE
  
  # Other scripts (quos) to be executed as add-ons to the module
  scripts = new.env(parent = emptyenv())
  
  # Environment with toolboxes
  tool_env = new.env(parent = pkg_env)
  load_rave_module_package(tool_env, parse_context = parse_context)
  
  # environment to run the code with context
  tmp_env = new.env(parent = tool_env)
  # rave_context(context = parse_context, spos = 1L, tenv = tmp_env)
  rave_context(context = parse_context, tenv = tmp_env)
  
  # Add references to make sure the environments can be accessed from context
  # `rave_compile`
  tool_env$...output_env = output_env
  tool_env$...input_env = input_env
  tool_env$...pkg_env = pkg_env
  tool_env$...init_env = init_env
  tool_env$...shared_env = shared_env
  tool_env$...scripts = scripts
  tool_env$...tmp_env = tmp_env
  
  return(list(
    content = content,
    input_env = input_env,
    output_env = output_env,
    init_env = init_env,
    script_env = scripts,
    tmp_env = tmp_env,
    shared_env = shared_env,
    tool_env = tool_env,
    pkg_env = pkg_env
  ))
}

compile_module_ui <- function(
  module_id, parse_context = c("rave_running", "rave_running_local")
){
  
  parse_context = match.arg(parse_context)
  # requires at least package name to be present
  rave_context(disallowed_context = 'default')
  .__rave_module__. = module_id
  
  envs = get_comp_env(module_id = module_id, parse_context = parse_context)
  
  get_content(content = envs$content, env = envs$tmp_env, evaluate = TRUE)
  
  envs
}


#' @title Parse and Compile Modules into \code{\link[rlang]{quosure}}
#' @description Function is only valid when developing modules or compiled by 
#' 'rave'. Calling directly without setting 'rave' context will result in 
#' failure.
#' @param module_id module ID
#' @param context compile context
#' 
#' Require context: `debug`, `compile`
#' Inner context: `debug`
#' 
#' @param module_id module ID
#' @param context pre-assign contexts in tool_env and tmp_enc
#' 
#' @noRd
parse_components <- function(module_id, parse_context = c("rave_running_local", "rave_running")){
  
  parse_context = match.arg(parse_context)
  # requires at least package name to be present
  ctx = rave_context(disallowed_context = 'default')
  .__rave_module__. = module_id
  
  envs = compile_module_ui(module_id = module_id, parse_context)
  
  tmp_env = envs$tmp_env
  input_env = envs$input_env
  output_env = envs$output_env
  shared_env = envs$shared_env
  
  # Find inputs
  input_layout = tmp_env$input_layout
  manual_inputs = c( tmp_env$manual_inputs, shared_env$manual_inputs )
  render_inputs = c( tmp_env$render_inputs, shared_env$render_inputs )
  inputs = as.list(input_env)
  defs = lapply(inputs, '[[', 'definition')
  names(defs) = NULL
  rave_inputs_quo = rlang::quo(
    rave_inputs(!!!defs, .input_panels = !!input_layout,
                .manual_inputs = !!manual_inputs,
                .render_inputs = !!render_inputs)
  )
  
  # Generate rave_updates
  init_expr = envs$init_env$init
  inits = lapply(inputs, '[[', 'initialization')
  names(inits) = names(inputs)
  
  init_anon_quos = lapply(init_expr, function(expr){rlang::quo(force(!!expr))})
  
  # rlang::quo(rave_updates({!!!init_anon_quos}))
  
  rave_update_quo = rlang::quo(rave_updates({!!!init_anon_quos}, !!!inits))
  # rave_update_quo = rlang::quo(rave_updates({eval(!!init_expr)}, !!!inits))
  
  # outputs
  output_layout = tmp_env[['output_layout']]
  comps = as.list(output_env)
  defs = lapply(comps, '[[', 'definition')
  if(length(defs)){
    titles = sapply(comps, '[[', 'title')
    names(defs) = titles
    order = order(sapply(comps, '[[', 'order'))
    defs = defs[order]
    
    # generate temp functions
    output_functions = lapply(comps, function(comp){
      rlang::quo(function(...){
        ._current_env = environment()
        ._env = new.env()
        ._env$get_value = function(key, ifNotFound = NULL){
          get0(key, envir = ._current_env, ifnotfound = ifNotFound)
        }
        
        ._env$get_variables <- function(level = 2, env = ._current_env){
          res <- names(env)
          if( level > 0 ){
            res <- c(res, ._env$get_variables(level - 1, env = parent.env(env)))
          } else {
            warning('results$get_variables is for debug use only')
          }
          unique(res)
        }
        
        ._env$async_value = function(key){
          ..param_env = get0('..param_env', envir = ._current_env)
          if(is.environment(..param_env)){
            async_var = get0('async_var', envir = ..param_env)
            if(is.function(async_var)){
              return(async_var(key))
            }
          }
          
          return(NULL)
        }
        do.call(!!comp$outputId, c(list(._env), list(...)))
      })
    })
    
    names(output_functions) = paste0('..', sapply(comps, '[[', 'outputId'))
    
  }else{
    output_functions = NULL
    # need to make sure at least one output
    defs = list('No Output' = quote(textOutput('do_nothing', width = 12L)))
  }
  
  if(is.null(output_layout)){
    rave_output_quo = rlang::quo(rave_outputs(!!!defs))
  }else{
    rave_output_quo = rlang::quo(rave_outputs(!!!defs, .output_tabsets = !!output_layout))
  }
  rave_output_quo
  
  return(list(
    rave_inputs_quo = rave_inputs_quo,
    rave_update_quo = rave_update_quo,
    rave_output_quo = rave_output_quo,
    output_functions = output_functions,
    script_env = envs$script_env,
    env = environment()
  ))
}

get_main_function <- function(module_id, parse_context = c("rave_running", "rave_running_local")){
  parse_context = match.arg(parse_context)
  # requires at least package name to be present
  rave_context(disallowed_context = 'default')
  .__rave_module__. = module_id
  
  path = get_path('inst', 'modules', module_id, 'main.R')
  content = readLines(path)
  
  expr = get_content(content = content, evaluate = FALSE, chunks = TRUE)
  main_quos = rlang::quos(!!! as.list(expr))
  names(main_quos) = attr(expr, 'chunk_names')
  
  main_quos$async_vars = attr(expr, 'async_vars')
  
  main_quos
}


.init_module <- function(module_id, debug = FALSE, parse_context = c("rave_running", "rave_running_local")){
  if(debug){
    parse_context = 'rave_running_local'
  }else{
    parse_context = match.arg(parse_context)
  }
  
  ctx = rave_context(disallowed_context = 'default')
  .__rave_module__. = module_id
  
  envs = compile_module_ui(module_id = module_id, parse_context = parse_context)
  param_env = new.env(parent = envs$tool_env)
  
  if(debug){
    list2env(as.list(envs$tool_env), envir = param_env)
  }
  # rave_context(context = parse_context, spos = 1L, tenv = param_env)
  rave_context(context = parse_context, tenv = param_env)
  
  mount_demo_subject()
  
  lapply(envs$script_env[['source']], function(f){
    if(rlang::is_quosure(f)){
      dipsaus::eval_dirty(f, env = param_env)
      # eval(rlang::quo_squash(f), envir = param_env)
    }else{
      source(file = f, local = param_env)
    }
    NULL
  })
  
  # initialize global variables
  init_expr = envs$init_env$init
  lapply(init_expr, function(expr){
    dipsaus::eval_dirty(expr, env = param_env)
  })
  # Initialize inputs
  inputs = as.list(envs$input_env)
  lapply(inputs, function(input){
    if(inherits(input, 'comp_input')){
      inputId = input$inputId
      def = input$definition
      f = def[[1]]
      args = formals(eval(f, envir = list(), enclos = param_env))
      value = dipsaus::eval_dirty(def[['value']], env = param_env)
      value %?<-% dipsaus::eval_dirty(def[['selected']], env = param_env)
      value %?<-% dipsaus::eval_dirty(args[['value']], env = param_env)
      value %?<-% dipsaus::eval_dirty(args[['selected']], env = param_env)
      
      init = input$initialization
      
      if(!is.null(init)){
        updates = dipsaus::eval_dirty(init, env = param_env)
        updated_value = updates[['value']]
        updated_value %?<-% updates[['selected']]
        if(!is.null(updated_value)){
          value = updated_value
        }
      }
      value = eval(value)
      param_env[[inputId]] = value
      
      catgl(inputId, '<- ', paste(utils::capture.output(cat2(value)), collapse = '\n'))
      
    }
  })
  
  if(debug){
    list2env(as.list(param_env), globalenv())
    
    return(invisible(param_env))
  }
  return(param_env)
}
  
#' @title Initialize 'RAVE' module for debug purpose
#' @param module_id module ID
#' @param debug whether to expose all functions to the global environment
#' @param parse_context parsing context, for internal use
#' @seealso \code{\link[rave]{load_rave_module_package}}
#' @export
init_module <- rave_context_generics('init_module', .init_module)
#' @export
init_module.rave_module_debug <- function(
  module_id, debug=TRUE, parse_context = 'rave_running_local'){
  
  .init_module(module_id, debug, parse_context)
}
#' @export
init_module.rave_running <- function(
  module_id, debug=FALSE, parse_context = 'rave_running_local'){
  
  .init_module(module_id, debug, parse_context)
}
#' @export
init_module.rave_running_local <- function(
  module_id, debug=FALSE, parse_context = 'rave_running_local'){
  
  .init_module(module_id, debug, parse_context)
}

