define_input_3d_viewer_generator <- function(
  inputId, label = 'Open viewer in a new tab', button_types = c('primary', 'default'), 
  download_label = 'Download', download_btn = TRUE,
  download_filename = 'rave_viewer.zip', 
  reactive = 'input'
){
  input_ui = paste0(inputId, '_ui')
  input_fun = paste0(inputId, '_fun')
  input_download = paste0(inputId, '_download')
  outputId = paste0(inputId, '_out')
  quo = rlang::quo({
    define_input( definition = actionButtonStyled(
      inputId = !!inputId, label = !!label, width = '100%', type = !!button_types[[1]]
    ) )
    define_input( definition = rave::customizedUI(!!input_ui) )
    # This is actually an output
    load_scripts(rlang::quo({
      
      assign(!!input_ui, function(){
        btn_class = c(!!button_types, '')[2]
        
        if( btn_class != '' ){
          btn_class = paste0('btn-', btn_class)
        }
        if( !!download_btn ){
          shiny::downloadButton(outputId = ns(!!input_download), label = !!download_label, 
                                style='width:100%', class = btn_class)
        }else{
          shiny::downloadLink(outputId = ns(!!input_download), label = !!download_label, 
                              style='width:100%')
        }
        
      })
      
      local({
        input %?<-% getDefaultReactiveInput()
        if( !!reactive == 'input' ){
          react = input
        }else{
          react = get0(!!reactive, ifnotfound = shiny::reactiveValues())
        }
        ...widget_env = new.env(parent = emptyenv())
        
        # Generate 3D viewer render function
        ...fun = function(){
          re = NULL
          f = get0(!!input_fun, envir = ..runtime_env, ifnotfound = function(...){
            cat2('3D Viewer', !!outputId,  'cannot find function', !!input_fun, level = 'INFO')
          })
          tryCatch({
            re = f()
          }, error = function(e){
            dipsaus::cat2(e, level = 'ERROR')
          })
          re
        }
        
        render_func = function(){
          threeBrain::renderBrain({
            re = NULL
            # Render function
            if(length( react[[!!inputId]] )){
              re = ...fun()
            }
            
            if('R6' %in% class(re)){
              re = re$plot()
            }
            ...widget_env$widget = re
            re
          })
        }
        
        output %?<-% getDefaultReactiveOutput()
        
        output[[!!input_download]] = shiny::downloadHandler(
          filename = !!download_filename, content = function(con){
            if( !length(...widget_env$widget) ){
              re = ...fun()
            }else{
              re = ...widget_env$widget
            }
            showNotification(p('Generating 3D viewer. Please wait...'), duration = NULL,
                             type = 'default', id = '...save_brain_widget')
            tmp_dir = tempdir()
            finfo = threeBrain::save_brain(re, directory = tmp_dir, 
                                           title = 'RAVE Viewer', as_zip = TRUE)
            on.exit({ unlink( finfo$zipfile ) })
            
            showNotification(p('Done!'), type = 'message', id = '...save_brain_widget')
            
            file.copy(finfo$zipfile, to = con, overwrite = TRUE, recursive = FALSE)
          }
        )
        
        
        # Register render function
        
        # 1. main viewer (if exists)
        output[[!!outputId]] <- render_func()
        
        # 2. side viewers
        # Register cross-session function so that other sessions can register the same output widget
        session$userData$cross_session_funcs %?<-% list()
        # ns must be defined, but in get_module(..., local=T) will raise error
        # because we are not in shiny environment
        ns %?<-% function(x){x} 
        session$userData$cross_session_funcs[[ns(!!outputId)]] = render_func
        
        
        observeEvent(input[[!!inputId]], {
          
          rave_id = session$userData$rave_id
          if(is.null(rave_id)){ rave_id = '' }
          token = session$userData$token
          if(is.null(token)){ token = '' }
          globalId = ns(!!outputId)
          
          query_str = list(
            type = '3dviewer',
            globalId = htmltools::urlEncodePath(globalId),
            sessionId = htmltools::urlEncodePath(rave_id),
            token = token
          )
          url = paste(sprintf('%s=%s', names(query_str), as.vector(query_str)), collapse = '&')
          
          shinyjs::runjs(sprintf('window.open("/?%s");', url))
        })
      })
      
    }))
  })
  
  parent_frame = parent.frame()
  
  eval_dirty(quo, env = parent_frame)
}

define_input_multiple_electrodes <- function(inputId, label = 'Electrodes'){
  quo = rlang::quo({
    define_input(
      definition = textInput(!!inputId, !!label, value = "", placeholder = '1-5,8,11-20'),
      init_args = c('label', 'value'),
      init_expr = {
        
        electrodes = preload_info$electrodes
        
        last_input = cache_input(!!inputId, val = as.character(electrodes)[1])
        e = dipsaus::parse_svec(last_input)
        e = e[e %in% electrodes]
        if(!length(e)){
          e = electrodes[1]
        }
        value = dipsaus::deparse_svec(e)
        label = paste0(!!label, ' (currently loaded: ', dipsaus::deparse_svec(electrodes), ')')
      }
    )
  })
  
  parent_frame = parent.frame()
  eval_dirty(quo, env = parent_frame)
}


define_input_single_electrode <- function(inputId, label = 'Electrode'){
  quo = rlang::quo({
    define_input(
      definition = selectInput(!!inputId, !!label, choices = '', selected = NULL, multiple = FALSE),
      init_args = c('choices', 'selected'),
      init_expr = {
        electrodes = preload_info$electrodes
        choices = as.character(electrodes)
        
        selected = cache_input(!!inputId, val = electrodes[1])
        selected = as.character(selected)
        
        if(length(selected) != 1 || !selected %in% choices){
          selected = choices[1]
        }
      }
    )
  })
  
  parent_frame = parent.frame()
  
  eval_dirty(quo, env = parent_frame)
}



define_input_frequency <- function(inputId, label = 'Frequency', is_range = TRUE, round = -1, initial_value = NULL){
  
  if(is_range){
    v = c(1,200)
  }else{
    v = 1
  }
  
  quo = rlang::quo({
    define_input(
      definition = sliderInput(!!inputId, !!label, min = 1, max = 200, value = !!v, round = !!round),
      init_args = c('min', 'max', 'value'),
      init_expr = {
        freq_range = range(preload_info$frequencies)
        min = floor(freq_range[1])
        max = ceiling(freq_range[2])
        initial_value = !!initial_value
        if(!!is_range){
          initial_value %?<-% c(min, max)
        }else{
          initial_value %?<-% min
        }
        
        value = cache_input(!!inputId, initial_value)
        if(length(value) == 1) {
          # the problem here is that it doesn't work for ranges...
          value = ..get_nearest_val(value, preload_info$frequencies)
        } else {
          v1 <- ..get_nearest_val(value[1], preload_info$frequencies)
          v2 <- ..get_nearest_val(value[2], preload_info$frequencies)
          value = c(v1,v2)
        }
      }
    )
  })
  
  parent_frame = parent.frame()
  
  eval_dirty(quo, env = parent_frame)
}

define_srate_input_slider <- function(inputId, label) {
  quo = rlang::quo({
    define_input(
      definition = sliderInput(!!inputId, !!label, min = 1, max = 100, value = 100, step = 1, round = 1),
      init_args = c('min', 'max', 'value'),
      init_expr = {
        min =1
        initial_value = module_tools$get_sample_rate(original = FALSE)
        max = min(initial_value * 2, module_tools$get_sample_rate(original = TRUE))
        value = cache_input(!!inputId, initial_value)
      }
    )
  })
  parent_frame = parent.frame()
  
  eval_dirty(quo, env = parent_frame)
}


define_input_time <- function(inputId, label = 'Time Range', is_range = TRUE, round = -2, initial_value = NULL){
  if(is_range){
    v = c(0,1)
  }else{
    v = 0
  }
  
  quo = rlang::quo({
    
    define_input(
      definition = sliderInput(!!inputId, !!label, min = 0, max = 1, value = !!v, step = 0.01, round = !!round),
      init_args = c('min', 'max', 'value'),
      init_expr = {
        time_range = range(preload_info$time_points)
        
        min = min(time_range[1])
        max = max(time_range[2])
        initial_value = !!initial_value
        
        if(!!is_range){
          initial_value %?<-% c(min, max)
        }else{
          initial_value %?<-% min
        }
        value = cache_input(!!inputId, initial_value)
      }
    )
  })
  
  parent_frame = parent.frame()
  
  eval_dirty(quo, env = parent_frame)
}

# define_input_condition_groups <- function(
#   inputId, label = 'Group', initial_groups = 1, 
#   init_args, init_expr, quoted = FALSE, ...){
#   
#   if(missing(init_args)){
#     init_args = c('initialize', 'value')
#   }
#   
#   if(missing(init_expr)){
#     init_expr = rlang::quo({
#       cond = unique(preload_info$condition)
#       
#       initialize = list(
#         group_conditions = list(
#           choices = cond
#         )
#       )
#       default_val = list(
#         list(
#           group_name = 'All Conditions',
#           group_conditions = list(cond)
#         )
#       )
#       value = cache_input(!!inputId, default_val)
#       if( !length(value) || !length(value[[1]]$group_conditions) || !any(value[[1]]$group_conditions %in% cond)){
#         value = default_val
#       }
#     })
#   }else if (!quoted){
#     init_expr = substitute(init_expr)
#   }
#   
#   quo = rlang::quo({
# 
#     define_input(
#       definition = compoundInput(
#         inputId = !!inputId, prefix= !!label, inital_ncomp = !!initial_groups, components = {
#           textInput('group_name', 'Name', value = '', placeholder = 'Condition Name')
#           selectInput('group_conditions', ' ', choices = '', multiple = TRUE)
#         }),
# 
#       init_args = !!init_args,
# 
#       init_expr = {
#         eval(!!init_expr)
#       }
#     )
#   })
# 
#   parent_frame = parent.frame()
# 
#   eval_dirty(quo, env = parent_frame)
# 
# }

define_input_condition_groups <- function(
  inputId, label = 'Group', initial_groups = 1, max_group = 10, min_group = 1,
  label_color = rep('black', max_group), init_args, init_expr, quoted = FALSE, ...
){
  # get_from_package('registerCompoundInput2', 'dipsaus', internal = TRUE)()
  if(missing(init_args)){
    init_args = c('initialization', 'value')
  }
  
  if(missing(init_expr)){
    init_expr = rlang::quo({
      cond = unique(preload_info$condition)
      
      initialization = list(
        group_conditions = list(
          choices = cond
        )
      )
      default_val = list(
        list(
          group_name = 'All Conditions',
          group_conditions = cond
        )
      )
      value = cache_input(!!inputId, default_val)
      # print('asdasdasd')
      # print(value)
      if( !length(value) || 
          !length(value[[1]]$group_conditions) || 
          !any(value[[1]]$group_conditions %in% cond)){
        value = default_val
      }
    })
  }else if (!quoted){
    init_expr = substitute(init_expr)
  }
  
  quo = rlang::quo({
    
    define_input(
      definition = dipsaus::compoundInput2(
        inputId = !!inputId, label = !!label, inital_ncomp = !!initial_groups, 
        components = htmltools::div(
          textInput('group_name', 'Name', value = '', placeholder = 'Condition Name'),
          selectInput('group_conditions', ' ', choices = '', multiple = TRUE)
        ),
        label_color = !!label_color, max_ncomp = !!max_group, min_group = !!min_group
      ),
      
      init_args = !!init_args,
      
      init_expr = !!init_expr
    )
  })
  
  parent_frame = parent.frame()
  
  eval_dirty(quo, env = parent_frame)
  
}

define_input_analysis_data_csv <- function(
  inputId, label, paths, reactive_target = sprintf('local_data[[%s]]', inputId),
  file_match_string = '\\.(csv|fst)$',
  multiple = TRUE, label_uploader = '...', try_load_yaml = TRUE, allow_uploader = FALSE){
  
  input_ui = inputId
  input_selector = paste0(inputId, '_source_files')
  input_uploader = paste0(inputId, '_uploader')
  input_btn = paste0(inputId, '_btn')
  input_evt = paste0(inputId, '__register_events')
  reactive_target = substitute(reactive_target)
  
  quo = rlang::quo({
    define_input(definition = customizedUI(!!input_ui))
    
    load_scripts(rlang::quo({
      ...ravemodule_environment_reserved %?<-% new.env(parent = emptyenv())
      ...ravemodule_environment_reserved[[!!input_ui]] = new.env(parent = emptyenv())
      
      assign(!!input_ui, function(){
        project_dir = dirname(subject$dirs$subject_dir)
        search_paths = file.path(project_dir, !!paths)
        choices = unlist(lapply(search_paths, list.files, pattern = !!file_match_string))
        # Order file names by date-time (descending order)
        dt = stringr::str_extract(choices, '[0-9]{8}-[0-9]{6}')
        od = order(strptime(dt, '%Y%m%d-%H%M%S'), decreasing = TRUE)
        choices = choices[od]
        
        uploader_tag = NULL
        if(!!allow_uploader){
          uploader_tag = htmltools::div(
            style = 'flex-basis: 50%; min-height: 80px;',
            htmltools::tags$label("Upload new files to this project's RAVE directory"),
            fileInput(inputId = ns(!!input_uploader), label = !!label_uploader, multiple = FALSE, width = '100%')
          )
        }
        
        # function to render UI
        htmltools::div(
          class = 'rave-grid-inputs',
          htmltools::div(
            style = 'flex-basis: 100%; min-height: 80px;',
            selectInput(inputId = ns(!!input_selector), label = !!label, choices = choices, selected = choices[1], multiple = !!multiple)
          ),
          htmltools::div(
            style = 'flex-basis: 100%',
            actionButtonStyled(inputId = ns(!!input_btn), label = 'Load selected data', width = '100%', type = 'primary')
          ),
          uploader_tag
        )
      })
      ...ravemodule_environment_reserved[[!!input_ui]][[!!input_evt]] = function(){
        .env = environment()
        .local_data = reactiveValues()
        
        is_reactive_context = function(){
          session = getDefaultReactiveDomain()
          any(c('ShinySession', 'session_proxy') %in% class(session))
        }
        
        # 1. function to scan source files
        rescan_source = function(search_paths, update = TRUE, new_selected = NULL){
          if(!length(search_paths)){
            return(NULL)
          }
          choices = unlist(lapply(search_paths, list.files, pattern = !!file_match_string))
          # Order file names by date-time (descending order)
          dt = stringr::str_extract(choices, '[0-9]{8}-[0-9]{6}')
          od = order(strptime(dt, '%Y%m%d-%H%M%S'), decreasing = TRUE)
          choices = choices[od]
          
          if(update && is_reactive_context()){
            session = getDefaultReactiveDomain()
            selected = c(shiny::isolate(input[[!!input_selector]]), new_selected)
            updateSelectInput(session, inputId = !!input_selector, choices = choices, selected = selected)
          }
          return(choices)
        }
        
        # 2. Find csv file within directory
        find_source = function(search_paths, fname){
          fpaths = file.path(search_paths, fname)
          fexists = file.exists(fpaths)
          if(!any(fexists)){ return(NULL) }
          return(fpaths[which(fexists)[1]])
        }
        # 3. Monitor subject change
        local_reactives = get_execenv_local_reactive()
        observe({
          if(monitor_subject_change()){
            project_dir = dirname(subject$dirs$subject_dir)
            .local_data$search_paths = search_paths = file.path(project_dir, !!paths)
            .local_data$group_analysis_src = search_paths[[1]]
            # Do not change it here because renderui will override update inputs
            # rescan_source(search_paths, update = TRUE)
          }
        }, env = .env, priority = -1)
        
        
        # 3 listen to event to upload file
        observeEvent(input[[!!input_uploader]], {
          csv_headers = c('Project', 'Subject', 'Electrode')
          path = input[[!!input_uploader]]$datapath
          group_analysis_src = .local_data$group_analysis_src
          tryCatch({
            print('Observe input_uploader')
            # try to load as csv, check column names
            if(endsWith(path, 'csv')) {
              dat = read.csv(path, header = TRUE, nrows = 3)
            } else if (endsWith(path, 'fst')) {
              dat = fst::read_fst(path, from = 1, 3)
            } else {
              stop('unable to parse file type: ', path)
            }
            
            if(all(csv_headers %in% names(dat))){
              now = strftime(Sys.time(), '-%Y%m%d-%H%M%S(manual).csv')
              # pass, write to group_analysis_src with name
              fname = input[[!!input_uploader]]$name
              fname = stringr::str_replace_all(fname, '[\\W]+', '_')
              fname = stringr::str_to_lower(fname)
              fname = stringr::str_replace(fname, '_csv$', now)
              if(!dir.exists(group_analysis_src)){
                dir.create(group_analysis_src, recursive = TRUE, showWarnings = FALSE)
              }
              file.copy(path, file.path(group_analysis_src, fname), overwrite = TRUE)
              rescan_source(.local_data$search_paths, new_selected = fname)
              return()
            }
            showNotification(p('The file uploaded does not have enough columns.'), type = 'error')
          }, error = function(e){
            showNotification(p('Upload error: ', e), type = 'error')
          })
        }, event.env = .env, handler.env = .env)
        
        observeEvent(input[[!!input_btn]], {
          # print('Observe input_btn')
          source_files = input[[!!input_selector]]
          search_paths = .local_data$search_paths
          progress = rave::progress('Loading data files', max = length(source_files) + 1)
          on.exit({ progress$close() })
          
          progress$inc('Checking files...')
          # find all the source files and get headers
          metas = lapply(source_files, function(fpath){
            fpath = find_source(search_paths, fpath)
            if( is.null(fpath) ){ return(NULL) }
            if(endsWith(fpath, 'csv')) {
              dat = read.csv( fpath , header = TRUE, nrows = 1)
            } else {
              dat = fst::read_fst(fpath, from=1, to=2)
            }
            list(
              fpath = fpath,
              header = names(dat)
            )
          })
          metas = dipsaus::drop_nulls(metas)
          headers = unique(unlist(lapply(metas, '[[', 'header')))
          
          # Read all data
          project_name = subject$project_name
          tbls = dipsaus::drop_nulls(lapply(metas, function(x){
            # print('trying to load ' %&% x$fpath)
            progress$inc(rutabaga::get_filename(x$fpath))
            if(endsWith(x$fpath, 'csv')) {
              tbl = data.table::fread(file = x$fpath, stringsAsFactors = FALSE, header = TRUE)
            } else {
              tbl = fst::read_fst(x$fpath)
            }
            tbl = tbl[tbl$Project %in% project_name, ]
            if(!nrow(tbl)){
              return(NULL)
            }
            mish = headers[!headers %in% names(tbl)]
            for(m in mish){
              tbl[[m]] = NA
            }
            
            # Load YAML files
            conf = NULL
            if( !!try_load_yaml ){
              yaml_path = paste0(x$fpath, '.yaml')
              if(file.exists(yaml_path)){
                conf = yaml::read_yaml(yaml_path)
              }
            }
            # print('returning loaded data ')
            return(list(
              data = tbl,
              conf = conf,
              path = x$fpath,
              subject = tbl$Subject[[1]]
            ))
          }))
          
          res = do.call('rbind', lapply(tbls, '[[', 'data'))
          
          if(!is.data.frame(res) || !nrow(res)){
            res = NULL
          }else{
            try({
              res$Electrode = as.character(res$Electrode)
              res$Subject = as.character(res$Subject)
              res$Condition = as.character(res$Condition)
            }, silent = TRUE)
            
            subjects = sapply(tbls, '[[', 'subject')
            confs = lapply(tbls, '[[', 'conf')
            names(confs) = subjects
            
            res = list(
              data = res,
              subjects = subjects,
              confs = confs,
              headers = names(res)
            )
            
          }
          if(is.character(!!reactive_target)){
            eval(parse(text = sprintf('%s <- res', !!reactive_target)))
          }else{
            do.call('<-', list(!!reactive_target, res))
          }
          
        }, event.env = .env, handler.env = .env)
      }
      
      eval_when_ready(function(...){
        ...ravemodule_environment_reserved[[!!input_ui]][[!!input_evt]]()
      })
    }))
  })
  
  parent_frame = parent.frame()
  eval_dirty(quo, env = parent_frame)
}


define_input_table_filters <- function(
  inputId, label = 'Filter', 
  watch_target = 'local_data[["analysis_data"]]', 
  reactive_target = 'local_data[["analysis_data_filtered"]]',
  table_not_present = p('Analysis table not loaded')
){
  input_ui = inputId
  watch_target = substitute(watch_target)
  reactive_target = substitute(reactive_target)
  input_add = paste0(inputId, '_add_btn')
  input_remove = paste0(inputId, '_remove_btn')
  input_preview = paste0(inputId, '_preview_btn')
  input_preview_table = paste0(inputId, '_preview_btn_table')
  input_filter_prefix = paste0(inputId, '_filter')
  
  quo = rlang::quo({
    define_input(definition = customizedUI(!!input_ui))
    
    load_scripts(rlang::quo({
      input %?<-% getDefaultReactiveInput()
      ...ravemodule_environment_reserved %?<-% new.env(parent = emptyenv())
      ...ravemodule_environment_reserved[[!!input_ui]] = new.env(parent = emptyenv())
      ...ravemodule_environment_reserved[[!!input_ui]]$local_filters = reactiveValues(
        filter_count = 0,
        filter_observers = 0
      )
      
      # Function to generate UI for iith filter
      ...ravemodule_environment_reserved[[!!input_ui]]$get_ui  = function(ii, vars = ''){
        filter = shiny::isolate(...ravemodule_environment_reserved[[!!input_ui]]$local_filters[[paste0('filter', ii)]])
        if(!is.list(filter)){ filter = list() }
        tagList(
          tagList(
            tags$label(sprintf('%s %d', !!label, ii), style = ifelse(ii == 1, '', 'margin-top: 15px;')),
            div(
              # To make a box to wrap group inputs
              class = 'rave-grid-inputs',
              div(
                style = 'flex-basis: 33%; min-height: 80px;',
                selectInput(ns(sprintf('%s_var_', !!input_filter_prefix, ii)), 'Variable', choices = vars, selected = get_val(filter, 'var', default = NULL))
              ),
              div(
                style = 'flex-basis: 33%; min-height: 80px;',
                selectInput(ns(sprintf('%s_op_', !!input_filter_prefix, ii)), 'Operator', choices = c('=', '!=', '>', '>=', '<', '<=', 'in', 'not in', 'between'), selected = get_val(filter, 'op', default = '='))
              ),
              div(
                style = 'flex-basis: 33%; min-height: 80px;',
                textInput(ns(sprintf('%s_val_', !!input_filter_prefix, ii)), 'Value', value = get_val(filter, 'val', default = NULL))
              ),
              div(
                style = 'flex-basis: 100%;',
                uiOutput(ns(sprintf('%s_msg_', !!input_filter_prefix, ii)))
              )
            )
            
          )
        )
      }
      
      # Given string like '=' return expression
      ...ravemodule_environment_reserved[[!!input_ui]]$get_operator = function(op){
        switch (op,
                '=' = '%s == %s',
                'in' = '%s %%in%% %s',
                'between' = '%s %%within%% %s',
                'not in' = '!%s %%in%% %s',
                {
                  paste('%s', op, '%s')
                }
        )
      }
      
      # Given data, operator and criteria, return logical filters
      ...ravemodule_environment_reserved[[!!input_ui]]$filter_data = function(dat, op, val){
        tryCatch({
          if( is.numeric(dat) && is.character(val) ){
            if( op %in% c('in', 'not in', 'between') ){
              val = as.numeric(stringr::str_split(val, '[^0-9-.]+')[[1]])
            }else{
              val = as.numeric(val)
            }
          }
          expr = ...ravemodule_environment_reserved[[!!input_ui]]$get_operator(op)
          expr = sprintf(expr, 'dat', deparse(val))
          sel = rlang::eval_tidy(rlang::parse_expr(expr), data = list(dat = dat))
          sel
        }, error = function(e){
          NULL
        })
      }
      
      ...ravemodule_environment_reserved[[!!input_ui]]$get_filter_results = function(ii){
        ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$update
        filter = ...ravemodule_environment_reserved[[!!input_ui]]$local_filters[[paste0('filter', ii)]]
        
        if(!is.data.frame(...ravemodule_environment_reserved[[!!input_ui]]$data) || !is.list(filter) || !isFALSE(filter$failed)){ return(NULL) }
        var = filter$var; op = filter$op; val = filter$val
        dat = ...ravemodule_environment_reserved[[!!input_ui]]$data[[var]]
        if( var == 'Electrode' ){
          dat = as.numeric(dat)
        }
        if(is.numeric(dat)){
          if( op %in% c('in', 'not in', 'between') ){
            val = as.numeric(stringr::str_split(val, '[^0-9-.]+')[[1]])
          }else{
            val = as.numeric(val)
          }
        }else{
          if( op %in% c('in', 'not in') ){
            val = stringr::str_split(val, ',[ ]{0,1}')[[1]]
          }
        }
        sel = ...ravemodule_environment_reserved[[!!input_ui]]$filter_data(dat, op, val)
        if(is.null(sel)){ return(NULL) }
        sel[is.na(sel)] = FALSE
        sel
      }
      ...ravemodule_environment_reserved[[!!input_ui]]$add_filter_observer = function(ii){
        .env = environment()
        observe({
          ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$update
          n_filters = ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_count
          if(!is.data.frame(...ravemodule_environment_reserved[[!!input_ui]]$data) || !length(n_filters) || n_filters < ii ){ return(NULL) }
          all_vars = names(...ravemodule_environment_reserved[[!!input_ui]]$data)
          var = input[[sprintf('%s_var_', !!input_filter_prefix, ii)]]; op = input[[sprintf('%s_op_', !!input_filter_prefix, ii)]]; val = input[[sprintf('%s_val_', !!input_filter_prefix, ii)]]
          var %?<-% ''; op %?<-% '='; val %?<-% ''
          val_txt = val
          # Do checks
          msg = ''
          failed = FALSE
          if( !var %in% all_vars ){
            msg = 'Variable not found'
            failed = TRUE
          }else{
            dat = ...ravemodule_environment_reserved[[!!input_ui]]$data[[var]]
            if( is.numeric(dat) ){
              if( op %in% c('in', 'not in', 'between') ){
                val = as.numeric(stringr::str_split(val, '[^0-9-.]+')[[1]])
              }else{
                val = as.numeric(val)
              }
              if( !length(val) || any(is.na(val)) ){
                msg = 'Value is blank or invalid'
                failed = TRUE
              }
            }else{
              if( op %in% c('in', 'not in') ){
                val = stringr::str_split(val, ',[ ]{0,1}')[[1]]
              }
            }
            if( !failed ){
              sel = ...ravemodule_environment_reserved[[!!input_ui]]$filter_data(dat, op, val)
              if( is.null(sel) ){
                msg = 'Filter has error, will be ignored'
                failed = TRUE
              }else{
                n_na = sum(is.na(dat[sel]))
                n_sel = sum(sel, na.rm = TRUE)
                msg = sprintf('%d of %d selected (%d NAs)', n_sel, length(sel), n_na)
                if(n_sel == 0){
                  msg = 'No data selected'
                  failed = TRUE
                }
              }
            }
          }
          
          re = list(
            var = var, op = op, val = val_txt, failed = failed, msg = msg
          )
          ...ravemodule_environment_reserved[[!!input_ui]]$local_filters[[paste0('filter', ii)]] = re
          ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_has_update = Sys.time()
        }, env = .env)
        
        output[[sprintf('%s_msg_', !!input_filter_prefix, ii)]] = shiny::renderUI({
          ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$update
          n_filters = shiny::isolate(...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_count)
          
          if(!is.data.frame(...ravemodule_environment_reserved[[!!input_ui]]$data) || !length(n_filters) || n_filters < ii ){ return(NULL) }
          
          filter = ...ravemodule_environment_reserved[[!!input_ui]]$local_filters[[paste0('filter', ii)]]
          if(!is.list(filter)){ return() }
          
          col = ifelse( isTRUE(filter$failed) , 'red', 'grey' )
          filter$msg %?<-% ''
          htmltools::span(style = col2hex(col, prefix = 'color:#'), filter$msg)
        })
        
      }
      
      
      # Add/remove filters
      observeEvent(input[[!!input_add]], {
        n_filters = shiny::isolate(...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_count) + 1
        n_observers = shiny::isolate(...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_observers)
        ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_count = n_filters
        # Check if observers are needed
        if( n_filters > n_observers ){
          ...ravemodule_environment_reserved[[!!input_ui]]$add_filter_observer( n_filters )
          ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_observers = n_filters
        }
      })
      observeEvent(input[[!!input_remove]], {
        n_filters = shiny::isolate(...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_count) - 1
        ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_count = max(n_filters, 0)
      })
      
      # summarise filters
      ...ravemodule_environment_reserved[[!!input_ui]]$filter_summary = function(){
        n_filters = ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_count
        nrows = 0
        if(is.data.frame(...ravemodule_environment_reserved[[!!input_ui]]$data)){
          nrows = nrow(...ravemodule_environment_reserved[[!!input_ui]]$data)
        }
        if(nrows == 0){
          return(logical(0))
        }
        filters = rep(TRUE, nrows)
        for(ii in seq_len(n_filters)){
          fil = ...ravemodule_environment_reserved[[!!input_ui]]$get_filter_results( ii )
          if(length(fil)){
            filters = filters & fil
          }
        }
        filters
      }
      
      
      # UI renderer
      assign(!!input_ui, function(){
        
        if(is.character(!!watch_target)){
          eval(parse(text = sprintf('dat <- %s', !!watch_target)))
        }else{
          do.call('<-', list(quote(dat), !!watch_target))
        }
        ...ravemodule_environment_reserved[[!!input_ui]]$data = dat
        ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$update = Sys.time()
        if(!is.data.frame(dat)){
          return(span(style = 'color: #a1a1a1', !!table_not_present))
        }
        n_filters = ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_count
        vars = names(dat); vars %?<-% ''
        
        filter_uis = NULL
        minus_btn = NULL
        
        if(n_filters > 0){
          minus_btn = actionButton(ns(!!input_remove), shiny::icon('minus'))
          filter_uis = lapply( seq_len(n_filters), function(ii){ ...ravemodule_environment_reserved[[!!input_ui]]$get_ui( ii , vars ) } )
        }
        
        tagList(
          filter_uis,
          div(
            # Put a div to make buttons within a row
            actionButton(ns(!!input_add), shiny::icon('plus')),
            minus_btn
          ),
          actionLink(ns(!!input_preview), 'Preview filtered data')
        )
      })
      
      
      # Misc:
      
      # preview data table
      observeEvent(input[[!!input_preview]], {
        # Collect data
        shiny::showModal(shiny::modalDialog(
          title = 'Preview filtered data', size = 'l', easyClose = TRUE, fade = FALSE,
          tags$style('.modal-lg { min-width: 80vw; }'),
          DT::dataTableOutput(ns(!!input_preview_table))
        ))
      })
      output[[!!input_preview_table]] <- DT::renderDataTable({
        ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$data_filtered
      })
      
      observe({
        ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$update
        ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$filter_has_update
        if( !is.data.frame(...ravemodule_environment_reserved[[!!input_ui]]$data) ){
          res = NULL
        }else{
          sel = ...ravemodule_environment_reserved[[!!input_ui]]$filter_summary()
          res = ...ravemodule_environment_reserved[[!!input_ui]]$data[sel,]
        }
        ...ravemodule_environment_reserved[[!!input_ui]]$local_filters$data_filtered = res
        if(is.character(!!reactive_target)){
          eval(parse(text = sprintf('%s <- res', !!reactive_target)))
        }else{
          do.call('<-', list(!!reactive_target, res))
        }
      })
      
      
      
    }))
  })
  
  parent_frame = parent.frame()
  eval_dirty(quo, env = parent_frame)
}


# options to save and load analysis parameters
define_input_analysis_yaml_chooser <- function(
  inputId, name_prefix = 'settings_', 
  # Relative to project directory
  read_path, write_path = read_path,
  labels = c('Save settings', 'Load settings')
){
  save_btn = paste0(inputId, '_save')
  load_btn = paste0(inputId, '_load')
  save_text = paste0(inputId, '_savename')
  do_save = paste0(inputId, '_do_save')
  quo = rlang::quo({
    define_input(customizedUI(inputId = !!inputId))
    load_scripts(rlang::quo({
      assign(!!inputId, function(){
        
        defaultPath = do.call(file.path, as.list(c(subject$project_name, '_project_data', !!read_path)))
        dir.create(file.path(subject$dirs$data_dir, defaultPath), showWarnings = FALSE, recursive = TRUE)
        defaultPath = normalizePath(defaultPath)
        shinyFiles::shinyFileChoose(
          input = input,
          id = !!load_btn, roots= c('RAVE Home' = normalizePath(subject$dirs$data_dir), 'root' = '/'),
          filetypes = c('yaml', 'yml'), defaultRoot = 'RAVE Home',
          defaultPath = defaultPath
        )
        
        div(
          class = 'rave-grid-inputs', style='border:none',
          div(
            style = 'flex-basis:50%',
            actionButtonStyled(inputId = ns(!!save_btn),
                               label=!!labels[[1]], icon = shiny::icon('save'), width = '100%')
          ),
          div(
            style = 'flex-basis:50%',
            shinyFiles::shinyFilesButton(id = ns(!!load_btn), label = !!labels[[2]], title = 'Select Analysis Settings',
                                         multiple = FALSE, icon = shiny::icon('puzzle-piece'), style = 'width:100%')
          )
        )
      })
      
      # redirect shiny server file chooser home directory
      eval_when_ready(function(.env, ...){
        
        with(.env, {
          input %?<-% getDefaultReactiveInput()
          shiny_is_running <- function() {
            cls <- class(getDefaultReactiveDomain())
            any(cls %in% c('ShinySession', 'session_proxy'))
          }
          save_inputs <- function(yaml_path, variables_to_export){
            if( !shiny_is_running() || !exists('getDefaultReactiveInput') ){ return(FALSE) }
            
            input <- getDefaultReactiveInput()
            cache_list = shiny::isolate(shiny::reactiveValuesToList(input))
            if(!missing(variables_to_export)) {
              cache_list =cache_list[variables_to_export]
            }
            # if( exists('local_data') && shiny::is.reactivevalues(local_data) ){
            #   local_dat = shiny::isolate(shiny::reactiveValuesToList(local_data))
            #   cl = names(cache_list); cl = cl[cl %in% names(local_dat)]
            #   cache_list[cl] = local_dat[cl]
            # }
            yaml::write_yaml(x = cache_list, fileEncoding = 'utf-8', file = yaml_path)
            return(TRUE)
          }
          
          # save Modal pop up
          observeEvent(input[[!!save_btn]], {
            tstmp <- strftime(Sys.time(), format = '%Y-%h-%d')
            
            shiny::showModal(shiny::modalDialog(
              title = 'Save Analysis Settings',
              size = 's',
              easyClose = TRUE,
              textInput(ns(!!save_text), label = 'Settings Name', value = paste0(!!name_prefix, tstmp)),
              tags$small('Will overwrite settings with the same name currently in RAVE settings folder'),
              footer = tagList(
                actionButtonStyled(ns(!!do_save), 'Save'),
                shiny::modalButton("Cancel")
              )
            ))
          })
          
          # Modal do save
          observeEvent(input[[!!do_save]], {
            # save
            fname = input[[!!save_text]]
            fname = stringr::str_replace_all(fname, '[^a-zA-Z0-9]+', '_')
            fname = paste0(fname, '.yaml')
            save_dir = do.call(file.path, as.list(c(normalizePath(subject$dirs$subject_dir, mustWork = TRUE), '..', '_project_data', !!write_path)))
            print(save_dir)
            dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
            save_inputs(file.path(save_dir, fname))
            shiny::removeModal()
          })
          
        })
        
      })
    }))
    
  })
  
  parent_env = parent.frame()
  eval_dirty(quo, env = parent_env)
}
