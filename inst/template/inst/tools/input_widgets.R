define_input_multiple_electrodes <- function(inputId, label = 'Electrodes'){
  quo = rlang::quo({
    define_input(
      definition = textInput(!!inputId, !!label, value = "", placeholder = '1-5,8,11-20'),
      init_args = c('label', 'value'),
      init_expr = {

        electrodes = preload_info$electrodes

        last_input = cache_input(!!inputId, val = as.character(electrodes[1]))
        e = dipsaus::parse_svec(last_input)
        e = e[e %in% electrodes]
        if(!length(e)){
          e = electrodes[1]
        }
        value = dipsaus::deparse_svec(e)
        label = paste0(!!label, ' (currently loaded: ' , dipsaus::deparse_svec(electrodes) , ')')
      }
    )
  })

  parent_frame = parent.frame()

  dipsaus::eval_dirty(quo, env = parent_frame)
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

  dipsaus::eval_dirty(quo, env = parent_frame)
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
      }
    )
  })

  parent_frame = parent.frame()

  dipsaus::eval_dirty(quo, env = parent_frame)
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

  dipsaus::eval_dirty(quo, env = parent_frame)
}

define_input_condition_groups_default <- function(inputId, label = 'Group', initial_groups = 1){
  quo = rlang::quo({
    
    define_input(
      definition = compoundInput(
        inputId = !!inputId, prefix= !!label, inital_ncomp = !!initial_groups, components = {
          textInput('group_name', 'Name', value = '', placeholder = 'Condition Name')
          selectInput('group_conditions', ' ', choices = '', multiple = TRUE)
        }),
      
      init_args = c('initialize', 'value'),
      
      init_expr = {
        cond = unique(preload_info$condition)
        
        initialize = list(
          group_conditions = list(
            choices = cond
          )
        )
        default_val = list(
          list(
            group_name = 'All Conditions',
            group_conditions = list(cond)
          )
        )
        value = cache_input(!!inputId, default_val)
        if( !length(value) || !is.list(value[[1]]) ||
            !length(value[[1]]$group_conditions) || !any(value[[1]]$group_conditions %in% cond)){
          value = default_val
        }
      }
    )
  })
  
  parent_frame = parent.frame()
  
  dipsaus::eval_dirty(quo, env = parent_frame)
  
}



define_input_auto_recalculate <- function(inputId, label, 
                                          type = c('checkbox', 'button'), 
                                          button_type = 'primary', 
                                          default_on = FALSE){
  type = match.arg(type)
  widget_id = paste0(inputId, '_', type)
  
  quo = rlang::quo({
    define_input(customizedUI(inputId = !!inputId))
    load_scripts(rlang::quo({
      assign(!!inputId, function(){
        if( !!type == 'checkbox' ){
          checkboxInput(ns(!!widget_id), label = !!label, value = !!default_on)
        }else{
          icon = NULL
          if(!!(!default_on)){
            icon = shiny::icon('arrow-right')
          }
          dipsaus::actionButtonStyled(
            ns(!!widget_id), !!label, width = '100%', type = !!button_type,
            icon = icon)
        }
      })
      
      register_auto_calculate_widget(!!widget_id, !!type, !!default_on)
      
    }))
  })
  parent_env = parent.frame()
  dipsaus::eval_dirty(quo, env = parent_env)
  
  
}







define_input_condition_groups <- function(
  inputId, label = 'Group', initial_groups = 1, max_group = 10, min_group = 1,
  label_color = rep('black', max_group), init_args, init_expr, quoted = FALSE, ...){
  
  if(missing(init_args)){
    init_args = c('initialization', 'value')
  }
  # dipsaus::registerCompoundInput2()
  
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
      if( !length(value) || !length(value[[1]]$group_conditions) || !any(value[[1]]$group_conditions %in% cond)){
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
      
      init_expr = eval(!!init_expr)
    )
  })
  
  parent_frame = parent.frame()
  
  dipsaus::eval_dirty(quo, env = parent_frame)
}

















