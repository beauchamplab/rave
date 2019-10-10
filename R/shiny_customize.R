#' Re-write shiny actionButton to enable styles
#' @param inputId,label,icon,width see \code{shiny::actionButton}
#' @param type default, primary, info... see bootstrap
#' @param btn_type html tag attribute "type"
#' @param class additional classes
#' @param ... other methods passed to actionButton
#' @export
actionButtonStyled <- function(inputId, label, icon = NULL, width = NULL, type = 'default', btn_type = 'button', class = '', ...){
  value <- shiny::restoreInput(id = inputId, default = NULL)

  args = list(...)
  style = args[['style']]
  width %?<-% 'auto'
  style %?<-% ''
  style = paste0("width: ", validateCssUnit(width), ";", style)

  args[['style']] = style
  args[['id']] = inputId
  args[['type']] = btn_type
  args[['class']] = sprintf("btn btn-%s action-button %s", type, class)
  args[['data-val']] = value
  args[['id']] = inputId

  do.call(
    tags$button,
    c(
      list(list(icon, label)),
      args
    )
  )
}

#' Re-write shiny `fileInput`, but minimal
#' @param inputId,label,multiple,accept,width see \code{shiny::fileInput}
#' @param type button type, such as `primary`, `default`, `warning`, `info`,
#' @param class additional class to button
#' @param ... ignored
#' @export
fileInputMinimal <- function(inputId, label, multiple = FALSE, accept = NULL, width = 'auto', type = 'default', class = '', ...){
  # shiny::fileInput('asd','ad', multiple = F, buttonLabel = 'asddd', width = '100%')
  
  restoredValue = shiny::restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue = NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- jsonlite::toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  input_ui = htmltools::tags$input(id = inputId, name = inputId, type = 'file', style="display: none;")
  if(isTRUE(multiple)){
    input_ui$attribs$multiple = 'multiple'
  }
  if (length(accept) > 0){
    input_ui$attribs$accept <- paste(accept, collapse = ",")
  }
  htmltools::div(
    class = 'form-group shiny-input-container',
    style = sprintf('width: %s', width),
    htmltools::div(
      class = 'input-group',
      htmltools::tags$label(
        class = 'input-group-btn',
        htmltools::span(
          class = sprintf('btn btn-file btn-%s %s', type, class),
          label,
          input_ui
        )
      )
    ),
    htmltools::div(
      id = sprintf('%s_progress', inputId), class = "progress progress-striped active shiny-file-input-progress hidden",
      htmltools::div( class = 'progress-bar' )
    )
  )
}

#' Customized input just designed for RAVE to include multiple inputs and duplicate them
#' @param inputId shiny input ID
#' @param label Label prefix
#' @param components expressions to include ordinary shiny inputs
#' @param max_ncomp maximum components to show
#' @param inital_ncomp initial counts
#' @param prefix fieldset legend
#' @param style additional css style
#' @export
compoundInput <- function(
  inputId, label = '', components = NULL, max_ncomp = 10, inital_ncomp = 1, prefix = 'Group', style = ''
){
  components = substitute(components)
  quos = rlang::quos(!!!as.list(components)[-1])

  quos = lapply(quos, parse_call, env = parent.frame())

  inital_ncomp = max(inital_ncomp, 1)


  div(
    id = inputId,
    class = 'rave-ui rave-ui-compound',
    'data-value' = inital_ncomp,
    'data-max' = max_ncomp,
    div(
      class = 'rave-ui-compound-wrapper',
      tagList(
        lapply(seq_len(max_ncomp), function(ind){
          div(
            class = paste('rave-ui-compound-inner', ifelse(inital_ncomp < ind, 'hidden', '')),
            'data-value' = ind,
            tags$fieldset(
              style = sprintf('border: 1px solid #efefef; padding:.35em .625em .75em; margin-bottom: 15px;%s', style),
              tags$legend(paste(prefix, ind), style = 'border:none; margin: 0; padding: 0 10px; font-size: 14px;'),
              tagList(
                lapply(quos, function(quo){
                  quo = quo$.change_param(
                    inputId = paste0(inputId, '_', quo$.args$inputId, '_', ind),
                    label = quo$.args$label
                  )
                  rlang::eval_tidy(quo)
                })
              )
            )
          )
        })
      )
    ),
    div(
      class = 'rave-ui-compound-ctrl',
      tags$button(class = 'btn btn-default', type = 'button', 'data-target' = inputId, 'data-value'='1', '+'),
      tags$button(class = 'btn btn-default', type = 'button', 'data-target' = inputId, 'data-value'='0', '-')
    ),
    div(
      class = 'rave-ui-compound-meta hidden',
      paste(
        '{',
        paste(
          sapply(quos, function(quo){
            sprintf('"%s":"%s::%s"',
                    eval(quo$.args$inputId, quo$env),
                    quo$.func$func_ns,
                    quo$.func$func_name)
          }),
          collapse = ','
        ),
        '}'
      )

    )
  )

}

#' internally used to cheat RAVE
#' @param rave_id internally used
fake_session <- function(rave_id = '__fake_session__'){
  fakesession = new.env()
  fakesession$sendInputMessage = function(inputId, message){
    return(message)
  }
  fakesession$userData = new.env(parent = emptyenv())
  fakesession$userData$rave_id = rave_id
  fakesession
}

#' internally used for debugging functions (reactive)
#' @param func function
#' @param ... params for function
with_fake_session <- function(func, ...){
  fakesession = new.env()
  fakesession$sendInputMessage = function(inputId, message){
    return(message)
  }
  local({
    func(fakesession, ...)
  }, envir = fakesession)
}

#' internally used for debugging functions
#' @param ... see with_fake_session
#' @param .args same as ...
#' @param .func function to pass to with_fake_session
get_fake_updated_message <- function(..., .args = list(), .func = NULL){
  .args = c(
    list(...),
    .args
  )
  .args = dropNulls(.args)
  if(sum(names(.args) %in% c('choices', 'selected')) > 0){
    .func = shiny::updateSelectInput
  }

  if(is.function(.func)){
    .args = c(
      func = .func,
      .args
    )
    return(do.call(with_fake_session, args = .args))
  }else{
    return(.args)
  }
}

#' Function to update cmpoundInput (broken)
#' @param session shiny session
#' @param inputId shiny input ID
#' @param to extend count
#' @param ... other params
#' @export
updateCompoundInput <- function(session, inputId, to, ...){
  val = isolate(session$input[[inputId]])
  if(is.null(val)){
    return()
  }

  which = max(min(attr(val, 'maxcomp'), to), 1)

  message = list(which = which)
  session$sendInputMessage(inputId, message)
  return(invisible())
}

.ui_update_repo <- new.env()

#' Internally register function to shiny (to be deprecated)
#' @param key input function
#' @param update_func update function
#' @param value_field which is value
#' @param update_value I forget what's this
#' @param default_args default args when initializing
ui_register_function <- function(key, update_func = NULL, value_field = 'value', update_value = F, default_args = list()){
  if(!is.null(update_func) && is.function(update_func)){
    update_func = list(
      update_func = update_func,
      value = value_field,
      default_args = default_args,
      update_value = update_value
    )
    assign(key, update_func, envir = .ui_update_repo)
  }else{
    update_func = get(key, envir = .ui_update_repo)
  }
  assert_that(is.function(update_func$update_func), msg = 'Update function not found!')
  return(invisible(update_func))
}




#' Function to register compound inputs to shiny
register_compoundInput <- function(){

  # Check if function has already been registered
  .registered = get_conf('rave_shiny_compoundInput', default = FALSE)
  if(.registered){
    return(invisible())
  }


  shiny::registerInputHandler("rave.compoundInput", function(data, shinysession, name) {
    if (is.null(data)){
      return(NULL)
    }

    # restoreInput(id = , NULL)
    meta = as.list(data$meta)
    timeStamp = as.character(data$timeStamp)
    maxcomp = as.integer(data$maxcomp)
    inputId = as.character(data$inputId)
    value =  data$val

    ids = names(meta)
    ncomp = as.integer(data$ncomp)
    if(length(ids) == 0 || is.null(ncomp) || ncomp <= 0){
      return(NULL)
    }

    # nvalid = length(dropInvalid(value, deep = T))
    # nvalid = max(1, nvalid)
    # nvalid = min(nvalid, ncomp)

    re = lapply(value, function(val){
      sapply(val, function(v){
        tryCatch({
          jsonlite::fromJSON(v)
        }, error = function(e){
          NULL
        })
      }, simplify = F, USE.NAMES = T)
    })

    attr(re, 'ncomp') <- ncomp
    attr(re, 'meta') <- meta
    attr(re, 'timeStamp') <- timeStamp
    attr(re, 'maxcomp') <- maxcomp
    return(re)

  }, force = TRUE)

  set_conf('rave_shiny_compoundInput', TRUE)
}
