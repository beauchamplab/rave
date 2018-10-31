#' Re-write shint actionButton to enable styles
#' @param inputId see shiny::actionButton
#' @param label  see shiny::actionButton
#' @param icon see shiny::actionButton
#' @param width see shiny::actionButton
#' @param type default, primary, info... see bootstrap
#' @param btn_type html tag attribute "type"
#' @param class additional classes
#' @param ... other methods passed to actionButton
#' @import htmltools
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
  value <- deparse(substitute(components))
  dots = lazyeval::as.lazy(value)
  dots = lazyeval::as.lazy_dots(as.list(dots$expr[-1]))
  lapply(dots, parse_call, env = parent.frame()) ->
    dots
  class(dots) <- 'lazy_dots'

  inital_ncomp = max(inital_ncomp, 1)


  div(
    id = inputId,
    class = 'rave-ui rave-ui-compound',
    'data-value' = inital_ncomp,
    'data-max' = max_ncomp,
    div(
      class = 'rave-ui-compound-wrapper',
      tagList(
        lapply(seq(1, max_ncomp), function(ind){
          div(
            class = paste('rave-ui-compound-inner', ifelse(inital_ncomp < ind, 'hidden', '')),
            'data-value' = ind,
            tags$fieldset(
              style = sprintf('border: 1px solid #efefef; padding:.35em .625em .75em; margin-bottom: 15px;%s', style),
              tags$legend(prefix %&% ' ' %&% ind, style = 'border:none; margin: 0; padding: 0 10px; font-size: 14px;'),
              tagList(
                lapply(dots, function(comp){
                  comp = comp$.change_param(
                    inputId = paste0(inputId, '_', comp$.args$inputId, '_', ind),
                    label = comp$.args$label
                  )
                  lazyeval::lazy_eval(comp)
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
          sapply(dots, function(comp){
            sprintf('"%s":"%s::%s"',
                    comp$.args$inputId,
                    comp$.func$func_ns,
                    comp$.func$func_name)
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
  assertthat::assert_that(is.function(update_func$update_func), msg = 'Update function not found!')
  return(invisible(update_func))
}



