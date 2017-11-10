# #' @include cls_observable.R
# shiny components



#' @export
InputComponent <- R6::R6Class(
  'InputComponent',
  # inherit = Observable,

  portable = FALSE,

  public = list(
    shiny_function = NULL,
    shiny_update = NULL,

    triggers = TRUE,

    module = NULL,
    ns = NULL,

    id = NULL, # local variable ID
    global_var = NULL,

    allowed_fields = c('inputId', 'label'),
    argList = list(),

    valueName = 'value',

    init = NULL,
    update = NULL,
    activated = NULL,

    activate = function(input, triggers = NULL, module_id, session){
      if(session$userData$session_id %in% self$activated){
        self$update(session)
        return()
      }
      self$module <- data_repository$get_module(module_id = module_id)
      logger(self$module$id, 'asdadsadsadsadasd')

      if(is.function(self$init)){
        self$update <- function(session){
          new_args <- self$init()
          if(is.list(new_args)){
            new_args = new_args[names(new_args) %in% self$allowed_fields]
            new_args[['inputId']] <- NULL
            if(length(new_args) > 0){
              new_args[['inputId']] <- self$id
              new_args[['session']] <- session
              do.call(self$shiny_update, args = new_args)
            }
          }
        }
      }else{
        self$update <- function(session){}
      }
      self$update(session)

      # register observe
      shiny::observeEvent(input[[self$id]], {
        set_local_var <- get('set_local_var', envir = self$module$runtime_env)
        set_global_var <- get('set_global_var', envir = self$module$runtime_env)

        p <- list(input[[self$id]])
        names(p) <- self$id
        set_local_var(.list = p)

        if(!is.null(self$global_var)){
          names(p) <- self$global_var
          set_global_var(.list = p)
        }


        if(self$triggers){
          triggers$input = Sys.time()
        }

      })

      logger('Activating Input - ', self$id, ' - ', self$module$id)
      self$activated = c(self$activated, session$userData$session_id)
    },

    initialize = function(inputId,
                          init = NULL, global_var = NULL, module = I, triggers = TRUE, ...){
      self$id <- inputId
      self$ns <- ns
      self$global_var <- global_var
      self$triggers <- triggers

      # arglist
      argList <- list(...)
      argList[['inputId']] <- inputId
      for(n in self$allowed_fields){
        self$argList[[n]] <- argList[[n]]
      }
      # init
      if(is.function(init)){
        self$init <- init
      }
    }

  ),


  active = list(
    ui = function(){
      if(is.function(self$shiny_function)){
        shiny_function <- force(self$shiny_function)
        argList <- force(self$argList)
        argList[['inputId']] <- self$ns(argList[['inputId']])
        function(){
          do.call(shiny_function, args = argList)
        }
      }
    }
  )
)


#' @export
SectionInput <- R6::R6Class(
  'SectionInput',
  inherit = InputComponent,
  portable = FALSE,

  private = list(
    input_comps = list(),
    choices = c(),
    selected = NULL
  ),
  public = list(
    initialize = function(inputId, label, selected = NULL, ...){
      # Name of items are choices, if selected matches with the name, that section will be rendered, else the first one
      input_comps = list(...)
      if(length(selected) == 0 || is.na(selected) || !selected %in% names(input_comps)){
        private$selected = names(input_comps)[1]
      }else{
        private$selected = selected
      }

      private$choices = names(input_comps)
      private$input_comps = input_comps

      super$initialize(inputId, label = label)
    },
    activate = function(input, triggers = NULL, module_id, session){
      if(session$userData$session_id %in% self$activated){
        for(section in names(private$input_comps)){
          for(comp in private$input_comps[[section]]){
            comp$update(session)
          }
        }
        return()
      }
      self$module <- data_repository$get_module(module_id = module_id)

      logger('Activating Section Input - ', self$id, ' @ ', self$module$id)
      self$activated = c(self$activated, session$userData$session_id)


      for(section in names(private$input_comps)){
        for(comp in private$input_comps[[section]]){
          comp$activate(input, triggers, module_id, session)
        }
      }

      # register observe
      set_local_var <- get('set_local_var', envir = self$module$runtime_env)

      # since sectionInput is not shinyinput, so we need to register local_val by ourselves
      .list = list(
        private$selected
      )
      names(.list) = self$id
      set_local_var(.list = p)


      shiny::observeEvent(input[[self$id]], {


        p <- list(input[[self$id]])
        names(p) <- self$id
        set_local_var(.list = p)

        if(self$triggers){
          triggers$input = Sys.time()
        }

      })



    }
  ),
  active = list(
    ui = function(){

      # <div class="btn-group" role="group" aria-label="...">
      #   <button type="button" class="btn btn-default">Left</button>
      #     <button type="button" class="btn btn-default">Middle</button>
      #       <button type="button" class="btn btn-default">Right</button>
      #         </div>
      return(function(){
        ns <- self$ns

        do.call(
          shiny::tabsetPanel,
          args = c(
            list(
              id = ns(self$id),
              selected = private$selected
            ),
            lapply(names(private$input_comps), function(section){
              do.call(shiny::tabPanel, args = c(
                list(
                  title = section,
                  style = 'padding-top: 15px;'
                ),
                lapply(private$input_comps[[section]], function(comp){
                  comp$ns <- ns
                  comp$ui()
                })
              ))
            })
          )
        )


        # shiny::div(
        #   id = ns(self$id),
        #   class = 'Section-Input',
        #   shiny::radioButtons(inputId = ns(selection_id), label = self$argList$label, choices = private$choices, selected = private$selected),
        #
        #   lapply(names(private$input_comps), function(section){
        #     shiny::div(
        #       `data-info` = section,
        #       args <- lapply(private$input_comps[[section]], function(comp){
        #         comp$ns <- ns
        #         comp$ui()
        #       }) %>%
        #         shiny::tagList()
        #     )
        #   }) %>%
        #     shiny::tagList()
        # )
      })

    }
  )
)

#' @export
TextInput <- R6::R6Class(
  'TextInput',
  inherit = InputComponent,
  portable = FALSE,

  public = list(
    initialize = function(inputId, label, ...){
      self$allowed_fields <- c('inputId', 'label', 'value', 'width', 'placeholder')
      self$shiny_function <- shiny::textInput
      self$shiny_update <- shiny::updateTextInput
      self$valueName <- 'value'

      super$initialize(inputId, label = label, ...)
    }
  )
)

#' @export
TextAreaInput <- R6::R6Class(
  'TextAreaInput',
  inherit = InputComponent,
  portable = FALSE,

  public = list(
    initialize = function(inputId, label, ...){
      self$allowed_fields <- c('inputId', 'label', 'value', 'width', 'placeholder', 'height',
                               'cols', 'rows', 'resize')
      self$shiny_function <- shiny::textAreaInput
      self$shiny_update <- shiny::updateTextAreaInput
      self$valueName <- 'value'

      super$initialize(inputId, label = label, ...)
    }
  )
)

#' @export
NumericInput <- R6::R6Class(
  'NumericInput',
  inherit = InputComponent,
  portable = FALSE,

  public = list(
    initialize = function(inputId, label, value, ...){
      self$allowed_fields <- c('inputId', 'label', 'value', 'min', 'max', 'step', 'width')
      self$shiny_function <- shiny::numericInput
      self$shiny_update <- shiny::updateNumericInput
      self$valueName <- 'value'

      super$initialize(inputId, label = label, value = value, ...)
    }
  )
)

#' @export
CheckboxInput <- R6::R6Class(
  'CheckboxInput',
  inherit = InputComponent,
  portable = FALSE,
  public = list(
    initialize = function(inputId, label, ...){
      self$allowed_fields <- c('inputId', 'label', 'value', 'width')
      self$shiny_function <- shiny::checkboxInput
      self$shiny_update <- shiny::updateCheckboxInput
      self$valueName <- 'value'

      super$initialize(inputId, label = label, ...)
    }
  )
)

#' @export
SelectInput <- R6::R6Class(
  'SelectInput',
  inherit = InputComponent,
  portable = FALSE,

  public = list(
    initialize = function(inputId, label, choices, ...){
      self$allowed_fields <- c('inputId', 'label', 'choices', 'selected', 'multiple',
                               'selectize', 'width', 'size')
      self$shiny_function <- shiny::selectInput
      self$shiny_update <- shiny::updateSelectInput
      self$valueName <- 'selected'

      super$initialize(inputId, label = label, choices = choices, ...)
    }
  )
)

#' @export
SliderInput <- R6::R6Class(
  'SelectInput',
  inherit = InputComponent,
  portable = FALSE,

  public = list(
    initialize = function(inputId, label, min, max, value, ...){
      self$allowed_fields <- c('inputId', 'label', 'min', 'max', 'value', 'step', 'round',
                               'format', 'locale', 'ticks', 'animate',
                               'width', 'sep', 'pre', 'post', 'timeFormat',
                               'timezone', 'dragRange')
      self$shiny_function <- shiny::sliderInput
      self$shiny_update <- shiny::updateSliderInput
      self$valueName <- 'value'

      super$initialize(inputId, label = label, min = min, max = max, value = value, ...)
    }
  )
)


#' @export
RadioButtons <- R6::R6Class(
  'SelectInput',
  inherit = InputComponent,
  portable = FALSE,

  public = list(
    initialize = function(inputId, label, ...){
      self$allowed_fields <- c('inputId', 'label', 'choices', 'selected',
                               'inline', 'width')
      self$shiny_function <- shiny::radioButtons
      self$shiny_update <- shiny::updateRadioButtons
      self$valueName <- 'selected'

      super$initialize(inputId, label = label, ...)
    }
  )
)


#' @export
ActionButton <- R6::R6Class(
  'ActionButton',
  inherit = InputComponent,
  portable = FALSE,

  public = list(
    initialize = function(inputId, label, ...){
      self$allowed_fields <- c('inputId', 'label', 'icon', 'width', 'style')
      self$shiny_function <- shiny::actionButton
      self$shiny_update <- shiny::updateActionButton

      super$initialize(inputId, label = label, ...)
    }
  )
)


#' @export
FileInput <- R6::R6Class(
  'FileInput',
  inherit = InputComponent,
  portable = FALSE,

  public = list(
    initialize = function(inputId, label, ...){
      self$allowed_fields <- c('inputId', 'label', 'multiple', 'accept', 'width',
                               'buttonLabel', 'placeholder')
      self$shiny_function <- shiny::fileInput

      super$initialize(inputId, label = label, init = NULL, ...) # NO UPDATE on refresh
    }
  )
)



#' @export
OutputComponent <- R6::R6Class(
  'OutputComponent',
  portable = FALSE,

  public = list(
    shiny_function = NULL,
    shiny_render = NULL,
    ns = NULL,

    id = NULL,
    title = NULL,
    width = 12,
    class = NULL,

    activated = NULL,
    argList = list(),

    initialize = function(outputId, title, width = 12, class = NULL, ns = I, ...){
      self$id = outputId
      self$title <- title
      self$width <- width
      self$class <- class

      self$argList <- list(...)
      self$argList[['outputId']] <- outputId

      self$ns <- ns
    },

    activate = function(output, triggers, module_id, session = getDefaultReactiveDomain()){
      if(session$userData$session_id %in% self$activated){
        return()
      }

      local({


        output[[self$id]] <- self$shiny_render({
          validate(
            need(!is.null(triggers$output), 'Updating')
          )
          # validate(
          #   need(!is.null(triggers$output), 'Updating2')
          # )

          logger('Render Output - ', self$id)
          module <- data_repository$get_module(module_id = module_id)

          validate(
            module$validate
          )

          # if(length(module$validate) > 0){
          #   shiny::validate(
          #     c(
          #       module$validate,
          #       shiny::need(length(module$results) > 0, 'Initializing...')
          #     )
          #   )
          # }


          res <- module$results[[self$id]]
          if(is.function(res)){
            res()
          }else{
            res
          }
        })
      })



      logger('Activating Output - ', self$id)
      self$activated <- c(self$activated, session$userData$session_id)
    }
  ),

  active = list(
    ui = function(){
      argList <- self$argList
      argList[['outputId']] <- self$ns(self$id)
      function(){
        shinydashboard::box(
          width = self$width,
          title = self$title,
          collapsible = TRUE,
          div(
            class = self$class,
            do.call(self$shiny_function, args = argList)
          )
        )

      }
    }
  )

)


#' @export
PlotOutput <- R6::R6Class(
  'PlotOutput',
  inherit = OutputComponent,
  portable = FALSE,

  public = list(
    initialize = function(outputId, title, ...){
      self$shiny_function <- shiny::plotOutput
      self$shiny_render <- shiny::renderPlot

      super$initialize(outputId, title, ...)
    }
  )
)



#' @export
PlotlyOutput <- R6::R6Class(
  'PlotlyOutput',
  inherit = OutputComponent,
  portable = FALSE,

  public = list(
    initialize = function(outputId, title, ...){
      self$shiny_function <- plotly::plotlyOutput
      self$shiny_render <- plotly::renderPlotly

      super$initialize(outputId, title, ...)
    }
  )
)



#' @export
DataTableOutput <- R6::R6Class(
  'DataTableOutput',
  inherit = OutputComponent,
  portable = FALSE,

  public = list(
    initialize = function(outputId, title, ...){
      self$shiny_function <- shiny::dataTableOutput
      self$shiny_render <- shiny::renderDataTable

      super$initialize(outputId, title, ...)
    }
  )
)



#' @export
VerbatimTextOutput <- R6::R6Class(
  'VerbatimTextOutput',
  inherit = OutputComponent,
  portable = FALSE,

  public = list(
    initialize = function(outputId, title, ...){
      self$shiny_function <- shiny::verbatimTextOutput
      self$shiny_render <- shiny::renderPrint

      super$initialize(outputId, title, ...)
    }
  )
)


#' @export
UiOutput <- R6::R6Class(
  'UiOutput',
  inherit = OutputComponent,
  portable = FALSE,

  public = list(
    initialize = function(outputId, title, ...){
      self$shiny_function <- shiny::uiOutput
      self$shiny_render <- shiny::renderUI

      super$initialize(outputId, title, ...)
    }
  )
)



