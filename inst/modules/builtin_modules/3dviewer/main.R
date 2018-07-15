require(threejsr)
require(colourpicker)
require(stringr)
require(rave)

session = getDefaultReactiveDomain()
input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
local_data = reactiveValues(
  mask_name = NULL,
  to_be_imported = NULL
)
env = new.env()
env$masks = list()
.module_path = 'Viewer3D'
.module_id = 'viewer_3d'

source('UI.R')
source('import.R')
if(F){
  m = ModuleEnvir$new(module_id = 'id', label_name = 'lb', script_path = './inst/modules/builtin_modules/3dviewer/main.R'); init_app(m)

  rave_prepare('Complete/YAB', 1, 'YABa', c(1,2), data_types = NULL, reference = 'default')
  local_data = list()
  infile = list(
    name = 'CE_2018-07-08_14_51_13.csv',
    datapath = '~/rave_data/data_dir/Complete/YAB/rave/suma/CE_2018-07-08_14_51_13.csv'
  )


  e = m$private$exec_env$mwtNxMl52FzoHsdDbYEp
  local_data = isolate(reactiveValuesToList(e$static_env$local_data))
  env = e$static_env$env
}



data_controls_name = function(){
  local_data$refresh_controller
  name = local_data$mask_name
  if(!length(name) == 1 || !name %in% names(env$masks)){
    name = NULL
  }
  local_data$refresh_control_pane = Sys.time()
  selectInput(ns('mask_name'), 'Select a Dataset for Visualization', choices = c('_Blank', names(env$masks)), selected = name)
}


data_controls_details = function(){
  local_data$refresh_control_pane
  name = local_data$mask_name
  name %?<-% '_Blank'
  ui = NULL
  if(name %in% names(env$masks)){
    mask = env$masks[[name]]
    local_data$controller_data = mask
    switch(
      mask$type,
      'static' = {
        ui = tagList(
          selectInput(ns('main_var'), 'Display Colours', choices = mask$header),
          selectInput(ns('thred_var'), 'Threshold', choices = mask$header),
          sliderInput(ns('thred_rg'), 'Range', min = 0, max = 1, value = c(0,1), round = -2L),
          selectInput(ns('info_var'), 'Click Info', choices = mask$header, multiple = T, selected = mask$header),
          checkboxInput(ns('col_sym'), 'Symmetric Color', value = T)
        )
      },
      'animation' = {
        ui = tagList(
          sliderInput(ns('ani_log_fps'), 'Speed', min = -2, max = 2, value = 0, step = 0.01),
          checkboxInput(ns('col_sym'), 'Symmetric Color', value = T)
        )
      }
    )
  }
  ui
}

observe({
  local_data$mask_name = input$mask_name
  local_data$col_sym = input$col_sym
  local_data$main_var = input$main_var
  local_data$thred_var = input$thred_var
  local_data$thred_rg = input$thred_rg
  local_data$info_var = input$info_var
})

observe({
  try({
    mask = local_data$controller_data
    thred_var = local_data$thred_var
    col = mask$header == thred_var
    val = mask$body[, col]
    val = as.numeric(val)
    val = val[!is.na(val)]
    if(length(val)){
      val = range(val, na.rm = T)

      if(val[1] < val[2]){
        val[1] = floor(val[1] * 100) / 100
        val[2] = ceiling(val[2] * 100) / 100
        updateSliderInput(session, 'thred_rg', label = sprintf('Range (%s)', thred_var), min = val[1], max = val[2], value = val)
      }
    }
  }, silent = T)
})




rave_execute({

})



viewer = function(){
  try({
    name = local_data$mask_name
    name %?<-% '_Blank'
    if(name %in% names(env$masks)){
      mask = local_data$controller_data
    }else{
      mask = NULL
    }
    mask %?<-% list(
      electrodes = NULL,
      values = NULL
    )
    mask$type %?<-% '_blank'

    col_sym = local_data$col_sym
    col_sym %?<-% T

    marker = apply(subject$electrodes, 1, function(x){
      as.character(div(
        h5('Electrode - ', x['Electrode'], ' ', tags$small(sprintf(
          '%s [%s, %s]', x['Label'], x['Group'], x['Type']
        )))
      ))
    })

    switch (mask$type,
            '_blank' = {
              module_tools$plot_3d_electrodes(
                tbl = subject$electrodes,
                marker = marker,
                fps = 1,
                loop = F,
                control_gui = F,
                background_colors = c(bgcolor, '#000000'),
                control = mouse_control
              )
            },
            'static' = {
              main_var = local_data$main_var
              thred_var = local_data$thred_var
              thred_rg = local_data$thred_rg
              thred_rg %?<-% c(-Inf, Inf)
              info_var = local_data$info_var
              body = mask$body[order(mask$electrodes), ]
              mask$electrodes = sort(mask$electrodes)

              # thred value
              values = as.numeric(body[, mask$header == main_var])
              t_vals = as.numeric(body[, mask$header == thred_var])
              sel = !is.na(t_vals) & t_vals %within% thred_rg & (mask$electrodes %in% subject$filter_all_electrodes(mask$electrodes))

              body = mask$body[sel, ]
              values = values[sel]
              electrodes = mask$electrodes[sel]

              if(length(info_var)){
                # marker should be shown even the electrode is filtered out
                sapply(info_var, function(v){
                  mk = unlist(mask$body[, mask$header == v])
                  if(is.numeric(mk)){
                    # mk = sprintf('%.4f', mk)
                    mk = prettyNum(mk, digits=4, format="fg")
                  }
                  sapply(mk, function(x){
                    as.character(
                      tags$li(tags$label(v), ' ', x)
                    )
                  })
                }) ->
                  tmp
                apply(tmp, 1, function(x){
                  as.character(
                    div(
                      tags$ul(HTML(x))
                    )
                  ) ->
                    s
                  str_remove_all(s, '\\n')
                }) ->
                  tmp
                in_mask = subject$electrodes$Electrode %in% mask$electrodes
                marker[in_mask] = str_c(
                  marker[in_mask],
                  tmp
                )
              }else{
                marker = NULL
              }

              module_tools$plot_3d_electrodes(
                tbl = subject$electrodes,
                electrodes = electrodes,
                values = values,
                symmetric = col_sym,
                marker = marker,
                fps = 1,
                loop = F,
                control_gui = F,
                background_colors = c(bgcolor, '#000000'),
                control = mouse_control
              )
            },
            'animation' = {
              module_tools$plot_3d_electrodes(
                tbl = subject$electrodes,
                electrodes = mask$electrodes,
                values = t(mask$body),
                symmetric = col_sym,
                marker = marker,
                fps = 1,
                loop = T,
                control_gui = T,
                background_colors = c(bgcolor, '#000000'),
                control = mouse_control
              )
            }
    )
  }, silent = T)



}




