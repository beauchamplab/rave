SHINY_DESCRIPTION = shiny::HTML(
  '
  SUMA Viewer
  <br>
  ')
UNIVARIATE_MODE = 'selected_electrode'


SHINY_INPUT = list(
  ActionButton$new(
    inputId = 'suma_start',
    label = 'Launch SUMA'
  ),
  ActionButton$new(
    inputId = 'suma_refresh',
    label = 'Refresh Table'
  )

)


tmp_env = new.env()
tmp_env$subject_id = ''
tmp_env$suma_refresh = -1


SHINY_EXECUTE = function(params, ...){
  subject_id = data_env$subject$id
  suma_refresh = get_local_var('suma_refresh'); if(length(suma_refresh) == 0) suma_refresh = -2

  if(tmp_env$suma_refresh == suma_refresh){
    logger('Launch SUMA')
    .suma$launch_suma(subject_id)
    tmp_env$subject_id = subject_id

  }else{
    logger('Retrieve SUMA')
    tmp_env$suma_refresh = suma_refresh
  }



  # Refresh suma table
  electrodes = unique(rev(.suma$last_chosen))
  electrodes_tbl = data_env$subject$electrodes
  plyr::ldply(electrodes, function(e){
    electrodes_tbl[electrodes_tbl$Number == e, ]
  }) ->
    hist_table



  return(list(
    suma_history = function(){
      hist_table
    }
  ))
}

SHINY_OUTPUT = list(
  `Mask Viewer` = list(
    DataTableOutput$new(
      outputId = 'suma_history',
      title = 'List of Electrodes in Mask',
      width = 12,
      class = 'pre-wrap'
    )
  )
)

