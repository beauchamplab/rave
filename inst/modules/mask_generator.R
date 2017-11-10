SHINY_DESCRIPTION = shiny::HTML(
  '
  Mask Generator
  <br>
  ')
ENABLE_SUMA = FALSE
UNIVARIATE_MODE = 'selected_electrode'


SHINY_INPUT = list(
  SectionInput$new(
    inputId = 'mask_mode',
    label = 'Mode',
    `Mask Input` = list(
      TextInput$new(
        inputId = 'mask_input_text',
        label = 'Mask Input',
        placeholder = '1-10,15,33-100,...',
        init = function(){
          list(
            value = get_local_var('mask_input_text', '')
          )
        }
      )
    ),
    `Value Input` = list(
      FileInput$new(
        inputId = 'mask_input_file',
        label = 'Value File'
      ),
      CheckboxInput$new(
        inputId = 'mask_file_header',
        label = 'My data has header'
      ),
      TextAreaInput$new(
        inputId = 'mask_filter_eval',
        label = 'Filter Expression'
      )
    ),
    `Operators` = list(
      TextAreaInput$new(
        inputId = 'mask_combinator',
        label = 'Combine Masks'
      )
    )
  ),
  TextInput$new(
    inputId = 'mask_name',
    label = 'Mask Name',
    placeholder = 'Name of your mask (use "a"-"z", 0-9, "_"), e.g. mask_1, kimbo, ...',
    triggers = FALSE
  ),
  ActionButton$new(
    inputId = 'mask_submit',
    label = 'Store Mask'
  )

)

params = list(mask_input_text = '1-2')

get_masks = function(text){
  tryCatch({
    text = str_split(text, ',', simplify = T)
    lapply(as.vector(text), function(x){
      if(str_detect(x, '\\-')){
        x = as.integer(str_trim(str_split(x, '\\-', simplify = T)))
        seq(x[1], x[2])
      }else{
        as.integer(x)
      }
    }) %>%
      unlist() ->
      se
    se = se[!is.na(se)]
    sort(unique(se))
  }, error = function(e){
    return(c())
  })->
    selected_electrodes
  return(selected_electrodes)
}



tmp_env = new.env()
with(tmp_env, {
  last_btn_value = 0
  masks = list()
})



SHINY_EXECUTE = function(params, ...){
  print('--------------')

  # TRUE means this people clicked submit button
  submit_btn_val = as.numeric(get_local_var('mask_submit', 0))
  is_save_mask = (tmp_env$last_btn_value < submit_btn_val)
  tmp_env$last_btn_value = submit_btn_val

  subject = data_env$subject
  subject_id = data_env$subject$id

  mode = get_local_var('mask_mode')
  # if(length(mode) != 1){
  #   mode = 'Mask Input'
  # }

  # variables

  if(mode == 'Mask Input'){
    # look at mask_input_text, which is like '1-10, 40, 55-145'...
    eval_text = get_local_var('mask_input_text') # Why use this? becaus params can't capture the input for some weird reason... Actually `get_local_var` is more recommended
    selected_electrodes = get_masks(eval_text)
    selected_electrodes = subject$filter_valid_electrodes(selected_electrodes)

  }else if(mode == 'Value Input'){
    header = get_local_var('mask_file_header')
    file_path = get_local_var('mask_input_file')$datapath
    mask_file = read.csv(file_path, header = header)

    eval_text = get_local_var('mask_filter_eval')
    selected_electrodes = parse_text(eval_text, env = mask_file, default = NULL)

    if(!is.null(selected_electrodes)){
      if(is.logical(selected_electrodes)){
        selected_electrodes = which(selected_electrodes)
      }else{
        selected_electrodes = selected_electrodes %>% unlist %>% as.numeric()
      }


    }
    selected_electrodes = subject$filter_valid_electrodes(selected_electrodes)

  }else if(mode == 'Operators'){
    eval_text = get_local_var('mask_combinator')
    selected_electrodes = parse_text(eval_text, env = tmp_env$masks[[subject_id]], default = NULL)
    if(!is.null(selected_electrodes)){
      if(is.logical(selected_electrodes)){
        selected_electrodes = which(selected_electrodes)
      }else{
        selected_electrodes = selected_electrodes %>% unlist %>% as.numeric()
      }
    }
    selected_electrodes = subject$filter_valid_electrodes(selected_electrodes)

    mask_file = cbind(subject$electrodes[, 1:3], as.data.frame(tmp_env$masks[[subject_id]]))
  }


  # if(length(selected_electrodes) == 0){
  #   selected_electrodes = subject$valid_electrodes
  # }

  bool_sel = rep(FALSE, nrow(subject$electrodes))

  bool_sel[subject$electrodes$Number %in% selected_electrodes] = TRUE


  if(is_save_mask){
    name = str_trim(params$mask_name)
    if(str_length(name) == 0){
      name = 'default_name'
    }else if(!str_detect(name, '^[a-zA-Z]')){
      name = str_c('mask_', name)
    }
    name = str_to_lower(str_replace_all(name, '\\W', '_'))
    if(is.null(tmp_env$masks[[subject_id]])){
      tmp_env$masks[[subject_id]] = list()
    }

    tmp_env$masks[[subject_id]][[name]] = bool_sel
  }

  return(list(
    mask_viewer_print = function(){
      if(sum(bool_sel) == 0){
        cat('No valid electrode selected.')
      }else{
        cat('Total ', sum(bool_sel), ' Electrodes Selected. \n(Invalid electrodes have already been excluded)\n')
        ind = which(bool_sel)
        labels = data_env$subject$electrode_label_by_index(ind)
        ind = str_c(ind, ' - ', labels)
        # ind = ind[1:min(21, length(ind))]
        # if(length(ind) == 21){
        #   ind[21] = '...'
        # }
        cat(str_c(ind, collapse = ', '))
      }

    },
    mask_vis = function(){
      electrodes = data_env$subject$electrodes
      ind = which(bool_sel)
      electrodes %>%
        mutate(Mask = Number %in% ind) ->
        electrodes
      plotly::plot_ly() %>%
        add_markers(
          x = ~Coord_x,
          y = ~Coord_y,
          z = ~Coord_z,
          color = ~Mask,
          hoverinfo = 'text',
          text = ~ sprintf('Label: %s<br />Number: %s<br />Valid: %s', Label, Number, (Valid != 0)),
          data = electrodes)
    },
    mask_viewer_table = function(){
      if(exists('mask_file')){
        if(length(selected_electrodes) > 0){
          mask_file = mask_file[selected_electrodes[selected_electrodes <= nrow(mask_file)], ]
        }

        return(mask_file)
      }

    }
  ))
}

SHINY_OUTPUT = list(
  `Mask Viewer` = list(
    VerbatimTextOutput$new(
      outputId = 'mask_viewer_print',
      title = 'List of Electrodes in Mask',
      width = 7,
      class = 'pre-wrap'
    ),
    PlotlyOutput$new(
      outputId = 'mask_vis',
      title = '3D Visualization',
      width = 5
    ),
    DataTableOutput$new(
      outputId = 'mask_viewer_table',
      title = 'Mask File Preview',
      width = 12
    )
  )
)


SHINY_VALIDATE = function(params){

  mode = get_local_var('mask_mode')
  if(mode == 'Value Input'){
    return(c(
      need(length(get_local_var('mask_input_file')) > 0, 'Please choose a csv file')
    ))
  }else if(mode == 'Operators'){
    return(c(
      need(length(tmp_env$masks[[data_env$subject$id]]) > 0, 'Please Import Masks Via Previous Tabs.')
    ))
  }

  return(NULL)
}

# for standalone script, usually for debug use,

if(!exists('NOT_RUN')){
  require(shiny)
  require(plotly)
  require(tidyverse)
  require(magrittr)
  require(stringr)

  rafe_opts$set_options(
    data_dir = 'D:/Documents/Dropbox/Dropbox/rafe/data',
    # data_dir = '/Users/beauchamplab/Dropbox/rafe/data',
    # module_lookup_file = '/Users/beauchamplab/Dropbox/Researches/rafe/inst/modules_dev.csv',
    module_lookup_file = system.file('modules.csv', package = 'rafe'),
    debug = 'FALSE',
    delay_input = '20'
  )

  subject_id = 'subject_lij119_UnNa'
  electrodes = 1:2
  # module_path = '/Users/beauchamplab/Dropbox/Researches/rafe/inst/modules/mask_generator.R'
  module_path = 'D:/Documents/Dropbox/Dropbox/Researches/rafe/inst/modules/mask_generator.R'
  attach_virtualenv(subject_id, electrodes, module_path)

  params = list(mask_input_text = '1-2')
  results <- SHINY_EXECUTE(params)
  results$mask_viewer_print()

  detach_virtualenv()


}
