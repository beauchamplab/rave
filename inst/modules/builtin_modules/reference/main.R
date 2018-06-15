# Module for referencing electrodes


# Init virtualenv for module dev
rave_prepare(subject = 'Complete/YAB', electrodes = 13:20, epoch = 'YABa', time_range = c(1,2), data_types = NULL, reference = 'default')



# load libraries
library(shiny)
library(rave)
library(stringr)
library(magrittr)

# Shiny session used to update inputs in advanced ways
session = getDefaultReactiveDomain()
local_data = shiny::reactiveValues(
  group_number = NULL,
  refresh = NULL
)

ref_group %?<-% list()
# Environment to store information
env = new.env(parent = baseenv())
env$ref_calc = 0
env$last_import = 'new..'

# Load UIs
source('UI.R')
# source('./inst/modules/builtin_modules/reference/UI.R')

observeEvent(session$input[[ns('bipolar_modal')]], {
  # get group info
  group_info = current_group()
  if(!length(group_info)){
    return()
  }

  ref_tbl = get_ref_table()
  electrodes = group_info$electrodes
  bptbl = ref_tbl[ref_tbl$Electrode %in% electrodes,]
  bptbl$Type = 'Bipolar Reference'

  if(nrow(bptbl) > 0 && unique(bptbl$Reference) == 'noref'){
    e = bptbl$Electrode
    bptbl$Reference = c(paste0('ref_', e[-1]), '')
  }
  env$bipolar_tbl = bptbl

  showModal(
    shiny::modalDialog(
      title = 'Bipolar Reference',
      size = 'l',
      easyClose = F,
      footer = tagList(
        actionButton(ns('bp_confirm'), 'Confirm')
      ),
      DT::DTOutput(ns('bipolar_table'))
    )
  )
}, event.env = ..runtime_env, handler.env = ..runtime_env)

observeEvent(session$input[[ns('bp_confirm')]], {
  tbl = env$bipolar_tbl
  ref_tbl = get_ref_table()
  if(nrow(tbl)){
    for(ii in seq_len(nrow(tbl))){
      sel = ref_tbl$Electrode == tbl$Electrode[ii]
      ref_tbl$Reference[sel] = tbl$Reference[ii]
      ref_tbl$Type[sel] = 'Bipolar Reference'
    }
    # save ref_tbl
    save_ref_table(ref_tbl)
  }
  removeModal(session = session)
}, event.env = ..runtime_env, handler.env = ..runtime_env)

bipolar_proxy = DT::dataTableProxy(ns('bipolar_table'), session = session)

session$output[[ns('bipolar_table')]] = DT::renderDT({
  env$bipolar_tbl
}, env = ..runtime_env, editable = TRUE)

observeEvent(session$input[[ns('bipolar_table_cell_edit')]], {
  info = session$input[[ns('bipolar_table_cell_edit')]]
  i = info$row
  j = info$col
  v = info$value

  # string match electrode
  v = str_match(v, '(ref_|[\\ ]{0})([0-9]*)')[3]
  if(is_invalid(v, .invalids = c('null', 'na', 'blank'))){
    v = ''
  }else{
    v = subject$filter_all_electrodes(as.integer(v))
    if(!length(v)){
      return()
    }else{
      v = str_c('ref_', v)
    }
  }


  bipolar_tbl = env$bipolar_tbl

  if(names(bipolar_tbl)[j] == 'Reference'){
    env$bipolar_tbl[i, j] = v
    DT::replaceData(bipolar_proxy, env$bipolar_tbl, resetPaging = FALSE)  # important
  }
}, event.env = ..runtime_env, handler.env = ..runtime_env)

session$output[[ns('elec_loc')]] = renderPlot({
  local_data$refresh
  group_info = current_group()
  group_info %?<-% list(electrodes = NULL)
  x = subject$electrodes$Coord_x
  y = subject$electrodes$Coord_y
  sel = subject$electrodes$Electrode %in% group_info$electrodes
  plot(x,y, col = sel + 1, pch = 16)
}, env = ..runtime_env)

observeEvent(session$input[[ns('cur_save')]], {
  ref_to = session$input[[ns('ref_to')]]
  group_info = current_group()
  if(is.null(group_info)){
    return()
  }
  electrodes = group_info$electrodes
  bad_electrodes = rave:::parse_selections(session$input[[ns('ref_bad')]])

  ref_table = get_ref_table()
  sel = ref_table$Electrode %in% electrodes


  ref_table$Group[sel] = group_info$rg_name
  if(group_info$rg_type %in% c('Common Average Reference', 'White Matter Reference', 'No Reference')){
    ref_table$Reference[sel] = ref_to
    ref_table$Reference[sel & ref_table$Electrode %in% bad_electrodes] = '' # set bad electrodes
    ref_table$Type[sel] = group_info$rg_type
    save_ref_table(tbl = ref_table)
    showNotification(p(
      group_info$rg_name, ' (', group_info$rg_electrodes, ') is now set to be referenced to [', ref_to, ']'
    ), type = 'message')
  }
}, priority = -1L, handler.env = ..runtime_env, event.env = ..runtime_env, domain = session)

# Customized UI
cur_group_ui = function(){
  refresh = local_data$refresh

  if(length(cur_group) && cur_group <= length(ref_group)){
    group_number = as.integer(cur_group)
    group_info = ref_group[[group_number]]
    group_type = group_info$rg_type
    group_name = group_info$rg_name
    electrodes = rave:::parse_selections(group_info$rg_electrodes)
    if(length(electrodes) == 0){
      return(hr())
    }
  }else{
    return(hr())
  }

  refs = get_refs()
  ref_names = names(refs); ref_names = c('noref', ref_names)

  ref_tbl = get_ref_table()

  sel = ref_tbl$Electrode %in% electrodes


  switch (
    group_type,
    'No Reference' = {
      selectInput(ns('ref_to'), 'Reference to:', choices = 'noref', selected = 'noref')
    },
    'Bipolar Reference' = {
      tagList(
        tags$label('Reference to:'),
        actionButton(ns('bipolar_modal'), 'Open Table', width = '100%', style = 'margin-bottom: 15px')
      )
    },
    # By default, it's either 'Common Average Reference' or 'White Matter Reference'
    {
      # try to get reference name
      selected = unique(c(ref_tbl$Reference[sel]), 'noref')
      selected = selected[selected != ''][1]
      selectInput(ns('ref_to'), 'Reference to:', choices = ref_names, selected = selected)
    }

  ) ->
    inp




  tagList(
    hr(),
    plotOutput(ns('elec_loc')),
    fluidRow(
      column(
        width = 7,
        inp,
        p(
          tags$label('Group Name: '), group_name, br(),
          tags$label('Electrodes: '), group_info$rg_electrodes, br(),
          tags$label('Bad Electrodes: '), textOutput(ns('bad_electrodes_out'), inline = T)
        )
      ),
      column(
        width = 5,
        textInput(ns('ref_bad'), 'Bad Electrodes:', value = rave:::deparse_selections(ref_tbl$Electrode[sel & ref_tbl$Reference == ''])),
        div(
          style = 'float: right',
          actionButton(ns('cur_save'), 'Save Group')
        )
      )
    ),
    hr()
  )
}

session$output[[ns('bad_electrodes_out')]] = renderText({
  bad_electrodes = rave:::parse_selections(session$input[[ns('ref_bad')]])
  bad_electrodes = subject$filter_all_electrodes(bad_electrodes)
  if(length(bad_electrodes)){
    bad_electrodes = rave:::deparse_selections(bad_electrodes)
    bad_electrodes
  }else{
    'No bad electrode'
  }
}, env = ..runtime_env)

# Utils
current_group = function(){
  group_number = as.integer(cur_group)
  if(!length(group_number) || group_number > length(ref_group)){
    return()
  }
  group_info = ref_group[[group_number]]
  electrodes = rave:::parse_selections(group_info$rg_electrodes)
  electrodes = subject$filter_all_electrodes(electrodes)
  if(!length(electrodes)){
    return()
  }
  group_info$electrodes = electrodes
  return(group_info)
}
get_ref_table = function(){
  ref_info = cache(key = list(
    ref_name_alt = ref_name_alt,
    subject = subject$id
  ), import_external())
  ref_table = ref_info$table
  ref_table
}

save_ref_table = function(tbl, is_new = FALSE){
  print('Saving')
  val = list(
    table = tbl,
    new = is_new
  )
  cache(key = list(
    ref_name_alt = ref_name_alt,
    subject = subject$id
  ), val, replace = T)
  local_data$ref_tbl = tbl
  invisible()
}

import_external = function(){
  dirs = module_tools$get_subject_dirs()
  ref_name_alt %?<-% 'new..'
  f = file.path(dirs$meta_dir, ref_name_alt)
  if(file.exists(f)){
    tbl = read.csv(f, stringsAsFactors = F)
    if(!'Type' %in% names(tbl)){
      tbl$Type = 'No Reference'
    }else{
      tbl$Type[!tbl$Type %in% c('Common Average Reference', 'Bipolar Reference', 'White Matter Reference', 'No Reference')] = 'No Reference'
    }
    tbl = tbl[,c('Electrode', 'Group', 'Reference', 'Type')]
    is_new = T
  }else{
    tbl = data.frame(
      Electrode = subject$valid_electrodes,
      Group = '',
      Reference = 'noref',
      Type = 'No Reference',
      stringsAsFactors = F
    )
    is_new = F
  }
  list(
    table = tbl,
    new = is_new
  )
}
load_reference = function(){
  dirs = module_tools$get_subject_dirs()
  ref_name_alt %?<-% 'new..'
  # Get current settings
  key = list(
    ref_name_alt = ref_name_alt,
    subject = subject$id
  )

  ref_info = cache(key = key, import_external())
  ref_tbl = (ref_info$table)

  if(is.null(ref_tbl)){
    return()
  }

  if(env$last_import != ref_name_alt){
    env$last_import = ref_name_alt
    ref_info$new = TRUE
  }else{
    ref_info$new = FALSE
  }

  # If ref_info$new, update compound input ref_group s.t. it matches with current settings, else replace and cache ref_info
  if(ref_info$new){
    ref_info$new = FALSE
    unique_refs = ref_tbl[!duplicated(ref_tbl[,c('Group', 'Type')]), ]
    nn = nrow(unique_refs)
    if(nn > 0){
      lapply(seq_len(nn), function(i){
        # Group i
        row = unique_refs[i, ]
        # name
        updateTextInput(session, ns(sprintf('%s_%s_%d', 'ref_group', 'rg_name', i)), value = row$Group)
        # ref Method
        updateSelectInput(session, ns(sprintf('%s_%s_%d', 'ref_group', 'rg_type', i)), selected = row$Type)
        # Electrodes
        merged = merge(ref_tbl, row, by = c('Group', 'Type'), suffixes = c('', 'y'))
        updateTextInput(
          session,
          ns(sprintf('%s_%s_%d', 'ref_group', 'rg_electrodes', i)),
          value = rave:::deparse_selections(merged$Electrode)
        )

        updateCompoundInput(session, ns('ref_group'), to = nn)
      })

    }

  }else{
    # Construct table
    all_es = NULL
    for(ii in seq_len(length(ref_group))){
      sub_group = ref_group[[ii]]
      sub_es = sub_group$rg_electrodes
      sub_es = rave:::parse_selections(sub_es)
      if(any(sub_es %in% all_es)){
        dup_es = sub_es[sub_es %in% all_es]
        showNotification(
          p('Group [', sub_group$rg_name, '(', ii, ')] has duplicated electrode(s): ', rave:::deparse_selections(dup_es)),
          type = 'warning'
        )
      }
      all_es = c(all_es, sub_es)
      sub_sel = ref_tbl$Electrode %in% sub_es
      if(any(sub_sel)){

        ref_tbl$Group[sub_sel] = sub_group$rg_name
        ref_tbl$Type[sub_sel] = sub_group$rg_type
      }
    }

    ref_info$table = ref_tbl
  }

  cache(key = key, val = ref_info, replace = T)
}

gen_reference = function(electrodes){
  electrodes = module_tools$get_valid_electrodes(electrodes)
  if(length(electrodes) == 0){
    return()
  }
  dirs = module_tools$get_subject_dirs()
  fname = sprintf('ref_%s.h5', rave:::deparse_selections(electrodes))
  f = file.path(dirs$channel_dir, 'reference', fname)
  # generate reference
  # Step 0: chunk matrix
  ncores = rave_options('max_worker')
  ncols = ceiling(length(electrodes) / ncores)
  nes = length(electrodes)
  mat = matrix(NA, nrow = ncores, ncol = ncols)
  mat[seq_along(electrodes)] = electrodes

  # Summing up
  env$volt = list()
  env$coef = list()


  progress = rave::progress(sprintf('Generating reference [%s]', fname), max = length(electrodes)+1)
  on.exit(progress$close())

  blocks = subject$preprocess_info('blocks')


  lapply(seq_len(ncols), function(ii){
    es = mat[, ii]
    es = es[!is.na(es)]

    lapply_async(es, function(e){
      root_dir = dirs$channel_dir
      fname = sprintf('%d.h5', e)
      sapply(blocks, function(b){
        coef = sqrt(load_h5(file.path(root_dir, 'power', fname), name = sprintf('/raw/power/%s', b))[])
        phase = exp(1i * load_h5(file.path(root_dir, 'phase', fname), name = sprintf('/raw/phase/%s', b))[])
        list(
          volt = load_h5(file.path(root_dir, 'voltage', fname), name = sprintf('/raw/voltage/%s', b))[],
          coef = coef * phase
        )
      }, USE.NAMES = T, simplify = F) ->
        re
      gc()
      return(re)
    }, .call_back = function(i){
      progress$inc(message = sprintf('Loading electrode %d', es[[i]]))
    }) ->
      re


    gc()
    lapply(re, function(dat){
      for(b in blocks){
        if(length(env$volt[[b]])){
          env$volt[[b]] = env$volt[[b]] + dat[[b]][['volt']]
          env$coef[[b]] = env$coef[[b]] + dat[[b]][['coef']]
        }else{
          env$volt[[b]] = dat[[b]][['volt']]
          env$coef[[b]] = dat[[b]][['coef']]
        }
      }
      NULL
    })
  })

  progress$inc(message = 'Saving to disk.')

  # Average
  for(b in blocks){
    volt = env$volt[[b]] / nes
    coef = env$coef[[b]] / nes
    coef = array(c(Mod(coef), Arg(coef)), dim = c(dim(coef), 2)) # Freq x Time x 2
    save_h5(volt, file = f, name = sprintf('/voltage/%s', b), chunk = 1024, replace = T)
    save_h5(coef, file = f, name = sprintf('/wavelet/coef/%s', b), chunk = c(dim(coef)[1], 128, 2), replace = T)
  }

  showNotification(p('Reference [', fname, '] exported.'), type = 'message')
}
check_reference = function(){
  ref_es = rave:::parse_selections(ref_electrodes)
  ref_es = module_tools$get_valid_electrodes(ref_es)

  if(length(ref_es)){
    ref_calc_label = 'Generate [ref_' %&% rave:::deparse_selections(ref_es) %&% "]"
  }else{
    ref_calc_label = 'Generate Reference'
  }

  ref_calc %?<-% 0
  if(env$ref_calc < ref_calc){
    env$ref_calc = ref_calc
    if(length(ref_es) == 0){
      showNotification(p('No electrode(s) selected'), type = 'error')
    }else{
      # check conditions if we need to create reference
      old_files = list.files(env$ref_dir, pattern = 'ref_.*\\.h5')
      old_files = str_split_fixed(old_files, '(ref_)|(\\.h5)', 3)[,2]
      new_file = rave:::deparse_selections(ref_es)
      if(new_file %in% old_files){
        showNotification(p('Reference [ref_', new_file, '.h5] already exists.'), type = 'message')
      }else{
        gen_reference(ref_es)
      }
    }
  }

  updateActionButton(session, inputId = ns('ref_calc'), label = ref_calc_label)
}
get_refs = function(){
  dirs = module_tools$get_subject_dirs()
  refs = list.files(file.path(dirs$channel_dir, 'reference'), pattern = '^ref_.*\\.h5$')
  if(!length(refs)){
    return(list())
  }
  es = str_split_fixed(refs, '(ref_)|(\\.h5)', n = 3)[,2]
  re = lapply(es, rave:::parse_selections)
  names(re) = 'ref_' %&% es
  re
}


session$output[[ns('export_table')]] <- DT::renderDataTable({
  if(is.data.frame(local_data$ref_tbl)){
    local_data$ref_tbl
  }
})

observe({
  val = session$input[[ns('ref_export_name')]]
  val %?<-% 'default'
  val = str_replace_all(val, '\\W', '')
  val = str_to_lower(val)
  val = 'Reference Table Name: (reference_' %&% val %&% '.csv)'
  updateTextInput(session, ns('ref_export_name'), label = val)
}, env = ..runtime_env, label = 'OBSERVER (ref_export_name)')

observeEvent(session$input[[ns('do_export')]], {
  # get ref_table
  ref_tbl = get_ref_table()
  dirs = subject$dirs
  fname = session$input[[ns('ref_export_name')]]
  fname %?<-% 'default'
  fname = str_replace_all(fname, '\\W', '')
  fname = str_to_lower(fname)
  fname = 'reference_' %&% fname %&% '.csv'
  fpath = file.path(dirs$meta_dir, fname)
  rave:::safe_write_csv(data = ref_tbl, file = fpath, row.names = F)
  showNotification(p('Reference table [', fname, '] exported.'), type = 'message')
}, event.env = ..runtime_env, handler.env = ..runtime_env)

# Rave Execute
rave_execute({
  # Part 1: Load or new reference scheme
  load_reference()


  # Part 2: show a specific group
  local_data$refresh = Sys.time()

  # Check if table needs to be saved
  cur_group_save %?<-% 0
  env$cur_group_save %?<-% 0
  if(env$cur_group_save < cur_group_save){
    # Save table
    local_data$ref_tbl = get_ref_table()
    showModal(
      shiny::modalDialog(
        title = 'Export Reference Table',
        size = 'l',
        easyClose = T,
        footer = fluidRow(
          div(
            class = 'col-md-4 col-md-push-8 col-sm-12',
            textInput(ns('ref_export_name'), 'Reference Name: ', value = 'default', placeholder = 'File name for reference table')
          ),
          column(
            width = 12L,
            modalButton('Cancel'),
            actionButton(ns('do_export'), 'Export')
          )
        ),
        DT::dataTableOutput(ns('export_table'))
      )
    )
  }
  env$cur_group_save = cur_group_save



  # Part 3: prepare electrodes to be calcumated (mean)
  check_reference()
})


# Output - visualizations
console = function(){
  print(reactiveValuesToList(session$input))
}



# Debug
if(FALSE){
  m = ModuleEnvir$new(module_id = 'mid', 'ref', script_path = './inst/modules/builtin_modules/reference/main.R'); init_app(m)
  ns = shiny::NS('mid')

  # Post
  execenv = m$private$exec_env$nBjmfUpDRJdQrKzEX87y
  execenv$private$inputs
  ref_group = execenv$param_env$ref_group

  session = execenv$static_env$session


  e = environment(execenv$static_env$console)

}
