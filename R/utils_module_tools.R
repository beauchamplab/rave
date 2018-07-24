#' Tools for module writers
#' @export
rave_module_tools <- function(env = NULL, data_env = NULL, quiet = FALSE) {
  if(!is.environment(data_env)){
    data_env = getDefaultDataRepository()
  }
  tools = new.env()

  local({
    ####### part 1: Data ######
    is_loaded = function(data_type){
      repo = data_env$.private$repo
      on.exit(rm(repo))
      !is.null(repo[[data_type]])
    }


    get_power = function(force = T, referenced = T) {
      repo = data_env$.private$repo
      on.exit(rm(repo))
      nm = ifelse(referenced, 'power', 'raw_power')
      if(force && is.null(repo[[nm]])){
        epoch_name = data_env$.private$meta$epoch_info$name
        time_range = data_env$.private$meta$epoch_info$time_range
        repo$epoch(
          epoch_name = epoch_name,
          pre = time_range[1],
          post = time_range[2],
          electrodes = data_env$preload_info$electrodes,
          data_type = 'power',
          referenced = referenced
        )
      }
      return(repo[[nm]])
    }

    get_phase = function(force = T, referenced = T){
      repo = data_env$.private$repo
      on.exit(rm(repo))
      nm = ifelse(referenced, 'phase', 'raw_phase')
      if(force && is.null(repo[[nm]])){
        epoch_name = data_env$.private$meta$epoch_info$name
        time_range = data_env$.private$meta$epoch_info$time_range
        repo$epoch(
          epoch_name = epoch_name,
          pre = time_range[1],
          post = time_range[2],
          electrodes = data_env$preload_info$electrodes,
          data_type = 'phase',
          referenced = referenced
        )
      }
      return(repo[[nm]])
    }

    get_voltage = function(force = T, referenced = T){
      repo = data_env$.private$repo
      on.exit(rm(repo))
      nm = ifelse(referenced, 'volt', 'raw_volt')
      if(force && is.null(repo[[nm]])){
        epoch_name = data_env$.private$meta$epoch_info$name
        time_range = data_env$.private$meta$epoch_info$time_range
        repo$epoch(
          epoch_name = epoch_name,
          pre = time_range[1],
          post = time_range[2],
          electrodes = data_env$preload_info$electrodes,
          data_type = 'volt',
          referenced = referenced
        )
      }
      return(repo[[nm]])
    }

    get_voltage2 = function(){

      if(is.null(data_env$.private[['volt_unblocked']])){
        blocks = data_env$subject$preprocess_info('blocks')
        dirs = data_env$subject$dirs
        electrodes = data_env$subject$electrodes$Electrode

        print(sys.on.exit())

        progress = progress('Prepare preprocess voltage', max = length(electrodes) + 1)

        lapply_async(electrodes, function(e){
          sapply(blocks, function(b){
            f = file.path(dirs$channel_dir, 'voltage', sprintf('%d.h5', e))
            load_h5(f, '/raw/voltage/' %&% b, ram = T)
          }, simplify = F, USE.NAMES = T)
        }, .call_back = function(i){
          progress$inc(sprintf('Loading voltage data - %d', electrodes[i]))
        }) ->re

        progress$inc('Finalizing...')

        data_env$.private[['volt_unblocked']] = new.env()
        r = sapply(blocks, function(b) {
            l = list()
            l[electrodes] = lapply(re, function(comp) {
              comp[[b]]
            })
          }, simplify = F, USE.NAMES = T)

        list2env(r, envir = data_env$.private[['volt_unblocked']])
        progress$close()
        rm(list = ls(), envir = environment())
      }


      data_env$.private[['volt_unblocked']]
    }

    clean = function(items = c('raw_volt', 'raw_phase', 'raw_power')){
      for(i in items){
        data_env$.private$repo[[i]] = NULL
      }
      gc()
    }

    get_meta = function(name) {
      meta = data_env$.private$meta
      switch (
        name,
        'electrodes' = {
          meta$electrode
        },
        'frequencies' = {
          meta$frequency
        },
        'time_points' = {
          meta$time
        },
        'trials' = {
          meta$epoch_data
        }
      )
    }

    get_subject_dirs = function() {
      data_env$subject$dirs
    }

    get_subject_data = function(
      name,
      path = 'common',
      ...,
      default = NULL,
      try_open = T,
      check_cache = T
    ) {

      if(check_cache){
        val = data_env$.module_data[[path]][[name]]
        if(!is.null(val)){
          return(val)
        }
      }

      root_dir = data_env$subject$dirs$module_data_dir
      path = file.path(root_dir, path)
      if (!dir.exists(path)) {
        dir.create(path, recursive = T, showWarnings = F)
      }
      file = list.files(path,
                        pattern = sprintf('^%s\\.', name),
                        full.names = T)
      if(try_open){
        if (length(file)) {
          data = default
          try({
            data = try_load_file(file[[1]], name, ..., simplify = T)
            return(data)
          })
        }
        return(NULL)
      }else{
        return(file)
      }
    }

    get_loaded_electrodes = function() {
      repo = data_env$.private$repo
      on.exit(rm(repo))
      e = repo$raw$keys()
      e = as.numeric(e)
      e = e[!is.na(e)]
      sort(e)
    }

    save_subject_data = function(data,
                                 name,
                                 ...,
                                 path = 'common',
                                 format = 'rds') {
      format = str_to_lower(format)
      assertthat::assert_that(format %in% c('rds', 'h5', 'rdata', 'csv', 'mat'),
                              msg = 'Unsupported format: MUST be rds, h5, rdata, csv, or mat')
      root_dir = data_env$subject$dirs[['rave_dir']]
      path = file.path(root_dir, 'module_data', path)
      if (!dir.exists(path)) {
        dir.create(path, recursive = T, showWarnings = F)
      }
      file = list.files(path,
                        pattern = sprintf('^%s\\.', name),
                        full.names = T)
      if (length(file)) {
        append = list(...)[['append']]
        append %?<-% FALSE
        if (!append) {
          lapply(file, function(f) {
            unlink(x = f)
          })
        }
      }

      file = file.path(path, sprintf('%s.%s', name, format))

      try_save_file(data = data,
                    name = name,
                    fpath = file,
                    ...)
    }


    get_sample_rate = function(original = F){
      if(original){
        return(data_env$.private$preproc_tools$get_srate())
      }else{
        return(data_env$subject$sample_rate)
      }
    }

    ###### Part 2: utilities #######
    get_valid_electrodes = function(electrodes = seq_len(10000)){
      data_env[['subject']]$filter_valid_electrodes(electrodes = electrodes)
    }

    baseline = function(from, to, electrodes = NULL, ...){
      repo = data_env$.private$repo
      on.exit(rm(repo))
      data_env$.private$repo$baseline(from = from, to = to, electrodes = electrodes, ...)
    }

    reload = function(epoch, epoch_range, reference, electrodes){
      has_change = F
      if(missing(electrodes)){
        electrodes = data_env$preload_info$electrodes
      }else{
        has_change = T
      }
      if(missing(epoch)){
        epoch = data_env$preload_info$epoch_name
      }else{
        has_change = T
      }
      if(missing(epoch_range)){
        epoch_range = range(data_env$preload_info$time_points)
        epoch_range = abs(epoch_range)
      }else{
        has_change = T
      }
      if(missing(reference)){
        reference = data_env$preload_info$reference_name
      }else{
        has_change = T
      }

      rave::rave_prepare(
        subject = data_env$subject$subject_id,
        electrodes = electrodes,
        epoch = epoch,
        time_range = epoch_range,
        reference = reference,
        attach = F,
        data_types = NULL
      )

      group = 'main_app'
      last_entry('electrodes', electrodes, save = T, group = group)
      last_entry('epoch', epoch, save = T, group = group)
      last_entry('epoch_range', epoch_range, save = T, group = group)

      # execenv$reloadUI()
      # global_reactives$force_refresh_all = Sys.time()
      # global_reactives$has_data = Sys.time()
    }

    ###### Part 3: Visualization ######
    plot_3d_electrodes = function(
      tbl = NULL,
      electrodes,
      values = NULL,
      key_frame = NULL,
      marker = NULL,
      link_module = NULL,
      variable_name = 'electrode',
      link_text = 'View Electrode',
      palette = colorRampPalette(c('navy', 'grey', 'red'))(1001),
      symmetric = T,
      fps = 5,
      control_gui = F,
      loop = T,
      radiu_normal = 5,
      radiu_minis = 2,
      ...
    ){
      "marker MUST be NULL or have length of electrodes, or of nrow(tbl)"
      "values can be MULL, vector or matrix, either numbers or colors/namedcolors"
      "if values is a matrix, then ncol(values)=length(electrodes), or if vector"
      "length(values)=length(electrodes). Rows of values will be used to generate animation"


      tbl %?<-% data_env$subject$electrodes
      tbl = as.data.frame(tbl, stringsAsFactors = F)
      tbl$Label[is.na(tbl$Label)] = ''
      tbl$Name = stringr::str_trim(sprintf('Electrode %d %s', tbl$Electrode, tbl$Label))
      tbl$Radius = radiu_normal
      tbl$Radius[stringr::str_detect(tbl$Group, '^([Ee]pi)|([Mm]ini)')] = radiu_minis
      tbl$active = 0
      tbl$Marker = tbl$Name

      if(!missing(electrodes) && length(electrodes)){
        electrodes = electrodes[electrodes %in% tbl$Electrode]

        tbl$active = tbl$Electrode %in% electrodes
        ind = sapply(electrodes, function(e){which(tbl$Electrode == e)})
        tbl$active[ind] = seq_along(ind)


        # Markers
        marker %?<-% tbl$Name[ind]
        assertthat::assert_that(length(marker) %in% c(length(electrodes), nrow(tbl)), msg = "marker MUST be NULL or have length of electrodes, or of nrow(tbl)")
        # Add links to Marker if indicated
        # TODO


        if(length(marker) == length(electrodes)){
          tbl$Marker[ind] = marker
        }else{
          tbl$Marker = marker
        }


        # Values
        values %?<-% rep(0, length(electrodes))
        if(is.vector(values)){
          values = matrix(values, nrow = 1)
        }
        assertthat::assert_that(ncol(values) == length(electrodes), msg = "values MUST either be NULL or ncol(values) == length(electrodes)")

        # Assign colors
        if(is.numeric(values)){
          res = length(palette)
          rg = range(values)
          if(rg[1] == rg[2]){
            values = array(palette[floor((res+1)/2)], dim = dim(values))
          }else{
            if(symmetric){
              b = 0
              a = (res-2) / 2 / max(abs(rg))
              c = (res + 1) / 2
            }else{
              b = rg[1]
              a = (res-1) / (rg[2]-rg[1])
              c = 1
            }
            col_ind = floor(a * (values - b) +c)
            col_ind[col_ind < 1] = 1
            col_ind[col_ind > res] = res
            values = matrix(palette[col_ind], ncol = length(electrodes))
          }
        }else{
          # values are characters, we assume that those are colors that needs no
        }
        # Set keyframes
        key_frame %?<-% seq_len(nrow(values))

        has_value = T
      }else{
        if(length(marker) == nrow(tbl)){
          tbl$Marker = marker
        }
        has_value = F
      }

      n = nrow(tbl)

      lapply(seq_len(n), function(ii){
        row = tbl[ii, ]
        g = with(row, {
          threejsr::GeomSphere$new(
            position = c(Coord_x, Coord_y, Coord_z),
            mesh_name = Name,
            mesh_info = Marker,
            radius = Radius
          )

        })

        if(!is.null(link_module)){
          g$extra_data(
            text = link_text,
            module_id = link_module,
            variable_name = variable_name,
            value = row$Electrode
          )
        }

        if(row$active > 0){
          # has value
          sapply(values[,row$active], function(v){
            get_color(col = v, alpha = T) / 255
          }) ->
            col
          colnames(col) = NULL
          col = t(col)

          g$animation_event(
            name = 'ani',
            event_data = col,
            key_frames = key_frame,
            loop = loop,
            pixel_size = 4
          )
        }

        g

      }) ->
        geoms

      threejsr:::threejs_scene.default(
        elements = geoms,
        fps = fps,
        control_gui = control_gui,
        ...
      )
    }

  }, envir = tools)



  # If env is provided, create active binds
  if(is.environment(env) && !environmentIsLocked(env)){

    makeActiveBinding('module_tools', function(){
      tools
    }, env)

    makeActiveBinding('subject', function(){
      data_env$subject
    }, env)

    makeActiveBinding('data_check', function(){
      data_env$data_check
    }, env)

    makeActiveBinding('preload_info', function(){
      data_env$preload_info
    }, env)
  }

  return(tools)


}

#' @import stringr
#' @export
try_save_file <- function(data, ..., fpath, name, append = F) {
  postfix = tail(str_to_lower(as.vector(str_split(
    fpath, '\\.', simplify = T
  ))), 1)
  switch (
    postfix,
    'csv' = {
      utils::write.csv(data, fpath, ..., append = append)
    },
    'h5' = {
      args = list(...)
      ctype = args[['ctype']]
      ctype %?<-% storage.mode(data)
      rave::save_h5(
        data,
        fpath,
        'data',
        ...,
        new_file = !append,
        replace = !append,
        ctype = ctype
      )
    },
    'rdata' = {
      nms = names(list(...))
      nms = nms[!nms == '']
      if (!missing(data)) {
        nms = c(nms, 'data')
      }
      env = new.env()
      if (append && file.exists(fpath)) {
        base::load(fpath, envir = env)
      }
      if (length(nms)) {
        nms = unique(nms)
        for (nm in nms) {
          env[[nm]] = get(nm, envir = environment(), inherits = T)
        }
        base::save(list = nms,
                   envir = env,
                   file = fpath,
                   ...)
      }
    },
    'rds' = {
      base::saveRDS(data, fpath, ...)
    },
    'mat' = {
      args = c(list(con = fpath, name = data),
               list(...))
      names(args)[2] = name

      do.call(R.matlab::writeMat,
              args = args)
    }
  )
}

#' @import stringr
#' @export
try_load_file <- function(fpath, name, ..., env = new.env(parent = emptyenv()), simplify = T) {
  if (!file.exists(fpath)) {
    return(NULL)
  }
  file_info = file.info(fpath)
  if (file_info[['isdir']]) {
    return(NULL)
  }

  postfix = tail(str_to_lower(as.vector(str_split(
    fpath, '\\.', simplify = T
  ))), 1)


  if(simplify){
    env = new.env(parent = emptyenv())
  }

  switch (
    postfix,
    'csv' = {
      env$data = utils::read.csv(file = fpath, ...)
    },
    'h5' = {
      env$data = rave::load_h5(file = fpath, name = 'data', ...)
    },
    'rdata' = {
      base::load(file = fpath, envir = env, ...)
    },
    'rds' = {
      env$data = base::readRDS(file = fpath, ...)
    },
    'mat' = {
      env$data = R.matlab::readMat(con = fpath, ...)
    }
  )

  data = as.list(env, all.names = T)

  if(simplify){
    if(length(data) == 1){
      return(data[[1]])
    }
    if(length(data) == 0){
      return(NULL)
    }
  }

  return(data)
}
