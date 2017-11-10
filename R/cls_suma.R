#' @include cls_observable.R
#' @include options.R
NULL



#' @export
SUMA <- R6::R6Class(
  'SUMA',
  inherit = Observable,

  portable = FALSE,

  public = list(
    observable = TRUE,
    update = NULL,
    status = 'Suspended',

    dyld_library_path = NULL,
    suma_path = NULL,
    suma_monitor_dir = NULL,
    suma_spec_file = NULL,
    unbuffer_path = "",
    suma_gifti_name_regex = NULL,

    export_dir = NULL, # directory to export suma niml file

    signal_from_suma = NULL,  # will be reactive value to notify rave

    monitor_list = list(),

    reset = function(session_id = NULL){
      for(fname in names(self$monitor_list)){
        if(is.null(session_id)){
          unlink(fname)
        }else{
          if(self$monitor_list[[fname]][['session_id']] == session_id){
            unlink(fname)
          }
        }

      }
    },

    launch_suma = function(subject_id, session = shiny::getDefaultReactiveDomain()){

      wd = getwd()
      on.exit({
        setwd(wd)
      })

      logger('SUMA Status: Suspended')
      self$status = 'Suspended'



      session_id = get_session_id(session)
      suma_monitor_file = file.path(self$suma_monitor_dir, sprintf('%s_%s.log', session_id, strftime(Sys.time(), '%Y%m%d_%H%M%S')))

      # Make sure the file exists so that we can get absolute path
      writeLines('', suma_monitor_file)

      suma_monitor_file = base::normalizePath(suma_monitor_file)

      self$monitor_list[[suma_monitor_file]] = list(
        subject_id = subject_id,
        session_id = session_id,
        last_chosen = c(),
        new_nodes = c()
      )

      if(stringr::str_length(self$unbuffer_path) == 0){
        command = 'suma'
        args = c()
        env = sprintf('PATH=$PATH:%s', self$suma_path)
      }else{
        command = 'unbuffer'
        args = c('suma')
        env = sprintf('PATH=$PATH:%s:%s', self$suma_path, self$unbuffer_path)
      }
      args = c(args,
        sprintf('-spec %s', file.path('..', self$suma_spec_file)),
        sprintf('> %s', suma_monitor_file)
      )

      if(self$dyld_library_path != ''){
        # or sudo fink install glib2-dev
        env = c(env,
          sprintf('DYLD_LIBRARY_PATH=%s', self$dyld_library_path)
        )
      }

      logger('Prepare for SUMA log - ', suma_monitor_file)

      logger('Launching SUMA for subject - ', subject_id)

      # Change working directory to niml outpath
      tmp_wd = file.path(
        rave_opts$get_options('data_dir'),
        subject_id,
        self$export_dir,
        'rave'
      )

      if(!file.exists(tmp_wd) || !file.info(tmp_wd)$isdir){
        unlink(tmp_wd)
        dir.create(tmp_wd)
      }

      setwd(tmp_wd)

      system2(command,
        args = args,
        env = env,
        stdout = '', stderr = '', wait = F)

      lll <<- list(command,
        args = args,
        env = env,
        stdout = '', stderr = '', wait = F)
      setwd(wd)
      logger('Resume SUMA status: Running')
      self$status = 'Running'
    },


    initialize = function(

      # Params needed to run suma -spec *.spec
      suma_path = rave_opts$get_options('suma_path'), #'/Users/beauchamplab/abin',
      dyld_library_path = rave_opts$get_options('dyld_library_path'), #'/opt/X11/lib/flat_namespace',
      suma_spec_file = rave_opts$get_options('suma_spec_file'), #'test.spec',
      unbuffer_path = rave_opts$get_options('unbuffer_path'),

      # file for fifo pipe
      suma_monitor_dir = rave_opts$get_options('suma_monitor_dir'), #'monitor.log',
      suma_gifti_name_regex = rave_opts$get_options('suma_gifti_name_regex'),

      export_dir = rave_opts$get_options('suma_export_dir')

    ){
      self$suma_path = suma_path
      self$dyld_library_path = dyld_library_path
      self$suma_spec_file = suma_spec_file
      self$unbuffer_path = unbuffer_path

      self$suma_monitor_dir <- suma_monitor_dir

      if(!file.exists(suma_monitor_dir) || !file.info(suma_monitor_dir)$isdir){
        dir.create(suma_monitor_dir, recursive = TRUE)
      }

      self$signal_from_suma <- shiny::reactiveVal()
      self$suma_gifti_name_regex = suma_gifti_name_regex
      self$export_dir = export_dir



      self$update = function(){

        for(fname in names(self$monitor_list)){
          tryCatch({
            session_id = self$monitor_list[[fname]][['session_id']]
            subject_id = self$monitor_list[[fname]][['subject_id']]
            last_chosen = self$monitor_list[[fname]][['last_chosen']]
            self$monitor_list[[fname]][['new_nodes']] = c()


            l <- readLines(fname)

            lines = which(stringr::str_detect(l, self$suma_gifti_name_regex))
            if(length(lines) > 0){
              lines = lines + 1
              stringr::str_extract_all(l[lines], '[0-9]+[\ ]*$') %>%
                unlist %>%
                as.numeric() ->
                nodes
              nodes = floor(nodes / as.numeric(rave_opts$get_options('suma_nodes_per_electrodes'))) + 1


              if(length(last_chosen) > 0){
                new_nodes = nodes[-c(1:length(last_chosen))]
              }else{
                new_nodes = nodes
              }

              if(length(new_nodes) > 0){
                self$monitor_list[[fname]][['last_chosen']] = nodes
                self$monitor_list[[fname]][['new_nodes']] = unique(new_nodes)
              }

            }

          }, error = function(e){
            traceback(e)
            unlink(fname)
            self$monitor_list[[fname]] = NULL
          })
        }


        self$signal_from_suma(self$monitor_list)
      }

      # self$check = function(){
      #   self$update()
      # }

    },
    check = function(){
      if(self$status == 'Running'){
        self$update()
      }

    },
    get_output_path = function(filename, subject_id = '', auto_create = TRUE){
      dir_path <- file.path(
        rave_opts$get_options('data_dir'),
        subject_id,
        self$export_dir,
        'rave'
      )
      if(auto_create){
        dir.create(dir_path, showWarnings = F, recursive = TRUE)
      }

      return(file.path(
        dir_path,
        filename
      ))
    },
    push_to_suma = function(suma_table, module_id = 'TMP', subject){
      nodes_per_elec <- as.numeric(rave_opts$get_options('suma_nodes_per_electrodes'))

      suma_table_1d = suma_table[rep(1:(nrow(suma_table)), each=nodes_per_elec),]

      suma_table_1d[,1] = (suma_table_1d[,1] - 1) * nodes_per_elec + seq(0, nodes_per_elec - 1)

      suma_table_1d = suma_table_1d[complete.cases(suma_table_1d),]
      module_id = stringr::str_to_upper(abbreviate(stringr::str_replace_all(module_id, '_', ' ')))

      file_name = format(Sys.time(), paste0(module_id, '_%Y%m%d_%H%M%S'))

      cmd_convert <- rave_opts$get_options('suma_to_niml')
      cmd_push <- rave_opts$get_options('suma_send_niml')
      value_file <- self$get_output_path('.tmp_value.dat', subject_id = subject$id)
      index_file <- self$get_output_path('.tmp_index.dat', subject_id = subject$id)
      niml_fname <- self$get_output_path(paste0(file_name, '.niml.dset'), subject_id = subject$id)
      export_csv <- self$get_output_path(paste0(file_name, '.csv'), subject_id = subject$id)
      tryCatch({

        write.csv(suma_table, file = export_csv, row.names = subject$electrode_label_by_index(as.numeric(suma_table[,1])))

        write.table(file=value_file, suma_table_1d[, -1],
          row.names = FALSE, col.names = FALSE)
        write.table(file=index_file, suma_table_1d[, 1],
          row.names = FALSE, col.names = FALSE)
        cmd <- sprintf(cmd_convert, value_file, index_file, niml_fname)
        logger('Firing command:\n\t', cmd)
        system(cmd, wait = TRUE)

        # cmd <- sprintf(cmd_push, niml_fname)
        # logger('Firing command:\n\t', cmd)
        # system(cmd, wait = FALSE)
      }, error = function(e){
        logger('An error was raised while pushing data to SUMA:', level = 'WARNING')
        traceback(e)
      })


      # system(paste0('~/abin/ConvertDset -o_niml -input ', tmp_file, ' -i_1D -node_index_1D ', index_file, ' -prefix ', niml_fname), wait=TRUE)
      # cmd <- paste0('~/abin/DriveSuma -com surf_cont -load_dset ', getwd(), '/', niml_fname, '.niml.dset')

      return(file_name)
    }
  )
)


#' Function to generate a simple R-SUMA adapter
#' @usage rsuma()
#' @return Returns a SUMA object, see \code{\link{SUMA}} to know more about what you can do with
#' SUMA instances.
#' @example ./inst/example/SUMA_example.R
#' @export
rsuma <- function(){
  return(SUMA$new())
}
