# notch filters to subjects

# needs:
# project_name='Congruency'; subject_code='YAB'; blocks = c('008', '010'); channels = 1:84; srate = 2000
# # frequencies = seq(4, 200, by = 4) wave_num = 7, compress = 2
#
#
# bulk_notch(project_name, subject_code, blocks, channels, srate, replace = F, save_plot = F, new_cache = F)


# bulk_notch <- function(
#   project_name, subject_code, blocks, channels, srate, replace = F, save_plot = T, new_cache = F, progress = NULL
# ){
#   dirs = get_dir(subject_code = subject_code, project_name = project_name,
#                  mkdirs = c('preprocess_dir', 'pre_visual_dir'))
#
#
#   channel_file = function(chl){
#     cfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))
#     return(cfile)
#   }
#
#   all_signals = list()
#
#   # Asyc plan
#   rave_setup()
#
#   for(ii in 1:length(channels)){
#     chl = channels[ii]
#     logger('Notch filter - channel: ', chl)
#     save = channel_file(chl)
#     new_file = new_cache
#     if(!replace && !new_cache && file.exists(save)){
#       next
#     }
#
#     if(!is.null(progress)){
#       progress$inc(1 / length(channels)/2, detail = sprintf('Channel - %d', ii))
#     }
#
#     for(block_num in blocks){
#       # import signal
#       signal = pre_import_matlab(subject_code, project_name, block_num, chl)
#
#
#       # Notch filter
#       filtered = notch_channel(signal, srate)
#
#       if(is.null(all_signals[[block_num]])){
#         nc = length(filtered); nr = length(channels)
#         all_signals[[block_num]] = matrix(rep(0, nc*nr), ncol = nc, nrow = nr)
#         logger(paste0(dim(all_signals[[block_num]]), collapse = 'x'), ' - ', length(filtered))
#       }
#       all_signals[[block_num]][ii,] = filtered
#
#
#       # save the filtered signal
#       chname = sprintf("notch/%s", block_num)
#       rawname = sprintf("raw/%s", block_num)
#       H5close()
#       save_h5(filtered, file = save, name = chname, ctype = 'double', chunk = 1024, replace = replace, new_file = new_file)
#       new_file = F
#       save_h5(signal, file = save, name = rawname, ctype = 'double', chunk = 1024, replace = replace)
#
#       rhdf5::H5close()
#
#       # Save notch image
#       if(save_plot){
#         async({
#           rave:::save_plot({
#             rave:::pre_inspect(
#               process_name = 'notch',
#               project_name = project_name,
#               subject_code = subject_code,
#               block_num = block_num,
#               srate = srate,
#               chls = chl,
#               details = T,
#               boundary = -1
#             )
#           }, path = file.path(dirs$pre_visual_dir, sprintf('notch_%s_ch_%d.png', block_num, chl)))
#         }, envir = environment(), plan = NULL)
#       }
#     }
#
#
#   }
#   if(save_plot){
#     logger('Notch filter - Do NOT close R session now, plots are generated in the background', level = 'INFO')
#   }
#
#   new_file = new_cache
#   all_f = file = file.path(
#     dirs$preprocess_dir, 'all_notch.h5'
#   )
#   if(!replace && !new_cache && file.exists(all_f)){
#     return()
#   }else{
#     for(block_num in blocks){
#       if(!is.null(progress)){
#         progress$inc(1 / length(blocks)/2, detail = sprintf('Cache block - %s', block_num))
#       }
#       tmp = t(apply(all_signals[[block_num]], 1, decimate_fir, q = srate / 1000))
#       all_signals[[block_num]] = tmp
#       save_h5(tmp, all_f, name = block_num, chunk = c(1, 1024), replace = replace, ctype = 'double', new_file = new_file)
#       new_file = F
#
#     }
#   }
#
#
#
# }




###################### version 2
#' @import stringr
#' @import signal
#' @import rhdf5
#' @import stringr
#' @import magrittr
#' @import future
bulk_notch2 <- function(
  project_name, subject_code, blocks, channels, srate, progress = NULL
){
  dirs = get_dir(subject_code = subject_code, project_name = project_name,
                 mkdirs = c('preprocess_dir'))
  channel_file = function(chl){
    cfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))
    return(cfile)
  }

  lapply_async(channels, function(chl){
    save = channel_file(chl)
    for(block_num in blocks){

      # import signal
      signal = pre_import_matlab(subject_code, project_name, block_num, chl)


      # Notch filter
      filtered = notch_channel(signal, srate)


      # save the original signal
      name = sprintf("raw/%s", block_num)
      save_h5(signal, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

      # save the filtered signal
      name = sprintf("notch/%s", block_num)
      save_h5(filtered, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

      # Also save CAR identical to filtered (This is a historical problem)
      # Our sequence was notch -> CAR -> wavelet
      # However, I think wavelet can be a head of CAR since conv is linear
      # If we calc ref after wavelet, I don't want to change wavelet code - too complicated
      # So instead, alloc space for CAR (Do CAR as if the refs are 0)
      name = sprintf('/CAR/%s', block_num)
      rave:::save_h5(filtered, file = save, name = name, chunk = 1024, replace = T)
      H5close()

      # save the compressed signal [2]
      name = sprintf("notch/compress2/%s", block_num)
      compressed = rave::decimate_fir(x = filtered, q = 2)
      save_h5(compressed, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

      # save the compressed signal [5]
      name = sprintf("notch/compress5/%s", block_num)
      compressed = rave::decimate_fir(x = filtered, q = 5)
      save_h5(compressed, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

      # save the compressed signal [10]
      name = sprintf("notch/compress10/%s", block_num)
      compressed = rave::decimate_fir(x = filtered, q = 10)
      save_h5(compressed, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

      # save the compressed signal [20]
      name = sprintf("notch/compress20/%s", block_num)
      compressed = rave::decimate_fir(x = filtered, q = 20)
      save_h5(compressed, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

    }
  }, .call_back = function(i){
    chl = channels[i]
    logger('Notch filter - channel: ', chl)
    if(!is.null(progress)){
      progress$inc(1 / length(channels), detail = sprintf('Channel - %d', chl))
    }
  })

}
