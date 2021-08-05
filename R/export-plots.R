#' Export voltage (analog trace) diagnostic plots for each electrode
#' @description You must import subject through \code{rave_preprocess()} and 
#' then run this function
#' @param subject character with the following format: 
#' \code{"project_name/subject_code"}
#' @param electrodes a integer vector. For example: \code{c(1,3,4,10:15)}
#' @param blocks the blocks to include. Default is all blocks of this subject
#' @param save_dir the directory you want to save the files
#' @param winlen window length, default is twice of the subject sampling rate
#' @param freq_lim default is half of voltage sampling rate
#' @param nclass number of classes in histogram plot, default is 50
#' @param fore_col Periodogram color for Notch filtered signals
#' @param back_col Periodogram color for raw signals
#' @param width,height,useDingbats passed to \code{\link[grDevices]{pdf}}
#' @param onefile collect images within one file?
#' @param ... All other parameters passed to \code{\link[grDevices]{pdf}} 
#' @seealso \code{\link[grDevices]{pdf}}, \code{\link{diagnose_signal}}, 
#' \code{\link[rave]{pwelch}}
#' @export
export_diagnose_voltage = function(
  subject, electrodes, blocks, save_dir = './export', width = 12, height = 7, 
  useDingbats = F, onefile = T, winlen, freq_lim, nclass = 50, 
  fore_col = 'black', back_col = 'grey80', ...
){
  subject = stringr::str_split_fixed(subject, '/', 2)
  # load preprocess info
  utils = rave_preprocess_tools()
  utils$check_load_subject(subject_code = subject[2], project_name = subject[1])
  if(utils$notch_filtered()){
    has_notch = T
  }else{
    has_notch = F
  }
  
  blocks %?<-% utils$get_blocks()
  srate = utils$get_srate()
  winlen %?<-% (2 * srate)
  freq_lim %?<-% (srate / 2)
  
  progress = progress('Generating Diagnostic Plots for Voltage', max = length(electrodes))
  on.exit({progress$close()})
  
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  
  for(e in electrodes){
    file = file.path(save_dir, sprintf('Voltage_Diagnose_%d.pdf', e))
    grDevices::pdf(file = file, width = width, height = height, useDingbats = useDingbats, ..., onefile = onefile)
    tryCatch({
      progress$inc(sprintf(' - Electrode %d', e))
      raw_volt = utils$load_voltage(electrodes = e, blocks = blocks, raw = T)
      if(has_notch){
        notch_volt = utils$load_voltage(electrodes = e, blocks = blocks, raw = F)
      }
      
      for(b in blocks){
        if(has_notch){
          s1 = notch_volt[[b]]
          s2 = raw_volt[[b]]
          name = 'Notch'
          col = c(fore_col, 'grey80')
          main = sprintf('Notch Filtered Signal - Block: %s, Electrode: %d', b, e)
        }else{
          s1 = raw_volt[[b]]
          s2 = NULL
          name = 'Raw'
          col = fore_col
          main = sprintf('Raw Voltage Signal - Block: %s, Electrode: %d', b, e)
        }
        
        
        diagnose_signal(
          s1 = s1, s2 = s2, col = col,
          name = name,
          max_freq = freq_lim,
          srate = srate,
          window = winlen,
          noverlap = winlen / 2,
          nclass = nclass,
          cex = 2,
          std = 3,
          lwd = 0.3,
          main = main
        )
      }
      
    }, error = function(err){
      catgl('Some error for the electrode {e}', level = 'ERROR')
    })
    grDevices::dev.off()
  }
  
  invisible()
}
