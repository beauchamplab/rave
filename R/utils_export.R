#' Export voltage diagnostics for electrodes
#' @description You must import subject throught rave_preprocess() and then run this function
#' @param subject "project_name/subject_code"
#' @param electrodes for example: c(1,3,4,10:15) will generate diagnose plots for 9 electrodes
#' @param blocks default is all blocks of this subject
#' @param save_dir which directory you want to export default will create a folder "exportr" in current path
#' @param winlen window length, default is 2*subject_sample_rate
#' @param freq_lim default is half of voltage sample rate, however I woould recommend: 300 Hz
#' @param nclass number of classes in histogram plot, default is 50
#' @param fore_col Notch filtered color
#' @param back_col Raw filtered color
#' @param ... All other params will be passed to \code{\link{pdf}} function
#' @seealso \code{\link{pdf}}, \code{\link{diagnose_signal}}, \code{\link{pwelch}}
#' @details Run "rave_preprocess()" first, import subject. If the subject is notch filtered, then it will compare
#' both raw and filtered signals, otherwise it will just show the raw voltage plots
#' @export
export_diagnose_voltage = function(
  subject, electrodes, blocks, save_dir = './export', width = 12, height = 7, useDingbats = F, onefile = T,
  winlen, freq_lim, nclass = 50, fore_col = 'black', back_col = 'grey80', ...
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

  dir.create(save_dir, recursive = T, showWarnings = F)


  for(e in electrodes){
    file = file.path(save_dir, sprintf('Voltage_Diagnose_%d.pdf', e))
    pdf(file = file, width = width, height = height, useDingbats = useDingbats, ..., onefile = onefile)
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
      logger('Some error for the electrode ', e, level = 'ERROR')
    })
    dev.off()
  }

  invisible()
}
