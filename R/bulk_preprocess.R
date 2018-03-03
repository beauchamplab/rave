# bulk_notch <- function(project_name, subject_code, blocks, channels, ncores = future::availableCores() - 2, ...){
#   process_name = 'notch'
#
#   parallel::mclapply(channels, function(chl){
#     for(block_num in blocks){
#       preprocess(process_name = process_name,
#                  project_name = project_name,
#                  subject_code = subject_code,
#                  block_num = block_num, chls = chl, ...)
#     }
#   }, mc.cores = ncores)
#
#   return(invisible())
# }


# bulk_CAR <- function(
#   project_name, subject_code, blocks, channels, ...
# ){
#   process_name = 'CAR'
#   re = list()
#   for(block_num in blocks){
#     ss = preprocess(process_name = process_name,
#                project_name = project_name,
#                subject_code = subject_code,
#                block_num = block_num,
#                chls = channels, ...)
#     re[[paste(block_num)]] = ss
#   }
#
#   return(invisible(re))
# }


# bulk_wavelet <- function(
#   project_name, subject_code, blocks, channels, data_dir = rave_options('data_dir'),
#   ncores = future::availableCores() - 2, srate = 2000,
#   frequencies = seq(4, 200, by = 4), wave_num = 7, compress = 2, ...
# ){
#   process_name = 'wavelet'
#   # This function takes long to execute, parallel is highly recommended
#   # However, simply parallel the process will cause IO error
#   # solution is to use futures package and use multiprocess
#   # Remember you can't edit one h5 file via different session in R at the same time
#   # therefore even though the process is paralleled, each single h5 file is
#   # handled in single process
#
#   # split the work load by channels
#
#   nrows = ceiling(length(channels) / ncores)
#
#   schedule = matrix(rep(NA, ncores * nrows), ncol = ncores); schedule[1:length(channels)] = channels
#
#   future::plan(future::multiprocess)
#
#   apply(schedule, 2, function(chls){
#
#
#     chls = chls[!is.na(chls)]
#     f = NULL
#     if(length(chls) > 0){
#       future::future({
#         .load_preprocess_func()
#         for(block_num in blocks){
#           preprocess(process_name = process_name, project_name = project_name,
#                      subject_code = subject_code, block_num = block_num, chls = chls,
#                      data_dir = data_dir, srate = srate, compress = compress,
#                      wave_num = wave_num, frequencies = frequencies)
#         }
#       }, globals = c(
#         '.load_preprocess_func', 'chls', 'blocks', 'process_name', 'project_name', 'subject_code',
#         'data_dir', 'srate', 'frequencies', 'wave_num', 'compress', names(list(...))
#       )) -> f
#     }
#     return(f)
#   }) ->
#     fs
#
#   check <- function(){
#     lapply(fs, future::resolved)
#   }
#
#   logger('Command sent. Wavelet is running in the background. ',
#          'If you are calling this function "check <- bulk_wavelet(...)", ',
#          'run "check()" to see if process finished. Don\'t close current session.')
#
#   return(check)
# }





# bulk_notch(project_name = 'Congruency',
#            subject_code = 'YAB',
#            blocks = c('010','011','012'),
#            channels = 1:84)
#
# allsignals = bulk_CAR(project_name = 'Congruency',
#          subject_code = 'YAB',
#          blocks = c('010','011','012'),
#          channels = (1:84)[-c(1:12, 32,64)])
#
# check = bulk_wavelet(project_name = 'Congruency',
#          subject_code = 'YAB',
#          blocks = c('008', '010','011','012'),
#          channels = (1:84)[-c(1:12, 32,64)])
