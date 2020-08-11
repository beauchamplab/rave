# try_save_file <- function(data, ..., fpath, name, append = F) {
#   postfix = utils::tail(stringr::str_to_lower(as.vector(stringr::str_split(
#     fpath, '\\.', simplify = T
#   ))), 1)
#   switch (
#     postfix,
#     'csv' = {
#       utils::write.csv(data, fpath, ..., append = append)
#     },
#     'h5' = {
#       args = list(...)
#       ctype = args[['ctype']]
#       ctype %?<-% storage.mode(data)
#       save_h5(
#         data,
#         fpath,
#         'data',
#         ...,
#         new_file = !append,
#         replace = !append,
#         ctype = ctype
#       )
#     },
#     'rdata' = {
#       nms = names(list(...))
#       nms = nms[!nms == '']
#       if (!missing(data)) {
#         nms = c(nms, 'data')
#       }
#       env = new.env()
#       if (append && file.exists(fpath)) {
#         base::load(fpath, envir = env)
#       }
#       if (length(nms)) {
#         nms = unique(nms)
#         for (nm in nms) {
#           env[[nm]] = get(nm, envir = environment(), inherits = TRUE)
#         }
#         base::save(list = nms,
#                    envir = env,
#                    file = fpath,
#                    ...)
#       }
#     },
#     'rds' = {
#       base::saveRDS(data, fpath, ...)
#     },
#     'mat' = {
#       args = c(list(con = fpath, name = data),
#                list(...))
#       names(args)[2] = name
#       
#       do.call(R.matlab::writeMat,
#               args = args)
#     }
#   )
# }
# 
# try_load_file <- function(fpath, name, ..., env = new.env(parent = emptyenv()), simplify = TRUE) {
#   if (!file.exists(fpath)) {
#     return(NULL)
#   }
#   file_info = file.info(fpath)
#   if (file_info[['isdir']]) {
#     return(NULL)
#   }
#   
#   postfix = utils::tail(stringr::str_to_lower(as.vector(stringr::str_split(
#     fpath, '\\.', simplify = T
#   ))), 1)
#   
#   
#   if(simplify){
#     env = new.env(parent = emptyenv())
#   }
#   
#   switch (
#     postfix,
#     'csv' = {
#       env$data = utils::read.csv(file = fpath, ...)
#     },
#     'h5' = {
#       env$data = load_h5(file = fpath, name = 'data', ...)
#     },
#     'rdata' = {
#       base::load(file = fpath, envir = env, ...)
#     },
#     'rds' = {
#       env$data = base::readRDS(file = fpath, ...)
#     },
#     'mat' = {
#       env$data = R.matlab::readMat(con = fpath, ...)
#     }
#   )
#   
#   data = as.list(env, all.names = TRUE)
#   
#   if(simplify){
#     if(length(data) == 1){
#       return(data[[1]])
#     }
#     if(length(data) == 0){
#       return(NULL)
#     }
#   }
#   
#   return(data)
# }
