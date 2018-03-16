#
# as_ff <- function(value, directory = './', name = NULL, readonly = FALSE, ...){
#   if(is.null(name)){
#     name <- as.character(substitute(value))
#   }
#   requireNamespace('ff')
#   if(!file.exists(getOption('fftempdir'))){
#     dir.create(getOption('fftempdir'), recursive = T)
#   }
#   # make sure when we close ff file, it won't be deleted
#   options(fffinonexit = FALSE)
#
#   if(!file.exists(directory)){
#     dir.create(directory, recursive = T)
#   }
#
#   x <- ff::as.ff(value, finalizer = 'close', ...)
#   ff::close.ff(x)
#   # ..setup_env$ff_pointers$add(x)
#   return(save_ff(name, x, directory, readonly = readonly))
# }
#
# load_ff = function(name, directory = NULL, readonly = FALSE){
#   requireNamespace('ff')
#   if(!file.exists(getOption('fftempdir'))){
#     dir.create(getOption('fftempdir'), recursive = T)
#   }
#   # make sure when we close ff file, it won't be deleted
#   options(fffinonexit = FALSE)
#
#   names <- load(file.path(directory, sprintf('%s.RData', name)), envir = environment())
#   # only look at first one because only one variable is inside (supposed)
#   x <- get(names[1], envir = environment())
#   ff::close.ff(x)
#   bit::physical(x)$filename <- normalizePath(file.path(directory, sprintf('%s.%s', name, getOption('ffextension', 'ff'))))
#   bit::physical(x)$readonly <- readonly
#   bit::physical(x)$finonexit <- FALSE
#
#   # add to pointer list
#   # ..setup_env$ff_pointers$add(x)
#   return(x)
# }
#
#
# save_ff <- function(name, x, directory, readonly = FALSE){
#   # make sure when we close ff file, it won't be deleted
#   options(fffinonexit = FALSE)
#
#   # close file so that we can move the file
#   ff::close.ff(x)
#
#   # get physical paths
#   path_from <- bit::physical(x)$filename
#   path_from = normalizePath(path_from)
#
#   path_to <- file.path(directory, sprintf('%s.%s', name, getOption('ffextension', 'ff')))
#
#   # move
#   if(file.exists(path_to)){
#     path_to = normalizePath(path_to)
#   }
#   if(path_to != path_from){
#     logger('Saving ff file to ', path_to)
#     file.copy(path_from, path_to)
#     unlink(path_from)
#   }
#
#   # re-assign path
#   # tools has inner method that can get absolute path
#   # However, if file does not exists, error will b raised (if file not moved, error!)
#   bit::physical(x)$filename <- normalizePath(path_to)
#   bit::physical(x)$readonly <- readonly
#   bit::physical(x)$finonexit <- FALSE
#
#
#   assign(name, x, envir = environment())
#   # ..setup_env$ff_pointers$add(x)
#   save(list = name, file = file.path(directory, sprintf('%s.RData', name)), envir = environment())
#   return(get(name, envir = environment()))
# }
#
# # b = as_ff(1:100, name = 'a', directory = './tmp')
# # c = load_ff(name = 'b', directory = './tmp')
# # bit::physical(b$a)$filename <- "/Users/beauchamplab/Dropbox/rave/data/subject_lij118_ChBr/ecog/cached/0"
#
# # d = rave:::load_ff('data_1', "/Users/beauchamplab/Dropbox/rave/data/subject_lij118_ChBr/ecog/cached/")
#
