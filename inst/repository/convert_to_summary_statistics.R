# convert to summary statistics

convert <- function(elec) {

  # require(magrittr)
  # load(paste0(elec, '/',  'electrode.RData'), e <- new.env())
  # colSD <- function(x) apply(x, 2, sd)
  # colN <- function(x) apply(x, 2, length)
  # double_colMean <- function(m) apply(m, 2, colMeans)
  # double_colSD <- function(m) apply(m, 2, colSD)
  # double_colN <- function(m) apply(m, 2, colN)
  #
  # stimuli=unique(e$CACHED_OBJ$trials$stimuli) %>% sort
  # content <- lapply(stimuli, function(st) {
  #   ind <- e$CACHED_OBJ$trials$stimuli == st
  #   list(mu=double_colMean(e$CACHED_OBJ$content[ind,,]) %>% t,
  #        sd=double_colSD(e$CACHED_OBJ$content[ind,,]) %>% t,
  #        N=double_colN(e$CACHED_OBJ$content[ind,,]) %>% t
  #   )
  # })
  # content %>% str
  # content %<>% set_names(stimuli)
  # electrode_ss <- list(content=content)
  # electrode_ss$frequencies <- e$CACHED_OBJ$frequencies
  # electrode_ss$time <- e$CACHED_OBJ$time
  # electrode_ss$channel <- e$CACHED_OBJ$channels[elec]
  #
  # save(electrode_ss, file=paste0(elec, '/electrode_collapsed.RData'))
}

if(FALSE) {
  setwd('~/Dropbox/MultisensoryIntegration/ecog/rafe/data/subject_APX1604/ecog/cache/load_ecog_raw/')
  fnames <- 1:240

  for(ii in fnames) {
      file.remove(paste0(ii, '/electrode_collapsed.Rdata'))
  }

  require(parallel)
  cl <- makeForkCluster(4)
  parSapply(cl, fnames, convert)
}

