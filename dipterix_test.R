require(shiny)
require(plotly)
require(tidyverse)
require(magrittr)
require(stringr)
require(fields)
require(uuid)  # install.packages('uuid')

if(!require(rhdf5)){
  source("https://bioconductor.org/biocLite.R")
  biocLite("rhdf5")
}

if(!require(HDF5Array)){
  source("https://bioconductor.org/biocLite.R")
  biocLite("HDF5Array")
}
reset_rave(); gc()
options(fffinonexit=FALSE)
rave_opts$set_options(
  # data_dir = '/Users/beauchamplab/Dropbox/rave (1)/data',
  # data_dir = '/Users/beauchamplab/Dropbox/rave/data',
  data_dir = '../../rafe/data/',
  module_lookup_file = './inst/modules_dev.csv',
  debug = 'FALSE',
  delay_input = '20',
  suma_parallel_cores = '7',
  batch_bytes = '2000000000', # 5G swap-RAM cache
  use_rhdf5 = 'FALSE',
  suma_path = '/Users/beauchamplab/abin'
)

future::plan(future::multicore, workers = 100)

# future::plan(future::sequential)
suma = rsuma()
init_app(options = list(
  launch.browser = T,
  # host = '10.28.1.155',
  port = 8080
), suma = suma)


#########
subject_ids = find_all_subjects(); subject_ids
subject_id = subject_ids[1]
subject = get_subject(subject_id)

attach_virtualenv(subject_id, 1:2)
data = data_env$data


freq_lim = c(50, 120)
freq = subject$frequencies$Frequency
sel = freq >= freq_lim[1] & freq <= freq_lim[2]

baseline_time = subject$time_points$Time <=0 & subject$time_points$Time > -0.5
stimulus = str_c(subject$trials$Stimulus, '_', subject$trials$Type)
uni_stimulus = unique(stimulus)

do_for_electrode(data.frame(
  subject = subject_id,
  electrodes = 1:240),
function(data){
  print(Sys.time())
  apply(data[,sel,,], 1, colSums) %>%
    t ->
    collapsed
  base_sig = rowMeans(collapsed[, baseline_time])
  bld = (collapsed / base_sig - 1) * 100
  # collapse by stimulus and controls
  re = NULL
  for(s in uni_stimulus){
    sel = stimulus == s
    r = colMeans(bld[sel, ])
    re = rbind(re, r)
  }
  return(re)
}, quiet = F, simplify = F) ->
  collapsed


X = NULL; Y = NULL
for(i in 1:nrow(collapsed)){
  X = rbind(X, collapsed$result[[i]])
  Y = rbind(Y, data.frame(
    electrode = collapsed$electrode[[i]],
    stimulus = uni_stimulus
  ))
}


save.image('../../rafe_cache.RData')

X[X > 200] = 200


# Only select 0-1s, i.e. 101 - 200
X = X[, 101:200]
image.plot(X)

require(kohonen)


g = somgrid(5, 5, topo = 'rectangular')
m = supersom(X, grid = g)
plot(m, type = 'counts')
plot(m)



