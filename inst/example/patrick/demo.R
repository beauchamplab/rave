# 1. Create your own settings

rafe_opts$load_settings(conf_path = './inst/example/patrick/settings.conf')

# rafe_opts$load_settings(conf_path = './inst/example/patrick/settings_macbook.conf')

### 2 Load R packages
require(shiny)
require(plotly)
require(tidyverse)
require(magrittr)
require(stringr)
require(fields)
require(uuid)

if(!require(rhdf5)){
  source("https://bioconductor.org/biocLite.R")
  biocLite("rhdf5")
  require(rhdf5)
}

if(!require(HDF5Array)){
  source("https://bioconductor.org/biocLite.R")
  biocLite("HDF5Array")
  require(HDF5Array)
}

# To installl R packages, for e.g., "uuid"
# install.packages('uuid')

# To lauch app
init_app()

### 3 Subject information (meta)

# find subjects
find_subject_ids()
subject = get_subject(subject_id = 'subject_lij118_ChBr')
subject$electrodes
subject$trials
subject$time_points
subject$frequencies


### 3 Do analysis for each electrodes selected
plot_images = function(
  D, main = '', col = fields::tim.colors(64),
  xlab = 'x', ylab = 'y',

  xrange = function(len){
    seq(0, 1, length.out = len)
  },
  yrange = function(len){
    seq(0, 1, length.out = len)
  }){
  image(
    D, main = main, col = col, axes=FALSE, xlab = xlab, ylab = ylab
  )
  axis(1,axTicks(1),lab=xrange(length(axTicks(1))))
  axis(2,axTicks(2),lab=yrange(length(axTicks(2))))

  fields::image.plot(
    D, axes=FALSE, legend.only = T, col = col
  )
}
# method 1
par(mfrow = c(1,3))
list(subject = 'subject_lij118_ChBr', electrodes = 71:73) %for_e% {
  # variable "data" is electrode data
  # data is Trial x Freq x Time x Electrode (1)
  freq = subject$frequencies$Frequency
  plot_images(t(data[1, freq > 50 & freq < 150, , ]), xlab = 'Time', ylab = 'Freq', xrange = function(len){
    seq(0, max(subject$time_points$Time), length.out = len)
  }, yrange = function(len){
    seq(50, 150, length.out = len)
  })
}


### 4 matlab

# Specify matlab_path in rafe_opts
system2(
  command = 'matlab',
  args = sprintf("-nodesktop -nosplash -r 'cmd_hopca_cptpa_ecog'"),
  env = c(
    sprintf("PATH=$PATH:%s", rafe_opts$get_options('matlab_path')),
    sprintf("MATLABPATH=%s:%s",
            tools::file_path_as_absolute(rafe_opts$get_options('temp_dir')),
            tools::file_path_as_absolute('./inst/modules/RHOP/')
    )
  )
)



### 5 Simple module



### 6 What if your algorithm take long time to run















