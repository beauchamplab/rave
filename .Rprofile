#### -- Package Autoloader -- ####
local({
  # For production
  cran_pkgs = c('pryr', 'assertthat', 'lazyeval', 'shinydashboard', 'fftw', 'yaml',
                'fftwtools', 'plotly', 'digest', 'gridBase', 'shinyjs', 'ff',
                'devtools', 'rlang', 'V8')

  cran_pkgs = cran_pkgs[!cran_pkgs %in% utils::installed.packages()[,1]]
  if(length(cran_pkgs)){
    message("Installing Dependencies from CRAN...")
    install.packages(cran_pkgs, repos='http://cran.us.r-project.org')
  }

  bioc_pkgs = c('HDF5Array', 'rhdf5')
  bioc_pkgs = bioc_pkgs[!bioc_pkgs %in% utils::installed.packages()[,1]]
  if(length(bioc_pkgs)){
    message("Installing Dependencies from BIOCONDUCTOR...")
    source("https://bioconductor.org/biocLite.R")
    biocLite(bioc_pkgs, suppressUpdates = T, suppressAutoUpdate = T)
  }
})
#### -- End Autoloader -- ####
