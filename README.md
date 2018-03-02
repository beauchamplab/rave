# RAVE
R Analysis and Visualization of ECOG Data (Alpha version)

## Installation


### Stable version
`devtools::install_github('beauchamplab/rave')`

Please make sure that your R has packages `devtools`, `tidyverse` installed.
It is also recommended that other packages (`rhdf5`, `HDF5Array`) be installed.

### Beta Version
IMPORTANT: This is unstable versionof RAVE

`devtools::install_github('beauchamplab/rave@rave-dipterix')`

Please make sure that you have latest `yaml` package installed. Upon failure, you might want to try: `install.packages('yaml')`.
All other dependencies should be installed automatically

Newest Update: (As of 03/02/2018)
*To be added*


## Toy example
```
# Load packages
library(rave)
require(tidyverse)
require(magrittr)

# Re-direct ECOG data directory and module index file
rave_opts$set_options(
  data_dir = system.file('example/data', package='rave'),
  module_lookup_file = system.file('modules.csv', package='rave')
);

# Launch web service
init_app()
```

## Details (if you are under stable version)
Please check `vignettes/user_guide` for details such as 
*data format*, *SUMA connection*, *Matlab options*. To know how to 
*write modules*, or *use command lines* (import subject data and perform 
quick analysis), check `vignettes/rafe-cookbook`.



