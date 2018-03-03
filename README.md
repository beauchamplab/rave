# RAVE
R Analysis and Visualization of ECOG Data (Alpha version)

## Installation

### 1. Environment Setup

In this section, you'll install R, RStudio, devtools and all other dependencies.

**R** is a functional programming language that *RAVE* uses. **Devtools** are necessary to enable advanced features. **RStudio** is an *IDE (Intergrated Development Environment)* for easy and better code management especially designed for R. If you are hosting a server in RAVE, or prefer to using command lines, RStudio is not necessary.

You need to check two things before installtion:

* Operating System
  + [Mac OS](#macos)
  + [Windows (Windows 10, with Bash enabled)](#windows)
  + Linux (Ubuntu-Like)
  + Linux (Others)


#### MacOS

#### Windows

On Windows, RAVE is limited because *AFNI* doesn't support windows. However, most of RAVE functions should work. To install RAVE, you need to have *bash* enabled on Windows 10.

It's easy to install on Windows. 

1. First, go to Cran-R official website and download install the latest R:
[https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)

2. After installing R, download and install *Rtools*. Please install the latest version:
[https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)

3. RStudio
[https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)


### 2. Install RAVE

TODO


### Stable version
`devtools::install_github('beauchamplab/rave')`

Please make sure that your R has packages `devtools`, `tidyverse` installed.
It is also recommended that other packages (`rhdf5`, `HDF5Array`) be installed.

### Beta Version
IMPORTANT: This is unstable versionof RAVE

`devtools::install_github('beauchamplab/rave@rave-dipterix')`
- or -
`devtools::install_github('beauchamplab/rave', ref = 'rave-dipterix')`

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



