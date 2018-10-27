# RAVE

`R` `A`*nalysis and* `V`*isualization of intracranial* `E`*lectroencephalography*

*Author: Zhengjia Wang*

*Last Updated: Oct 26, 2018*


<img src="https://raw.githubusercontent.com/dipterix/instrave/master/img/mainapp/mainapp_demo_small.png" width="46%" align="right" />

```r
# Install rave
install.packages('devtools')
devtools::install_github('beauchamplab/rave')
```

```r
# Launch main app
rave::init_app()
```

```r
# Show documents
help(package = 'rave')
```

<hr />


## Installation

If you have `R` and `RStudio` installed (on Windows, you will also need `RTools` to be installed), just run the following commands in RStudio console, otherwise click here for full-installation guide (should only take less than *10 min*).

```r
# Step 1. install devtools
install.packages('devtools')

# Step 2. install RAVE
devtools::install_github('beauchamplab/rave')

# Step 3. (optional) check additional dependencies just in case.
#         Should be installed automatically. 
rave:::check_updates()

# Step 4. check modules update
arrange_modules(T)
```

## Quick Guide

`RAVE` comes with two toy examples: one only contains raw `Matlab` files, the other has gone though full preprocessing pipeline. We start by introducing the main application (second subject, full preprocessed), and then introduce preprocessing pipeline.

Before starting the first part, let's make sure you are all set. 

First, open `RStudio`, in the console tab, enter the following line:

```r
library(rave)
```

This should print something like this: 

```
Active modules: 
 - 3D Viewer(viewer_3d)
 - Electrode Reference(reference)
 - Condition Explorer(condition_explorer)
 - Inter-Trial Phase Coherence(itpc_phase)
 - Onset Detection(onset_detection)
According to [/Users/beauchamplab/rave_modules/modules.csv]
[ INFO ]: RAVE - (Code: Ent) is loaded!
[ INFO ]: Module File:        	/Users/beauchamplab/rave_modules/modules.csv
[ INFO ]: Data Repository:    	/Users/beauchamplab/rave_data/data_dir
[ INFO ]: Raw-data Repository:	/Users/beauchamplab/rave_data/raw_dir
[ INFO ]: Type 'rave_options(launch_gui = T)' or '?rave_options' for details
```

#### RAVE Settings (optional)

Now, configure settings. This is *Optional* if no error occur during `library(rave)`, or you don't have `AFNI/SUMA` installed. Usually you only needs to set up once. In the console, enter:

```r
rave_options()
```

You will see this from your default browser:

<img src="https://raw.githubusercontent.com/dipterix/instrave/master/img/settings/rave_options.png" width="65%" align="left" />
RAVE settings, an HTML GUI lauched by a single line of code `rave_options()`


The most important part of this settings is **Raw subject data path** and **RAVE subject data path**. 

The first one specifies the raw data directory for RAVE. It stores the original Matlab files, by default is `~/rave_data/raw_dir`. 

RAVE will import raw data into the second folder, which stores the data that RAVE uses for all its scripts and dependencies. The second directory is usually very large and can be a network mapping. By default it is `~/rave_data/data_dir`



If you have `SUMA` installed, you can also specify SUMA path on the right panel. Make sure all paths are absolute paths. You can press "Test SUMA" to see if it launches.

*See more topics on:

RAVE options <br />
Directory Structure <br />
RAVE-SUMA <br />
3D Viewer <br />


<br>

#### Main Application

Once set the correct SUMA path and data repository paths, you can now launch the main application by typing

```r
init_app()
```

