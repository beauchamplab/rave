---
title: RAVE
author: Zhengjia Wang
date: Oct 26, 2018
---
<span style="color:#ffa500">**R**</span> <span style="color:#1874cd">**A**</span>*nalysis and* <span style="color:#006400">**V**</span>*isualization of intracranial* <span style="color:#7d26cd">**E**</span>*lectroencephalography*

<div style = "width:100%; display:flex;">
<div style = "width:60%; flex-basis:60%; margin-right:15px;">

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
# Show document
help(package = 'rave')
```
</div>
<div style = "width:40%; flex-basis:40%">
<img src="https://raw.githubusercontent.com/dipterix/instrave/master/img/mainapp/mainapp_demo.png" width="100%" />
RAVE Builtin Module - **Condition Explorer** by *John F. Magnotti*, *Zhengjia Wang*
</div>
</div>

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

Now, configure settings. This usually only needs to be done once. In your console, enter:

```r
rave_options()
```

You will see this from your default browser:

<div style = "width:100%; display:flex;">
<div style = "width:60%; flex-basis:60%; margin-right:15px;">
![RAVE settings, an HTML GUI lauched by a single line of code `rave_options()`](https://raw.githubusercontent.com/dipterix/instrave/master/img/settings/rave_options.png)
</div>
<div style = "width:40%; flex-basis:40%">
The most important part of this settings is **Raw subject data path** and **RAVE subject data path**. 

The first one specifies the raw data directory for RAVE. It stores the original Matlab files, by default is `~/rave_data/raw_dir`. 

RAVE will import raw data into the second folder, which stores the data that RAVE uses for all its scripts and dependencies. The second directory is usually very large and can be a network mapping. By default it is `~/rave_data/data_dir`



If you have `SUMA` installed, you can also specify SUMA path on the right panel. Make sure all paths are absolute paths. You can press "Test SUMA" to see if it launches.

*See more topics on:

* RAVE options
* Directory Structure
* RAVE-SUMA
* 3D Viewer

</div>
</div>
