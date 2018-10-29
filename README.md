

# RAVE

__R__ __A__*nalysis and* __V__*isualization of intracranial* __E__*lectroencephalography*

*Author: Zhengjia Wang*

*Last Updated: Oct 26, 2018*


<img src="https://raw.githubusercontent.com/dipterix/instrave/master/img/mainapp/mainapp_demo_small.png" width="60%" align="right" />

```r
# Install rave
install.packages('devtools')
library(devtools)
install_github('beauchamplab/rave')

# Launch main app
rave::init_app()
# Preprocess
rave::rave_preprocess()

# Show documents
help(package = 'rave')
```


## Installation

* If this is your first time using `R` or `RStudio`, please [click here for full-installation guide](./installation.md) (around *10-15 min*) and then proceed directly to the next section.

* Read the rest of the section if you have `R` and `RStudio` installed.

### 1. Dependencies

There might be some dependencies to be installed. 

On **MacOS**, open terminal (you can find it using "spotlight" and searching "terminal"), and enter

```
xcode-select --install
```

or check [this](https://www.moncefbelyamani.com/how-to-install-xcode-homebrew-git-rvm-ruby-on-mac/) on how to install `xcode` to your macos.

On **Windows**, install [Rtools](https://cran.r-project.org/bin/windows/Rtools/). Make sure to install the right version. For example, your `R` version is `3.5.1`, install `RTools` version `3.5`.

On **Ubuntu**, open terminal, install packages:

```
sudo apt-get install libssl-dev libcurl4-openssl-dev libssh2-1-dev libv8-3.14-dev libxml2-dev libfftw3-dev libtiff5-dev libhdf5-dev
```

Other linux systems might need to check [recommended system packages](./inst/markdowns/Installation_questions.md#recommended-settings)


### 2. Install RAVE

Launch RStudio, run the following commands in RStudio console:

```r
# Step 1. install devtools
install.packages('devtools')

# Step 2. install RAVE
devtools::install_github('beauchamplab/rave')

# Step 3. (optional) check additional dependencies just in case.
#         Should be installed automatically. 
rave:::check_updates()

# Step 4. (optional) check modules updates
arrange_modules(T)
```

Please **restart RStudio**.

## Quick Guide

In this section, you will know how to

1. Change RAVE general settings
2. Download demo data
3. Start main application with demo data

Before starting the first part, you need to import package `rave` first. Open `RStudio`, in the console tab, enter the following line:

```r
library(rave)
```

This should print something like: 

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


### 1. Change RAVE general settings (optional)

This is *Optional* and you can skip this part if no error occur during `library(rave)`, or you have no `AFNI/SUMA` installed. Usually you only needs to set up once. 

In the console, enter:

```r
rave_options()
```

You will see this from your default browser:

<img src="https://raw.githubusercontent.com/dipterix/instrave/master/img/settings/rave_options.png" width="65%" align="left" />

The most important part of this settings is **Raw subject data path** and **RAVE subject data path**. 

The first one specifies the raw data directory for RAVE. It stores the original Matlab files, by default is `~/rave_data/raw_dir`. 

RAVE will import raw data into the second folder, which stores the data that RAVE uses for all its scripts and dependencies. The second directory is usually very large and can be a network mapping. By default it is `~/rave_data/data_dir`



If you have `SUMA` installed, you can also specify SUMA path on the right panel. Make sure all paths are absolute paths. You can press "Test SUMA" to see if it launches.

*_See more topics on:_

RAVE options, Directory Structure, RAVE-SUMA, 3D Viewer

### 2. Download demo data

We've made downloading sample subject data really simple. As of _oct 28, 2018_, we only have one demo subject available. This number will increase in the future.

In the RStudio console, enter:

```r
download_sample_data(subject = 'sub1')
```

If you see messages as follows, then the subject is downloaded and you can proceed to the next part to view the subject details.

```
[ INFO ]: Expanding zip file
[ INFO ]: Copy from tempdir to data repository
[ INFO ]: Clean up
[ INFO ]: Done. Subject [sub1] is now at 
[Raw Data]: /Users/beauchamplab/rave_data/raw_dir/sub1
[RAVE Data]: /Users/beauchamplab/rave_data/raw_dir/demo
```



### 3. Start main app

Once set the correct SUMA path and data repository paths, you can now launch the main application by typing. Since `RAVE` comes with no data, please refer to the [last part](#2-download-demo-data) to download demo data.

Enter the following command, and your browser will show up with one HTML page like this:


<img src="https://raw.githubusercontent.com/dipterix/instrave/master/img/mainapp/welcome page.png" width="60%" align="right" />

```r
init_app()
```

In the navigation bar, click `Select Data`, and you'll see a panel showing up. There are four input sections and two output sections:

**Inputs**:

* **Project/Subject** select project (`demo`) and subject `sub1` that you downloaded in the last part
* **Epoch Selection** select how you want to epoch each blocks. `pre` is seconds before onset and `post` is seconds after onset.
* **Frequency** can subset frequencies at loading stage. This is useful when your computer doesn't have enough memories
* **Electrode & Reference** select electrodes `14-15,35,38,53,63` to load (we removed other electrodes for `sub1` and only leave 6 electrodes for demo). 

**Outputs**:

* **Load Estimation** will give you estimated data size and roughly estimated loading time. _Make sure_ the data size is not too large (cannot exceed your actual memory, otherwise the program will fail)
* **3D Viewer** A 3D visualization of brain based on [dipterix/threejsr](https://github.com/dipterix/threejsr) showing the location of electrodes. (Make sure `Load Mesh` is on if you want to view pials)

<img src="https://raw.githubusercontent.com/dipterix/instrave/master/img/mainapp/data selection.png" />


Hit `Import` button, go to `Condition Explorer` module and load power data (press button `Load Data`). You will see the following user interface showing power activities of subject `sub1`.

<img src="https://raw.githubusercontent.com/dipterix/instrave/master/img/mainapp/full.png" />


