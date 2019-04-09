# Developer Notes

**Starting From 03/11/2019**

**Created by: Zhengjia Wang**

## Package Build Checks

| Package Check Results  | Stable | Dev (RAVE-Fir) |
|:------------------|:------:|:----:|
| MacOS, Ubuntu     | [![Build Status](https://travis-ci.org/beauchamplab/rave.svg?branch=master)](https://travis-ci.org/beauchamplab/rave) | [![Build Status](https://travis-ci.org/beauchamplab/rave.svg?branch=rave-fir)](https://travis-ci.org/beauchamplab/rave) |
| Windows           | [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/beauchamplab/rave?branch=master&svg=true)](https://ci.appveyor.com/project/beauchamplab/rave) | [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/beauchamplab/rave?branch=rave-fir&svg=true)](https://ci.appveyor.com/project/beauchamplab/rave) |


## Notes

### TODOs for the next mini-version

### 04/09/2019

* Fixed `ravebuiltins` bugs, migrating to new decorator system
* Updated READMEs
* Re-order the loading screen by making 3D viewer to the top
* Updated AWS instance with newest RAVE
* Added alias `start_rave`
* Added support to use package function to generate UI
* Safe wrap `observer` and `observeEvent` to avoid crash
* Load N27 only for multiple subjects to save RAM
* Got rid of `%&%`
* Modules can call themselves, making it easier to export module results

### 03/20/2019

* `threeBrain` to `RAVE` double click events
* maximize widget height
* migrate all `threejsr` to `threeBrain`
* fix `threeBrain` gui height and scroll problem
* Enabled multiple subject in N27 brain


### 03/12/2019

* Pushed package `rutabaga` and `threeBrain` to cran, pending for inspection.
* Load mesh by default shiny_data_selector (when launching RAVE to select data)
* gui.dat transparency is now set to 0 (no transparency)
* `threeBrain` now can show customized information
* `threeBrain` has octree removed and embrace the native raycasters. 

Bug fixes

* `threeBrain` scene.clear_all() not clean all objects from clickable (fixed)


### 03/11/2019

* Added `threeBrain` package to github to replace `threejsr`
* Added `define_output_3d_viewer` to customized module packages to build 3D viewer easier for module writers.

Note:

`threeBrain` package re-implements JS library to render meshes in a more efficient 
way than `threejsr`. It also uses the latest three.js framework. 
