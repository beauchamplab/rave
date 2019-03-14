# Daily Updates (starting from 03/11/2019)

*Author: Zhengjia Wang*

## TODOs for the next mini-version

* `threeBrain` to `RAVE` double click events
* maximize widget height
* migrate all `threejsr` to `threeBrain`
* fix `threeBrain` gui height and scroll problem



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
