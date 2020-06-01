#!/usr/bin/env r
#
# Copyright (C) 2020         Zhengjia Wang
# Released under GPL (>= 3)

utils::unzip('/data/init/N27.zip', exdir = '/data/shared/others/three_brain', overwrite = TRUE)
pass_check = threeBrain::check_freesurfer_path('/data/shared/others/three_brain/N27', autoinstall_template = FALSE)
if (isTRUE(pass_check))  {
  threeBrain::merge_brain(template_dir = '/data/shared/others/three_brain/')
}

rave::rave_options(data_dir = '/data/ext/rave_data/data_dir', raw_data_dir = '/data/ext/rave_data/raw_dir')

if(!length(rave::get_projects()) && file.exists('/data/init/YAB.zip')){
  rave::download_subject_data(con = '/data/init/YAB.zip', replace_if_exists = TRUE, remove_zipfile = FALSE)
  rave::rave_brain2('demo/YAB', surfaces = c('pial', 'white', 'smoothwm'))
}

# check the maximum cores. Default to max of 8 cores

ncores <- rave::rave_options('max_worker')
if(ncores > 8){
  ncores = 8L
}
if(ncores < 1){
  ncores = 1L
}
rave::rave_options('max_worker' = ncores)

try({
  
  rave:::test_hdspeed()
  rave::rave_options(disable_startup_speed_check = TRUE)
  
})


