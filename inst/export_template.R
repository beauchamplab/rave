## This script is automatically generated
#{{module_info}}

###### Loading package options ######
#{{work_dir}}
#{{opts}}
opts <- do.call(rave_opts$set_options, args = opts)

###### Loading module settings ######
#{{module_settings}}
# loading module-specific packages
load_packages(c('shiny', 'future', 'plotly', 'rave', packages))

# Load module, create dev-environment
attach_virtualenv(subject_id, electrodes, module_path, packages, is_univariate)
rm(subject_id, electrodes, module_path, packages, is_univariate, opts)

############################## Parameters Here ##############################
# set your parameters here
#{{params}}

# Execute module to get results, as well we inner environment
results <- SHINY_EXECUTE(params)
env <- inner_environments(results)
############################## [BEGIN] Script for results ##############################
# you can actually type `results` to see what's inside,

#{{results_expr}}

############################## [END] Script for results ##############################
