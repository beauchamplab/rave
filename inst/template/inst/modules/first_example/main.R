# Main algorithm - rave_executes

require(${{PACKAGE}})

# Initialize inputs
dev_${{PACKAGE}}(expose_functions = TRUE)

mount_demo_subject()

init_module('${{MODULEID}}', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=TRUE

requested_electrodes = parse_selections(text_electrode)

time_points = preload_info$time_points
frequencies = preload_info$frequencies

trial = module_tools$get_meta('trials')

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug - Step 1: clear environment and reload

${{PACKAGE}}::dev_${{PACKAGE}}(expose_functions = TRUE)

mount_demo_subject()

env = reload_this_package(expose = FALSE, clear_env = TRUE)

# Step 2: make sure rave data is attached
attachDefaultDataRepository()


# Step 3: try to run from local session
module = rave::get_module(package = '${{PACKAGE}}', module_id = '${{MODULEID}}', local = T)

res = module()

# Check results
res$results$get_value(key = 'requested_electrodes')
res$results$get_value(key = 'time_points')
res$results$get_value(key = 'frequencies')
res$results$get_value(key = 'trial')

# Step 4: load module and preview

# First method
m = rave::get_module(package = '${{PACKAGE}}', module_id = '${{MODULEID}}', local = F)
rave::init_app(m)

# Second method
${{PACKAGE}}::dev_${{PACKAGE}}(expose_functions = TRUE)
view_layout('${{MODULEID}}')
