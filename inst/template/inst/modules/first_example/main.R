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

# Debug

${{PACKAGE}}::dev_${{PACKAGE}}(expose_functions = TRUE)

# Debug - offline:
main = ${{PACKAGE}}:::debug_module('${{MODULEID}}')
ret = main()
result = ret$results

result$get_value('preload_info')


# Debug - online:
${{PACKAGE}}::dev_${{PACKAGE}}(expose_functions = TRUE)
mount_demo_subject()
view_layout('${{MODULEID}}')

# Production - Deploy as RAVE module
# Always Ctrl/cmd+shift+B first if you want to deploy it online
rm(list = ls(all.names = TRUE)); rstudioapi::restartSession()
module = rave::get_module(package = '${{PACKAGE}}', module_id = '${{MODULEID}}')
rave::init_app(module)
