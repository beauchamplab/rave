# Main algorithm - rave_executes

require(${{PACKAGE}})

# Initialize inputs
dev_${{PACKAGE}}(expose_functions = TRUE)

mount_demo_subject()

init_module('${{MODULEID}}', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=TRUE

requested_electrodes = dipsaus::parse_svec(text_electrode)

time_points = preload_info$time_points
frequencies = preload_info$frequencies

trial = module_tools$get_meta('trials')

# use only the electrode(s) and trial type(s) requested

tnum <- trial$Trial[trial$Condition %in% requested_conditions]

# load the power
p <- module_tools$get_power()

# subset based on UI choices
p.sub <- p$subset(
  Trial ~ trial$Condition %in% requested_conditions,
  Electrode ~ Electrode %in% requested_electrodes,
  Frequency ~ Frequency %within% requested_frequencies,
  # just return an array, rather than a Tensor object
  data_only = TRUE,
  #don't drop length=1 margins
  drop = FALSE
)

# calculate baseline-corrected data across time(along_dim = 3), per frequency, per trial, per electrode; (unit_dims = 1,2,4)
bsl <- dipsaus::baseline_array(
  x=p.sub, along_dim = 3, unit_dims = c(1,2,4), method = 'percentage',
  baseline_indexpoints = which(
    time_points >= min(requested_baseline) & 
      time_points  <= max(requested_baseline))
)

my_text_result <- my_fancy_function()

y <- dipsaus::collapse(bsl, keep=3, average=TRUE)

my_plot_result <- list(
  x = time_points,
  y = y
)


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
