# Defines 'RAVE' Module Initialization Defines the global variables for the module. Called along with [`define_input`](https://beauchamplab.github.io/rave/reference/define_input.md) to define UI initialization actions once a subject is loaded.

Defines 'RAVE' Module Initialization Defines the global variables for
the module. Called along with
[`define_input`](https://beauchamplab.github.io/rave/reference/define_input.md)
to define UI initialization actions once a subject is loaded.

## Usage

``` r
define_initialization(expr)

# Default S3 method
define_initialization(expr)

# S3 method for class 'rave_module_debug'
define_initialization(expr)

# S3 method for class 'rave_running'
define_initialization(expr)

# S3 method for class 'rave_running_local'
define_initialization(expr)
```

## Arguments

- expr:

  R expression to run after subject is loaded

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires to install R package beauchamplab/ravebuiltins

# Enable debug mode
ravebuiltins::dev_ravebuiltins(reload = FALSE)
# Check data
define_initialization({
  rave_checks('power')
  power <- module_tools$get_power(referenced = TRUE)
})


# Initialize global variables for modules
ravebuiltins::dev_ravebuiltins(reload = FALSE)
define_initialization({
  print(subject$info())
  time_points = preload_info$time_points
})
define_input(
  shiny::sliderInput('time_range', 'Time-Range', min=0, 
                     max=1, value = c(0,1)),
  init_args = c('min', 'max', 'value'),
  init_expr = {
    min = min(time_points)
    max = max(time_points)
    value = c(0, max)
  }
)
} # }
```
