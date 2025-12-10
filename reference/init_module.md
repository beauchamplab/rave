# Initialize 'RAVE' module for debug purpose

Initialize 'RAVE' module for debug purpose

## Usage

``` r
init_module(
  module_id,
  debug = FALSE,
  parse_context = c("rave_running", "rave_running_local")
)
```

## Arguments

- module_id:

  module ID

- debug:

  whether to expose all functions to the global environment

- parse_context:

  parsing context, for internal use

## See also

[`load_rave_module_package`](https://beauchamplab.github.io/rave/reference/load_rave_module_package.md)
