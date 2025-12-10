# Function to load RAVE module package with UI tools

called internally by
[`init_module`](https://beauchamplab.github.io/rave/reference/init_module.md)
or other module packages

## Usage

``` r
load_rave_module_package(
  env,
  parse_context = c("rave_module_debug", "rave_running", "rave_running_local")
)
```

## Arguments

- env:

  environment to load tools

- parse_context:

  parsing context
