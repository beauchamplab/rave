# Convert module to objects used in shiny

Convert module to objects used in shiny

## Usage

``` r
shinirize(
  module,
  session = getDefaultReactiveDomain(),
  test.mode = TRUE,
  data_env = getDefaultDataRepository()
)
```

## Arguments

- module:

  [`ModuleEnvir`](https://beauchamplab.github.io/rave/reference/ModuleEnvir.md)
  object

- session:

  shiny session, default is current shiny session

- test.mode:

  passed by
  [`start_rave`](https://beauchamplab.github.io/rave/reference/start_rave.md)
  or
  [`init_app`](https://beauchamplab.github.io/rave/reference/init_app.md)

- data_env:

  internally used
