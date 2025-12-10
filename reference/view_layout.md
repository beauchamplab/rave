# Debug-use only, reload package, mount demo subject, and launch shiny app

Debug-use only, reload package, mount demo subject, and launch shiny app

## Usage

``` r
view_layout(
  module_id,
  sidebar_width = 3,
  launch.browser = TRUE,
  reload = TRUE,
  ...
)
```

## Arguments

- module_id:

  module ID to debug

- sidebar_width:

  input width, from 1 to 11, default is 3

- launch.browser:

  whether to launch browser, default is true, other options are
  [`rstudioapi::viewer`](https://rstudio.github.io/rstudioapi/reference/viewer.html)
  or false.

- reload:

  whether to reload package first. default is true,

- ...:

  passed to
  [`init_app`](https://beauchamplab.github.io/rave/reference/init_app.md)
