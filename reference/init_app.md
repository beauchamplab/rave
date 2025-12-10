# Initialize main application for debugging purpose

Initialize main application for debugging purpose

## Usage

``` r
init_app(
  modules = NULL,
  active_module = NULL,
  launch.browser = TRUE,
  theme = "red",
  disable_sidebar = FALSE,
  simplify_header = FALSE,
  ...,
  data_repo = getDefaultDataRepository()
)
```

## Arguments

- modules:

  which modules to show. See
  [`load_modules`](https://beauchamplab.github.io/rave/reference/load_modules.md)

- active_module:

  which module to focus at start up (use module ID)

- launch.browser:

  launch browsers, default is on

- theme:

  color theme for the website

- disable_sidebar:

  hide sidebar at startup?

- simplify_header:

  hide header at startup?

- ...:

  other parameters like `test.mode` for module debugging

- data_repo:

  internally used
