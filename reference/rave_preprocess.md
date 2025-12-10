# RAVE Preprocess Function

RAVE Preprocess Function

## Usage

``` r
rave_preprocess(
  sidebar_width = 3,
  launch.browser = TRUE,
  host = "127.0.0.1",
  port = NULL,
  quiet = TRUE,
  beta = FALSE,
  test.mode = FALSE,
  modules,
  ver = "3",
  theme = "purple",
  ...
)
```

## Arguments

- sidebar_width:

  sidebar width from 1 to 11.

- launch.browser:

  whether to launch browser, default is on

- host:

  default is `"localhost"`

- port:

  integer port of the app

- quiet:

  soft deprecated

- beta:

  whether to load experimental modules, default is false

- test.mode:

  passed to [`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)

- modules:

  preprocess modules to load, reserved

- ver:

  internally used please don't change

- theme:

  color theme

- ...:

  used for other functions for configuration and debug only
