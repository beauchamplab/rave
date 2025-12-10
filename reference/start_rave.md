# Start RAVE main application

Start RAVE main application

## Usage

``` r
start_rave_legacy(
  modules = NULL,
  active_module = NULL,
  launch.browser = TRUE,
  theme = "purple",
  disable_sidebar = FALSE,
  simplify_header = FALSE,
  token = NULL,
  data_repo = getDefaultDataRepository(),
  ...
)

launch_demo(
  modules = "power_explorer",
  launch.browser = TRUE,
  theme = "green",
  disable_sidebar = TRUE,
  simplify_header = FALSE,
  ...
)

start_rave2(
  host = "127.0.0.1",
  port = NULL,
  launch.browser = TRUE,
  jupyter = FALSE,
  as_job = FALSE,
  ...
)

start_rave(
  host = "127.0.0.1",
  port = NULL,
  launch.browser = TRUE,
  jupyter = FALSE,
  as_job = FALSE,
  ...
)
```

## Arguments

- modules:

  character vector, modules modules to load before starting application.

- active_module:

  character, which module to show as default.

- launch.browser:

  logical, whether to launch browser.

- theme:

  character, color theme, default is `'purple'`.

- disable_sidebar:

  logical, whether to hide sidebar.

- simplify_header:

  logical, whether to show simplified header.

- token:

  character vector, default is `NULL`. If specified, then a `?token=...`
  is needed in url to access to the application.

- data_repo:

  internally used.

- ...:

  other parameters. See details.

- host, port, jupyter, as_job:

  'RAVE' 2.0 related arguments; see
  [`start_session`](https://dipterix.org/ravedash/reference/rave-session.html)
