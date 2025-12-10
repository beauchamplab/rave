# Reload 'RAVE' module package without restarting 'RStudio'

For debugging module packages. In all other contexts it will raise
error.

## Usage

``` r
reload_module_package(expose = FALSE, clear_env = FALSE)
```

## Arguments

- expose:

  whether to expose development tools to the global environment; default
  is no

- clear_env:

  whether to clear the global environment before reloading; default is
  no
