# Load scripts that cannot put into package R folder

Use in `comp.R` to load scripts that cannot be put into package `"R/"`
folder. Usually the scripts contains shiny reactive values that changes
dynamically.

## Usage

``` r
load_scripts(..., asis = FALSE)
```

## Arguments

- ...:

  script files that are wrapped by
  [`get_path`](https://beauchamplab.github.io/rave/reference/get_path.md),
  or R quasi-quotations wrapped by
  [`quo`](https://rlang.r-lib.org/reference/defusing-advanced.html).

- asis:

  if the scripts to be loaded is a file, whether to copy to a temporary
  directory when launching 'RAVE'. Usually we set this to be true to
  save loading time. However, if your scripts also source other scripts
  in relative path, we recommend setting `asis=FALSE` and also load
  additional scripts using this function.

## Value

None, but will source, or run whatever code provided.

## Details

This function raises error when running in default contexts, and
requires debug mode, or run inside of 'RAVE' instance.

## See also

[`rave_context`](https://beauchamplab.github.io/rave/reference/rave_context.md),
[`get_path`](https://beauchamplab.github.io/rave/reference/get_path.md),
[`quo`](https://rlang.r-lib.org/reference/defusing-advanced.html)
