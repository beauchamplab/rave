# Finalize installation

download demo data

## Usage

``` r
finalize_installation(
  packages,
  upgrade = c("ask", "config-only", "always", "never", "data-only"),
  async = FALSE
)
```

## Arguments

- packages:

  package name to finalize. 'rave' to only update base demo data, or
  `c('threeBrain', 'ravebuiltins')` to upgrade built-in data, or leave
  it blank to upgrade all.

- upgrade:

  whether to ask. Default is `'always'` to receive default settings.
  Other choices are `'ask'`, `'never'`, `'config-only'`, and
  `'data-only'`

- async:

  whether to run scripts in parallel; default is true.
