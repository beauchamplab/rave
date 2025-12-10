# `lapply` using future package in asynchronous way

`lapply` using future package in asynchronous way

## Usage

``` r
lapply_async(
  x,
  fun,
  ...,
  .ncores = 0,
  .call_back = NULL,
  .packages = NULL,
  .envir = environment(),
  .globals = TRUE,
  .gc = TRUE,
  .as_datatable = FALSE,
  .nrows = 0
)

lapply_async3(
  x,
  fun,
  ...,
  .globals = TRUE,
  .gc = TRUE,
  .callback = NULL,
  .ncores = 0
)
```

## Arguments

- x, fun, ...:

  (See [`lapply`](https://rdrr.io/r/base/lapply.html))

- .ncores:

  Number of cores to use. If the value is 0, the number of cores will be
  determined by rave_options('max_worker').

- .call_back:

  A function takes current iteration number as argument, can be NULL.

- .packages:

  NULL be default, then the function will detect attached packages
  automatically. Otherwise you have to specify the packages that you
  want to load.

- .envir:

  internally used

- .globals:

  Automatically detect variables. See ?future::future

- .gc:

  Clean up environment after each iterations? Recommended for large
  datasets.

- .as_datatable:

  logical, return result as `data.frame`. Experimental.

- .nrows:

  integer, if `.as_datatable=TRUE`, number of rows expected.

- .callback:

  function or `NULL`, callback function to monitor updates.

## Examples

``` r
if (FALSE) { # \dontrun{
lapply_async(1:10, function(x){
  Sys.sleep(2) # Run for 1 secs
  Sys.getpid()
}, .ncores = 3, .call_back = function(i){
  cat('Running iteration -', i, '\n')
})
} # }
```
