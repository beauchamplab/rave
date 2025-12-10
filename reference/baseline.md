# Baseline signals

Baseline signals

## Usage

``` r
baseline(
  el,
  from,
  to,
  method = "mean",
  unit = "%",
  data_only = FALSE,
  hybrid = TRUE,
  swap_file = tempfile(),
  mem_optimize = TRUE,
  same_dimension = unit %in% c("%", "dB"),
  preop = NULL,
  op,
  data_env = getDefaultDataRepository()
)
```

## Arguments

- el:

  [`Tensor`](https://beauchamplab.github.io/raveio/reference/Tensor.html)
  or
  [`ECoGTensor`](https://beauchamplab.github.io/raveio/reference/ECoGTensor.html)
  object

- from:

  baseline start time

- to:

  baseline end time

- method:

  mean or median, default is mean

- unit:

  "%" percent signal change or "dB" decibel unit

- data_only:

  return array or tensor object?

- hybrid:

  if return tensor object, swap cache? useful for large dataset

- swap_file:

  by default [`tempfile()`](https://rdrr.io/r/base/tempfile.html), or
  you can specify path

- mem_optimize:

  optimize for large dataset? default is TRUE

- same_dimension:

  logical, true if `op` is element-wise operator

- preop:

  function before baseline

- op:

  function for baseline

- data_env:

  internally used
