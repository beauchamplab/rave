# Get Value or Default

Get Value or Default

## Usage

``` r
get_val(x, key = NULL, ..., .invalids = c("null", "na"))
```

## Arguments

- x:

  a list, or environment, or just any R object

- key:

  the name to obtain from `x`. Default is `NULL`

- ...:

  if the value is invalid, the default value to return

- .invalids:

  what counts as invalid? Default is `NULL` and `NA`, represented by
  `"null"` and `"na"`
