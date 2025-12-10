# Check if default data environment has object

Check if default data environment has object

## Usage

``` r
check_data_repo(
  var = c("subject"),
  any = FALSE,
  data_repo = getDefaultDataRepository()
)
```

## Arguments

- var:

  variable name

- any:

  whether all variables should be present of any variables should exist

- data_repo:

  internally used

## Value

Logical `TRUE` or `FALSE` indicating the existence of the variables
