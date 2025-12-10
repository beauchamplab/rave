# Get RAM usage

Get RAM usage

## Usage

``` r
get_mem_usage(
  modules = list(),
  data_envir = getDefaultDataRepository(),
  session = getDefaultReactiveDomain()
)
```

## Arguments

- modules:

  which module(s)

- data_envir:

  default uses
  [`getDefaultDataRepository`](https://beauchamplab.github.io/rave/reference/getDefaultDataRepository.md)

- session:

  shiny session instance
