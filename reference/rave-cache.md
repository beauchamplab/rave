# Cache R Objects with Different levels

Cache R Objects with Different levels

## Usage

``` r
cache(
  key,
  val,
  name,
  replace = FALSE,
  global = FALSE,
  persist = FALSE,
  test = FALSE,
  temporary = FALSE,
  ...
)

# S3 method for class 'rave_running'
cache(
  key,
  val,
  name,
  replace = FALSE,
  global = FALSE,
  persist = FALSE,
  test = FALSE,
  temporary = FALSE,
  ...
)

# S3 method for class 'rave_running_local'
cache(..., global = TRUE)

# Default S3 method
cache(..., global = TRUE)

cache_input(
  inputId,
  val = NULL,
  read_only = TRUE,
  ...,
  session = getDefaultReactiveDomain()
)

clear_cache(levels = 1)
```

## Arguments

- key:

  any R object, a named list would be the best.

- val:

  value to cache, if key exists, then value will not be evaluated nor
  saved

- name, inputId:

  character name of the dataset or input

- replace:

  if true, force replace the cached object with current one

- global:

  whether to cache the variable in global environment. If true, then the
  variable will be accessible from other instances and modules.

- persist:

  logical, whether persist on the hard-disk, only used when
  `global=FALSE`, the persisted data will be used by each modules

- test, read_only:

  whether not to save the value if cache is not found

- temporary:

  whether to use temporary map to cache, used internally.

- ...:

  ignored

- session:

  shiny session instance

- levels:

  levels when clear the cache

## Value

Cached value, or `val`. If cache and `val` are both missing, then return
`NULL`.

## Examples

``` r
# global can be set to false within RAVE modules
print(cache('a', 1, name = 'data', global = TRUE)) # returns 1
#> [1] 1
print(cache('a', 2, name = 'data', global = TRUE)) # still returns 1
#> [1] 1

# clear cache (for global=TRUE)
clear_cache(levels = 1:3)
print(cache('a', 2, name = 'data', global = TRUE)) # Now returns 2
#> [1] 2

# Not run `Sys.sleep` because a is cached
print(cache('a', 2, name = 'data', global = TRUE))
#> [1] 2
print(cache('a', {Sys.sleep(10); 1}, name = 'data', global = TRUE))
#> [1] 2

# get data without key
cache(name = 'data', global = TRUE)
#> [1] 2

# clear cache that is global-only
clear_cache(levels = 2)

# Test (test=TRUE) if cache exists, if not, return value but no save
cache(name = 'abracadabra', val = 'no cache', global = TRUE, test = TRUE)
#> [1] "no cache"
cache(name = 'abracadabra', global = TRUE)
#> NULL

# cache module inputs
if (FALSE) { # \dontrun{
# Need to run in package module environment
cache_input('abracadabra', 'no-magic', read_only = TRUE)  
} # }

```
