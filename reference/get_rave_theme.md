# Get RAVE Theme from Package Settings

Get RAVE Theme from Package Settings

## Arguments

- packages:

  packages to check

- type:

  characters, `"continuous"`, `"discrete"`, or both

- theme:

  `"light"` or `"dark"`; default is current theme saved in
  `rave_options('current_theme')`

- session:

  shiny session

## Value

A list contains all palettes found in the packages.

## Examples

``` r
pal = get_rave_theme('rave', type = c('continuous', 'discrete'), theme='light')
print(pal, plot=TRUE)
#> RAVE palettes in "light" mode for "continuous", "discrete" variables
#> Total 2 palettes:
#>   Default-Discrete (6 unique colors)
#>   Default-Continuous (11 unique colors)


pal = get_rave_theme('rave', type = c('continuous', 'discrete'), theme='dark')
print(pal, plot=TRUE)
#> RAVE palettes in "dark" mode for "continuous", "discrete" variables
#> Total 2 palettes:
#>   Default-Discrete (6 unique colors)
#>   Default-Continuous (11 unique colors)

```
