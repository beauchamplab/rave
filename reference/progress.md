# A wrapper for shiny Progress object

A wrapper for shiny Progress object

## Usage

``` r
progress(
  title,
  max = 1,
  session = getDefaultReactiveDomain(),
  quiet = FALSE,
  ...
)
```

## Arguments

- title:

  Main title for progress bar

- max:

  How many steps you have for this process

- session:

  shiny session, default is
  [`getDefaultReactiveDomain`](https://rdrr.io/pkg/shiny/man/domains.html)

- quiet:

  nonreactive-only mode? default is FALSE. If TRUE, then progress bar
  will be hidden in shiny app

- ...:

  other parameters passing to
  [`progress2`](https://dipterix.org/dipsaus/reference/progress2.html)

## Details

shiny::Progress class cannot be used under non-reactive environment.
rave::progress function wrap it up so that you can use it in
non-reactive settings.
