# Define 'RAVE' Module Output

Define 'RAVE' Module Output

## Usage

``` r
define_output(
  definition,
  title = "",
  width = 12L,
  order = Inf,
  keyword = "outputId",
  ...
)

# Default S3 method
define_output(
  definition,
  title = "",
  width = 12L,
  order = Inf,
  keyword = "outputId",
  ...
)
```

## Arguments

- definition:

  R expression of output, such as `plotOutput('out')`

- title:

  Title to show

- width:

  integer from 1 to 12, similar to "width" in
  [`column`](https://rdrr.io/pkg/shiny/man/column.html)

- order:

  the order of output, smaller order will be displayed first

- keyword:

  keyword for the output ID

- ...:

  ignored or passed to other methods

## Value

In default or debug context, it returns HTML tags, but when 'RAVE' is
running, the result will be parse list for internal use.
