# Parse 'RAVE' Module Contents

Parse 'RAVE' Module Contents

## Usage

``` r
get_content(content, env, evaluate = TRUE, chunks = FALSE)
```

## Arguments

- content:

  characters, R code to parse into expressions

- env:

  environment to parse the code into, please specify the context

- evaluate:

  whether to evaluate parse expression into `env`

- chunks:

  whether to respect code notations and chunk the code into separate
  parts

## Value

See details.

## Details

If `"evaluate=TRUE"`, then the parse code will be evaluated within `env`
and returns a logical value: `TRUE` means the content has something,
otherwise returns `FALSE`

If `"evaluate=FALSE"`, returns the parsed expression and add attributes
about the names of each chunk and whether they are asynchronous
