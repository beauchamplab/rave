# Get \`shiny' "input" and "output" objects under current context

Get \`shiny' "input" and "output" objects under current context

## Usage

``` r
getDefaultReactiveInput(session)

# Default S3 method
getDefaultReactiveInput(session)

# S3 method for class 'rave_module_debug'
getDefaultReactiveInput(session)

# S3 method for class 'rave_running'
getDefaultReactiveInput(session = shiny::getDefaultReactiveDomain())

# S3 method for class 'rave_running_local'
getDefaultReactiveInput(session)

getDefaultReactiveOutput(session = shiny::getDefaultReactiveDomain())

# Default S3 method
getDefaultReactiveOutput(session = shiny::getDefaultReactiveDomain())

# S3 method for class 'rave_module_debug'
getDefaultReactiveOutput(session = shiny::getDefaultReactiveDomain())

# S3 method for class 'rave_running'
getDefaultReactiveOutput(session = shiny::getDefaultReactiveDomain())

# S3 method for class 'rave_running_local'
getDefaultReactiveOutput(session = shiny::getDefaultReactiveDomain())
```

## Arguments

- session:

  shiny session instance

## Value

In shiny context, returns special
[`reactiveValues`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
that refers to the inputs and outputs of shiny applications. In
non-shiny contexts, returns a fake environment related to current fake
session, for debug purpose.
