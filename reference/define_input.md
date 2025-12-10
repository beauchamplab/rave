# Defines 'RAVE' Module Inputs

Defines 'RAVE' Module Inputs

## Usage

``` r
define_input(
  definition,
  init_args,
  init_expr,
  keyword = "inputId",
  update_level = 2,
  ...
)

# Default S3 method
define_input(
  definition,
  init_args,
  init_expr,
  keyword = "inputId",
  update_level = 2,
  ...
)

# S3 method for class 'rave_module_debug'
define_input(definition, init_args, init_expr, ...)

# S3 method for class 'rave_running_local'
define_input(
  definition,
  init_args,
  init_expr,
  keyword = "inputId",
  update_level = 2,
  ...
)

# S3 method for class 'rave_running'
define_input(
  definition,
  init_args,
  init_expr,
  keyword = "inputId",
  update_level = 2,
  ...
)
```

## Arguments

- definition:

  R expression to define UI elements without `ns()`, for example,
  `textInput('varname', 'Label', ...)`

- init_args:

  arguments to change once a subject is loaded

- init_expr:

  expression to evaluate with subject loaded

- keyword:

  what identifies the input element

- update_level:

  update action code: see details.

- ...:

  ignored or passed to other methods.

## Value

See details

## Details

This function behaves differently in different contexts. By default, it
returns the result of `definition`. When debugging modules (
`"rave_module_debug"`), it assigns a variable to the global environment
with the variable name defined as input ID. In other contexts it parse
the definition and returns a list for 'RAVE' to use internally to
compile the module.

If `update_level` is \`0\` then the input is defined as manual inputs,
which will not trigger re-calculate if changed. If \`1\` is set, then
the input is a render's input, and only update render functions. If
\`2\` is used, then once user change an input, then the whole module is
re-calculated.

`init_args` must be argument names of the definition. Once subject is
loaded, `init_expr` will be evaluated in a local environment, then
variables in `init_args` will be used to update the input widgets.
