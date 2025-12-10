# 'RAVE' Context: Read and Set Context of Environments

'RAVE' Context: Read and Set Context of Environments

## Usage

``` r
rave_context(
  context,
  require_contexts,
  disallowed_context,
  error_msg,
  spos = 2L,
  senv,
  tpos = 1L,
  tenv
)
```

## Arguments

- context:

  context string for target environment, optional, see \`Details'

- require_contexts:

  characters, (optional): required context for current function. If any
  context is missing, the function will raise errors

- disallowed_context:

  characters, (optional): defines the contexts that don't work for the
  function. If running within such contexts, the function will raise
  errors

- error_msg:

  characters, (optional): if running in improper contexts, the message
  to display, will passed to [`stop`](https://rdrr.io/r/base/stop.html)

- spos:

  levels to go up to search for `senv`, passed to
  [`parent.frame`](https://rdrr.io/r/base/sys.parent.html)

- senv:

  environment to read 'RAVE' contexts

- tpos:

  levels to go up to search for `tenv`, passed to
  [`parent.frame`](https://rdrr.io/r/base/sys.parent.html)

- tenv:

  environment to set 'RAVE' contexts

## Value

A list of current context, including the package name, module ID, and
current `ExecEnvir` instance if running under `"rave_running"` context.

## Details

Context strings tells the function which context it's running, and it
will affect the behaviors of functions within its environment. Because
'RAVE' modules are usually R packages, the context strings help the
module writers determine where the function is running. For example,
running locally, or in 'RAVE' container, or debug mode. A typical
example would be
[`get_path`](https://beauchamplab.github.io/rave/reference/get_path.md)
function. All external scripts used in R packages require to be obtained
using [`system.file`](https://rdrr.io/r/base/system.file.html). However,
because the files are subject to change, using system file function
requires re-compile the package, which is time-consuming. Function
[`get_path`](https://beauchamplab.github.io/rave/reference/get_path.md)
returns the file path relative to current working directory during the
development (in "default" context), and it calls
[`system.file`](https://rdrr.io/r/base/system.file.html) when 'RAVE'
instance is running.

There are four contexts: `"default"`, `"rave_module_debug"`,
`"rave_running"`, and `"rave_running_local"`.

- `default`:

  Default context: this means the function is running without any
  additional information.

- `rave_module_debug`:

  Debug mode: used to develop and debug modules locally. Under the
  context, the function will be aware of the package that module belongs
  to

- `rave_running`:

  If the function is running under this context, this means it's running
  inside of shiny application (usually within
  [`start_rave`](https://beauchamplab.github.io/rave/reference/start_rave.md)).
  The function will be able to get more contexts such as module ID, and
  current runtime environment
  ([`ExecEnvir`](https://beauchamplab.github.io/rave/reference/ExecEnvir.md))

- `rave_running_local`:

  Similar to `"rave_running"`, but without run-time environment. Under
  this context, the module is running locally without shiny. All
  reactive observers are disabled, and the modules will be compiled into
  a function with all the inputs defined by
  [`define_input`](https://beauchamplab.github.io/rave/reference/define_input.md)
  as arguments, and code within `"main.R"` as the main body of the
  function.

Function `rave_context` uses reserved variables in the environment:
`.__rave_context__.`, `.__rave_package__.`, `.__rave_module__.`, and
`.__rave_module_instance__.`. Please don't use these variables for other
purposes. See \`Examples' for how to set and read the context.

## Examples

``` r
# ------- 1. Read/Set Context ---------

library(dipsaus)
library(rave)
# Reset context for current environment
rave_context('default')

# Read from current caller's environment
fun <- function(...){
  ctx <- rave_context()
  cat2('The function is running under context - ', ctx$context)
  cat2('The package under the context - ', ctx$package)
  cat2('Module ID is - ', ctx$module_id)
  cat2('Running instance is - ', ctx$instance)
}
fun()
#> The function is running under context -  default 
#> The package under the context -   
#> Module ID is -   
#> Running instance is -  
## The function is running under context - default
## The package under the context - 
## ...

# Set debug context 
debug_env <- new.env()
rave_context('rave_module_debug', tenv = debug_env)
debug_env$.__rave_package__. <- 'ravebuiltins'

# With debug_env, the function is aware of the package it's under
with(debug_env, { fun() })
#> The function is running under context -  rave_module_debug 
#> The package under the context -  ravebuiltins 
#> Module ID is -   
#> Running instance is -  
## The function is running under context - rave_module_debug
## The package under the context - ravebuiltins
## ...

# To set context within the function and affect the functions inide
fun2 <- function(module_id){
  # Run rave_context and then set module ID
  rave_context('rave_running_local')
  .__rave_module__. <- module_id
  fun()
}
with(debug_env, { fun2('power_explorer') })
#> The function is running under context -  rave_running_local 
#> The package under the context -  ravebuiltins 
#> Module ID is -  power_explorer 
#> Running instance is -  
## The function is running under context - rave_running_local
## The package under the context - ravebuiltins
## Module ID is - power_explorer
## ...

# Let's see what we can do with rave_module_debug
with(debug_env, { get_path('inst/rave.yaml') })
# When I develop the package, it returns:
## "/Users/beauchamplab/.../ravebuiltins/inst/settings.yaml"
# When I run in other places, it returns
## "/Users/beauchamplab/Library/R/3.6/library/ravebuiltins/rave.yaml"


# ------- 2. Setting behaviors for context ---------
# One way to set different behaviors is to using `ctx`
if (FALSE) { # \dontrun{
fun <- function(){
  ctx <- rave_context()
  switch(ctx$context, ...)
}
} # }

# The other way is to use S3 generics provided by R syntax
fun <- rave_context_generics('fun', function(module_id, ...){})

# action for default
fun.default <- function(...){
  cat2('Function is not supposed to run under default context...',
       level = 'ERROR')
}

# for debug, set module ID and run with rave_running_local
fun.rave_module_debug <- function(module_id, ...){
  cat2('Debug mode... loading a test subject')
  # Do something ... like automatically mount_demo_subject
  # by running mount_demo_subject()
  
  rave_context('rave_running_local')
  .__rave_module__. <- module_id
  # Recall the function under rave_running_local context
  fun(module_id, ...)
}

# When running within RAVE container, local and with shiny
fun.rave_running_local <- function(...){
  ctx <- rave_context()
  cat2('Yay, running ', ctx$module_id, ' under context ',
       ctx$context, level='INFO')
}
fun.rave_running <- fun.rave_running_local

# Run in default mode, expect error message
fun('power_explorer')
#> Function is not supposed to run under default context... 

# Run in debug mode
debug_env <- new.env()
rave_context('rave_module_debug', tenv = debug_env)
debug_env$.__rave_package__. <- 'ravebuiltins'

# The function will run in debug mode, then rave_running_local
with(debug_env, { fun('power_explorer') })
#> Debug mode... loading a test subject 
#> Yay, running  power_explorer  under context  rave_running_local 


```
