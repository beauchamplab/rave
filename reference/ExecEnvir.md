# Session-based Module Runtime Environment Class

where all the module functions are executed. It's rarely created
manually, use
[`get_module`](https://beauchamplab.github.io/rave/reference/get_module.md)
to create module, run with `start_app(m, test.mode=TRUE)`, and then
inspect modules.

## Author

Zhengjia Wang

## Public fields

- `.__rave_context__.`:

  context string for current instance, indicating whether the module is
  running locally (public, but internally used)

- `.__rave_package__.`:

  current package name to run (public, but internally used)

- `.__rave_module__.`:

  module ID (public, but internally used)

- `.__rave_module_instance__.`:

  self instance (public, but internally used)

- `module_env`:

  [`ModuleEnvir`](https://beauchamplab.github.io/rave/reference/ModuleEnvir.md)
  instance

- `cache_env`:

  cache environment to store key-value pairs locally

- `parent_env`:

  the parent/top environment of the module, usually global environment
  or some name-space if the module is implemented as an R package

- `wrapper_env`:

  stores all the utility functions. Some functions are overridden there
  such as [`observe`](https://rdrr.io/pkg/shiny/man/observe.html),
  `rave_checks`, or `eval_when_ready`. These functions behave
  differently inside or outside of shiny context, and with or without
  data loaded. The environment will be locked once the module is
  initialized. The parent environment is `parent_env`

- `static_env`:

  stores module static functions. These functions are evaluated under
  `parse_env` and then moved here. The environment is locked after
  initialization. Its parent environment is `wrapper_env`

- `param_env`:

  stores parameters and most of the user inputs. It can also serve as a
  repository for global variables. Unlike the previous environments,
  `param_env` is unlocked, but module creators do not have access to
  this environment directly. The parent environment is `static_env`

- `runtime_env`:

  where the main part of module is running. All shiny
  [`observe`](https://rdrr.io/pkg/shiny/man/observe.html) and
  [`observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html) are
  redirected to this environment by default (unless using
  [`shiny::observe`](https://rdrr.io/pkg/shiny/man/observe.html)). All
  functions in `static_env` have access to this environment. The parent
  environment is `param_env`

- `async_env`:

  where asynchronous codes run

- `parse_env`:

  environment where modules are parsed. The parent environment is
  `runtime_env`. Once all functions are evaluated, this environment is
  not used. However, module creators don't directly access this
  environment once the module is initialized.

- `ns`:

  shiny name-space functions, is equivalent to `shiny::NS(module_id)`.
  The goal is to add prefixes to module inputs so that two modules with
  the same input ID are named differently

- `auto_execute`:

  (Deprecated) whether to auto-calculate results

- `manual_inputIds`:

  character vector; name list of manually input IDs. Used when the
  algorithm takes long to run

- `rendering_inputIds`:

  character vector; name list of input IDs that when one of the
  corresponding inputs is changed, then `rave_execute` will not get
  evaluated. Only the outputs are changed.

- `input_update`:

  expressions to update inputs

- `register_output_events`:

  expressions to register outputs

- `register_input_events`:

  expressions to register inputs

- `execute`:

  module main function. The function is dynamically generated. Don't
  call directly.

- `async_module`:

  (experimental) whether the module contains any asynchronous part

- `global_reactives`:

  shiny global `reactives`, internal use only

- `local_reactives`:

  shiny local `reactives`, internal use only

- `internal_reactives`:

  internal reactive values to control some elements, internal use only

- `ready_functions`:

  functions to run when the module is ready. The functions are called at
  the last step of
  [`shinirize`](https://beauchamplab.github.io/rave/reference/shinirize.md).
  Usually it's used along with `eval_when_ready`, to make sure
  `global_reactives` and `local_reactives` getting registered before
  functions calls

## Active bindings

- `input_ids`:

  vector of input IDs (read-only)

- `input_labels`:

  vector of input labels (read-only)

- `output_labels`:

  vector of output labels (read-only)

- `output_ids`:

  vector of output IDs (read-only)

## Methods

### Public methods

- [`ExecEnvir$reload()`](#method-ExecEnvir-reload)

- [`ExecEnvir$info()`](#method-ExecEnvir-info)

- [`ExecEnvir$print()`](#method-ExecEnvir-print)

- [`ExecEnvir$clean()`](#method-ExecEnvir-clean)

- [`ExecEnvir$new()`](#method-ExecEnvir-new)

- [`ExecEnvir$reset()`](#method-ExecEnvir-reset)

- [`ExecEnvir$copy()`](#method-ExecEnvir-copy)

- [`ExecEnvir$execute_with()`](#method-ExecEnvir-execute_with)

- [`ExecEnvir$names()`](#method-ExecEnvir-names)

- [`ExecEnvir$register_module()`](#method-ExecEnvir-register_module)

- [`ExecEnvir$register_context()`](#method-ExecEnvir-register_context)

- [`ExecEnvir$rave_inputs()`](#method-ExecEnvir-rave_inputs)

- [`ExecEnvir$rave_outputs()`](#method-ExecEnvir-rave_outputs)

- [`ExecEnvir$rave_updates()`](#method-ExecEnvir-rave_updates)

- [`ExecEnvir$rave_execute()`](#method-ExecEnvir-rave_execute)

- [`ExecEnvir$set_browser()`](#method-ExecEnvir-set_browser)

- [`ExecEnvir$generate_input_ui()`](#method-ExecEnvir-generate_input_ui)

- [`ExecEnvir$generate_output_ui()`](#method-ExecEnvir-generate_output_ui)

- [`ExecEnvir$is_global()`](#method-ExecEnvir-is_global)

- [`ExecEnvir$clone()`](#method-ExecEnvir-clone)

------------------------------------------------------------------------

### Method `reload()`

(experimental) signal the modules to reload

#### Usage

    ExecEnvir$reload()

#### Returns

none

------------------------------------------------------------------------

### Method `info()`

print variables in different layers (environment)

#### Usage

    ExecEnvir$info()

#### Returns

none

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print the memory address

#### Usage

    ExecEnvir$print(...)

#### Arguments

- `...`:

  ignored

#### Returns

memory address

------------------------------------------------------------------------

### Method `clean()`

clean the environments to release the resource

#### Usage

    ExecEnvir$clean()

#### Returns

none

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    ExecEnvir$new(session = getDefaultReactiveDomain(), parent_env = NULL)

#### Arguments

- `session`:

  shiny session instance

- `parent_env`:

  parent environment of this instance: package name space or global
  environment

------------------------------------------------------------------------

### Method `reset()`

reset the runtime environment, rarely used

#### Usage

    ExecEnvir$reset(inputs)

#### Arguments

- `inputs`:

  reactive value list

#### Returns

none

------------------------------------------------------------------------

### Method `copy()`

(deprecated) copy the instance locally

#### Usage

    ExecEnvir$copy(
      session_id = "__fake_runtime_env__",
      data_env = getDefaultDataRepository()
    )

#### Arguments

- `session_id`:

  character

- `data_env`:

  where the data is stored, default is the environment returned by
  [`getDefaultDataRepository`](https://beauchamplab.github.io/rave/reference/getDefaultDataRepository.md)

#### Returns

a copied instance

------------------------------------------------------------------------

### Method `execute_with()`

(deprecated) execute module with given parameter

#### Usage

    ExecEnvir$execute_with(param, async = FALSE, plan = NULL)

#### Arguments

- `param`:

  named list

- `async`:

  whether to run the whole module

- `plan`:

  future plan

#### Returns

runtime environment

------------------------------------------------------------------------

### Method [`names()`](https://rdrr.io/r/base/names.html)

returns names of a list, if names are null, returns blank characters

#### Usage

    ExecEnvir$names(x)

#### Arguments

- `x`:

  a list

#### Returns

the names of the list

------------------------------------------------------------------------

### Method `register_module()`

register
[`ModuleEnvir`](https://beauchamplab.github.io/rave/reference/ModuleEnvir.md)
instance

#### Usage

    ExecEnvir$register_module(module_env)

#### Arguments

- `module_env`:

  [`ModuleEnvir`](https://beauchamplab.github.io/rave/reference/ModuleEnvir.md)
  instance. The modules are shared across different sessions, but to run
  the module, we need to create runtime environment, which is
  `ExecEnvir`

#### Returns

none

------------------------------------------------------------------------

### Method `register_context()`

Register 'RAVE' context for current environment (internally used)

#### Usage

    ExecEnvir$register_context(context = c("rave_running", "rave_running_local"))

#### Arguments

- `context`:

  context string to indicate whether the module is running locally

#### Returns

None

------------------------------------------------------------------------

### Method `rave_inputs()`

parse input components

#### Usage

    ExecEnvir$rave_inputs(
      ...,
      .input_panels = list(),
      .tabsets = list(),
      .env = NULL,
      .manual_inputs = NULL,
      .render_inputs = NULL
    )

#### Arguments

- `...`:

  shiny input calls, such as `textInput('id', 'Name', ...)`

- `.input_panels, .tabsets`:

  together define the input layouts

- `.env`:

  ignored, debug only

- `.manual_inputs`:

  input IDs that won't cause module re-calculate when inputs are updated

- `.render_inputs`:

  input IDs that only trigger render functions when updated

#### Returns

none

------------------------------------------------------------------------

### Method `rave_outputs()`

parse output components

#### Usage

    ExecEnvir$rave_outputs(
      ...,
      .output_tabsets = list(),
      .tabsets = list(),
      .env = NULL
    )

#### Arguments

- `...`:

  shiny output calls, such as `plotOutput('id', 'Title')`

- `.output_tabsets, .tabsets`:

  together define the output layouts

- `.env`:

  debug use

#### Returns

none

------------------------------------------------------------------------

### Method `rave_updates()`

input initialization when `iEEG/ECoG` data are imported

#### Usage

    ExecEnvir$rave_updates(..., .env = NULL)

#### Arguments

- `...`:

  R expressions

- `.env`:

  for debug use

------------------------------------------------------------------------

### Method `rave_execute()`

parse, and compile to main function

#### Usage

    ExecEnvir$rave_execute(..., auto = TRUE, .env = NULL, async_vars = NULL)

#### Arguments

- `...`:

  R expressions

- `auto`:

  whether the module should run automatically

- `.env`:

  debug use

- `async_vars`:

  variables further passed to `async` module

#### Returns

none, but `ExecEnvir$execute` will be generated.

------------------------------------------------------------------------

### Method `set_browser()`

(experimental) cache R expression in browser `localStorage`

#### Usage

    ExecEnvir$set_browser(expr, session = getDefaultReactiveDomain())

#### Arguments

- `expr`:

  R expression

- `session`:

  shiny session instance

------------------------------------------------------------------------

### Method `generate_input_ui()`

generate input panels according to parsed `rave_inputs`

#### Usage

    ExecEnvir$generate_input_ui(sidebar_width = 3L)

#### Arguments

- `sidebar_width`:

  integer from 1 to 11, the width of the input panels

#### Returns

HTML tags

------------------------------------------------------------------------

### Method `generate_output_ui()`

generate outputs labels according to parsed `rave_outputs`

#### Usage

    ExecEnvir$generate_output_ui(sidebar_width = 3L)

#### Arguments

- `sidebar_width`:

  integer from 1 to 11, the width of the input panels, the output panel
  width is calculated as `12-sidebar_width`

#### Returns

HTML tags

------------------------------------------------------------------------

### Method `is_global()`

(deprecated) check if variable is shared across modules. Please use
`cache_input` instead to get variable values.

#### Usage

    ExecEnvir$is_global(inputId)

#### Arguments

- `inputId`:

  input ID

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ExecEnvir$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{

# Load module
module <- get_module('ravebuiltins', 'power_explorer')

# Create execute environmen
execenv <- module$get_or_new_exec_env()
execenv$info()

} # }
```
