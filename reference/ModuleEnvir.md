# R6 'RAVE' Module Class

contains module data, functions, etc.

## Public fields

- `cache_env`:

  cache environment for module

- `module_id`:

  module ID, unique

- `label_name`:

  corresponding module name

- `script_path`:

  compiled module scripts

- `script`:

  if `script_path` not exists, alternative script

- `author`:

  who wrote the module not often used

- `version`:

  module version

- `packages`:

  the packages to be loaded for the module

- `rmd_path`:

  deprecated

- `parent_env`:

  parent environment of the module, usually global environment or
  package environment

- `from_package`:

  whether the module is compiled from another R package. This value is
  required to be true since `"rave-0.1.9"`.

- `package_name`:

  which package does the module belong to?

- `sidebar_width`:

  input panel width, from 1 to 11

## Methods

### Public methods

- [`ModuleEnvir$info()`](#method-ModuleEnvir-info)

- [`ModuleEnvir$print()`](#method-ModuleEnvir-print)

- [`ModuleEnvir$new()`](#method-ModuleEnvir-new)

- [`ModuleEnvir$get_or_new_exec_env()`](#method-ModuleEnvir-get_or_new_exec_env)

- [`ModuleEnvir$load_script()`](#method-ModuleEnvir-load_script)

- [`ModuleEnvir$render_ui()`](#method-ModuleEnvir-render_ui)

- [`ModuleEnvir$clean()`](#method-ModuleEnvir-clean)

------------------------------------------------------------------------

### Method `info()`

print module information

#### Usage

    ModuleEnvir$info()

#### Returns

none

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print module information and returns memory address

#### Usage

    ModuleEnvir$print(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    ModuleEnvir$new(
      module_id,
      label_name,
      script_path,
      author = NULL,
      version = "0",
      packages = NULL,
      .script_content = NULL,
      rmd_path = NULL,
      parent_env = globalenv()
    )

#### Arguments

- `module_id, label_name, script_path, author, version`:

  see fields

- `packages, parent_env, rmd_path`:

  see fields

- `.script_content`:

  internal use

------------------------------------------------------------------------

### Method `get_or_new_exec_env()`

get the corresponding
[`ExecEnvir`](https://beauchamplab.github.io/rave/reference/ExecEnvir.md)
with shiny session

#### Usage

    ModuleEnvir$get_or_new_exec_env(
      session = getDefaultReactiveDomain(),
      ...,
      new = FALSE
    )

#### Arguments

- `session`:

  shiny session; see shiny
  [`domains`](https://rdrr.io/pkg/shiny/man/domains.html)

- `...`:

  ignored

- `new`:

  whether to force creating a new runtime environment if previous one
  already exists

#### Returns

an
[`ExecEnvir`](https://beauchamplab.github.io/rave/reference/ExecEnvir.md)
instance associated with current module and given session

------------------------------------------------------------------------

### Method `load_script()`

load and compile script into registered
[`ExecEnvir`](https://beauchamplab.github.io/rave/reference/ExecEnvir.md)

#### Usage

    ModuleEnvir$load_script(session = getDefaultReactiveDomain())

#### Arguments

- `session`:

  shiny session; see shiny
  [`domains`](https://rdrr.io/pkg/shiny/man/domains.html)

#### Returns

none

------------------------------------------------------------------------

### Method `render_ui()`

generate 'HTML' tags

#### Usage

    ModuleEnvir$render_ui(session = getDefaultReactiveDomain())

#### Arguments

- `session`:

  shiny session; see shiny
  [`domains`](https://rdrr.io/pkg/shiny/man/domains.html)

#### Returns

'HTML' tags

------------------------------------------------------------------------

### Method `clean()`

clean the module environment

#### Usage

    ModuleEnvir$clean(session = getDefaultReactiveDomain(), session_id)

#### Arguments

- `session`:

  shiny session; see shiny
  [`domains`](https://rdrr.io/pkg/shiny/man/domains.html)

- `session_id`:

  shiny 'RAVE' ID, default is auto-generated

## Examples

``` r
if (FALSE) { # \dontrun{

module <- get_module('ravebuiltins', 'power_explorer')
module
#> Module Name: Power Explorer 
#> Version: 0 
#> Script Path: ... 
#> Author(s):

} # }
```
