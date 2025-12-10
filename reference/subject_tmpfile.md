# Create temp file in subject module folder

Create temp file in subject module folder

## Usage

``` r
subject_tmpfile(
  module_id,
  fun_name = "",
  project_name,
  subject_code,
  pattern = "file_",
  data_env = getDefaultDataRepository()
)
```

## Arguments

- module_id:

  module id

- fun_name:

  function name (usually export\_"function_name" in the module)

- project_name:

  project name

- subject_code:

  subject code

- pattern:

  passed to [`tempfile`](https://rdrr.io/r/base/tempfile.html)

- data_env:

  internally used
