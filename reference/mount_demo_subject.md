# Load Demo Subject According to Package Configuration File

Load Demo Subject According to Package Configuration File

## Usage

``` r
mount_demo_subject(
  subject_code,
  project_name,
  force_reload_subject = FALSE,
  ...,
  download_url
)

# S3 method for class 'rave_module_debug'
mount_demo_subject(
  subject_code,
  project_name,
  force_reload_subject = FALSE,
  ...,
  download_url
)

# S3 method for class 'rave_running'
mount_demo_subject(...)

# S3 method for class 'rave_running_local'
mount_demo_subject(...)
```

## Arguments

- subject_code:

  optional, subject code

- project_name:

  optional, project name

- force_reload_subject:

  logical, whether to force reload subject even if another subject is
  loaded

- ...:

  further passed to
  [`rave_prepare`](https://beauchamplab.github.io/rave/reference/rave_prepare.md)

- download_url:

  optional, web link to subject archive

## Value

None

## Details

When debugging the 'RAVE' modules, it loads demo subject for debugging
from according to settings file `"inst/rave.yaml"`.

This function only function properly in `'rave_module_debug'` mode. This
means by default it raises errors. In other mode, for example
`'rave_running'`, it does nothing.
