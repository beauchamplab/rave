# Get Directories in \`RAVE\`

Get Directories in \`RAVE\`

## Usage

``` r
get_dir(
  subject_code,
  project_name,
  block_num,
  mkdirs = NULL,
  subject_id,
  relative = FALSE
)
```

## Arguments

- subject_code:

  subject code; can be ignored when `subject_id` is provided

- project_name:

  project name; can be ignored when `subject_id` is provided

- block_num:

  block name (optional)

- mkdirs:

  internally used

- subject_id:

  subject ID; can be omitted if `subject_code` and `project_name` are
  provided

- relative:

  whether to return relative path or absolute to root directory
