# Function to download subjects from internet/local

Function to download subjects from internet/local

## Usage

``` r
download_subject_data(
  con,
  replace_if_exists = FALSE,
  override_project = NULL,
  override_subject = NULL,
  temp_dir = tempdir(),
  remove_zipfile = TRUE,
  subject_settings = NULL,
  mode = "wb",
  ...
)
```

## Arguments

- con:

  an url or local file path

- replace_if_exists:

  Automatically replace current subject if subject files exist (default
  FALSE)

- override_project:

  if not null, project will be renamed to this value

- override_subject:

  if not null, subject will be renamed to this value

- temp_dir:

  temp directory to store downloaded zip files and extracted files

- remove_zipfile:

  clear downloaded zip files? if `con` is local file, this will be
  forced to FALSE

- subject_settings:

  override `"subject.yaml"` see details

- mode, ...:

  passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Details

Each downloaded zip file should have a `"subject.yaml"` file indicating
default project name, subject code, data directory and raw data
directory.

If you want to override subject settings, you need to implement your own
`subject_settings`. See examples.

## Examples

``` r
if (FALSE) { # \dontrun{
# Normal example
download.file(
  'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip',
  destfile = "~/rave_data/data-small.zip", mode = "wb")
download_subject_data(con = "~/rave_data/data-small.zip")

# or the following
# download_subject_data(
# 'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip'
# )

# rename project to demo_junk
download_subject_data(con = "~/rave_data/data-small.zip",
           override_project = 'demo_junk')

# override settings
download_subject_data(
  con = "~/rave_data/data-small.zip",
  subject_settings = list(
    # subject conf
    'demo_project/demo_subject' = list(
      data_dir = 'data 2/data_dir/demo/sub1',
      raw_dir = 'data 2/raw_dir/sub1'
    )
  )
)
} # }
```
