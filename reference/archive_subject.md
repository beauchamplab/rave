# Archive Subject into Zipped file

Save subject data, including brain imaging files into a zipped file.
Notice this function does not guarantee every file is in. Please always
double check what's inside.

## Usage

``` r
archive_subject(
  project_name,
  subject_code,
  include_cache = FALSE,
  include_fs = TRUE,
  include_raw = FALSE,
  save_to = tempdir()
)
```

## Arguments

- project_name:

  project name

- subject_code:

  subject code

- include_cache:

  whether to include cache for faster loading. Default is false

- include_fs:

  whether to include 'FreeSurfer' and 'AFNI/SUMA' files

- include_raw:

  whether to include raw data

- save_to:

  directory to save file to
