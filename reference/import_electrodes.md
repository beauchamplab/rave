# Import `.csv` files that contain electrode information

The table to import must contains a column `'Electrode'` that is
consistent with the corresponding subject.

## Usage

``` r
import_electrodes(path, subject, use_fs = NA, ...)
```

## Arguments

- path:

  path to the electrode file to import

- subject:

  'RAVE' project-subject combination

- use_fs:

  whether to use 'FreeSurfer', default is to auto-detect

- ...:

  passed to [`read.csv`](https://rdrr.io/r/utils/read.table.html)
