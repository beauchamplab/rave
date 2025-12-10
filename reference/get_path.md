# Safe Way to Access Module Package Files Using Relative Path

Safe Way to Access Module Package Files Using Relative Path

## Usage

``` r
get_path(..., mustWork = FALSE, is_directory = FALSE)
```

## Arguments

- ...:

  relative path to the file

- mustWork:

  whether the file must exists

- is_directory:

  whether required file is a directory

## Value

If you are developing the package, `get_path` returns the absolute file
path, otherwise it uses
[`system.file`](https://rdrr.io/r/base/system.file.html) to get the file
from package library.
