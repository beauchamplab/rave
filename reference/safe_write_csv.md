# Save data to "CSV", if file exists, rename old file

Save data to "CSV", if file exists, rename old file

## Usage

``` r
safe_write_csv(data, file, ..., quiet = FALSE)
```

## Arguments

- data:

  data frame

- file:

  "CSV" file to save

- ...:

  pass to [`write.csv`](https://rdrr.io/r/utils/write.table.html)

- quiet:

  suppress overwrite message
