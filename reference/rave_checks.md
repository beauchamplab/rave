# Check if data is loaded for current module

Check if data is loaded for current module

## Usage

``` r
rave_checks(
  ...,
  data = NULL,
  .raise_error = TRUE,
  rave_data = getDefaultDataRepository()
)
```

## Arguments

- ...:

  see details

- data:

  same as `...`, but can be a vector

- .raise_error:

  whether to raise error if data is missing

- rave_data:

  internally used

## Details

This function checks whether "ECoG" data is loaded. The format is:
`"DATA+(blankspace)+TYPE"`. `"DATA"` can be "power" (wavelet transform
amplitude), "phase" (complex angle), or "volt"/"voltage" (Before
wavelet). `"TYPE"` can be "raw" (no reference), "referenced" (referenced
by common average reference, white matter reference, or bipolar
reference). For voltage data, there is one more special type "full"
which loads voltage data for all electrodes.
