# R6 Class for Electrode

Stores single electrode or reference signals

## Author

Zhengjia Wang

## Public fields

- `electrode`:

  electrode number in integer

- `raw_power`:

  stores pre-epoch power spectrum with no reference

- `raw_phase`:

  stores pre-epoch phase with no reference

- `raw_volt`:

  stores pre-epoch analog traces with no reference

- `phase`:

  stores pre-epoch phase after reference

- `power`:

  stores pre-epoch power spectrum after reference

- `volt`:

  stores pre-epoch analog traces after reference

- `preload`:

  which of the three data are pre-loaded

- `reference`:

  character or `Electrode` instance indicating the reference for current
  electrode

- `has_power, has_phase, has_volt`:

  whether power, phase, or voltage data exist in file system before and
  after reference

## Active bindings

- `has_power, has_phase, has_volt`:

  whether power, phase, or voltage data exist in file system before and
  after reference

- `blocks`:

  character vector of block names (read-only)

- `subject_id`:

  character of subject ID (read-only)

- `reference_electrode`:

  whether this is a reference (read-only)

## Methods

### Public methods

- [`Electrode$info()`](#method-Electrode-info)

- [`Electrode$print()`](#method-Electrode-print)

- [`Electrode$switch_reference()`](#method-Electrode-switch_reference)

- [`Electrode$referenced()`](#method-Electrode-referenced)

- [`Electrode$clean()`](#method-Electrode-clean)

- [`Electrode$new()`](#method-Electrode-new)

- [`Electrode$epoch()`](#method-Electrode-epoch)

------------------------------------------------------------------------

### Method `info()`

print electrode information

#### Usage

    Electrode$info()

#### Returns

none

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

overrides default print method

#### Usage

    Electrode$print(...)

#### Arguments

- `...`:

  ignored

#### Returns

none

------------------------------------------------------------------------

### Method `switch_reference()`

switch reference (experimental)

#### Usage

    Electrode$switch_reference(new_reference)

#### Arguments

- `new_reference`:

  An electrode instance

#### Returns

none

------------------------------------------------------------------------

### Method `referenced()`

get referenced data

#### Usage

    Electrode$referenced(type = "power", ram = TRUE)

#### Arguments

- `type`:

  which data to reference, default is power

- `ram`:

  whether to load data to memory

#### Returns

If `ram` is true, then returns a list of matrices. The length of the
list equals the number of blocks, and each matrix is frequency by time
points. If `ram` is false, then returns an environment with each element
a `LazyH5` or
[`LazyFST`](http://dipterix.org/ieegio/reference/LazyFST.md) instance.

------------------------------------------------------------------------

### Method `clean()`

remove data from memory

#### Usage

    Electrode$clean(types = c("power", "phase", "volt"), force = FALSE)

#### Arguments

- `types`:

  data types to clean

- `force`:

  whether to remove pre-loaded data types

#### Returns

none

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    Electrode$new(
      subject,
      electrode,
      reference_by = "noref",
      preload = NULL,
      is_reference = FALSE
    )

#### Arguments

- `subject`:

  [`Subject`](https://beauchamplab.github.io/rave/reference/Subject.md)
  instance or characters like `"proj/sub"`

- `electrode`:

  number, integer

- `reference_by`:

  reference signals, choices are character, or `Electrode` instance;
  default is `"noref"`, meaning no reference to the electrode

- `preload`:

  data to load along with constructor

- `is_reference`:

  is current instance a reference?

#### Returns

An `Electrode` instance

------------------------------------------------------------------------

### Method `epoch()`

epoch electrode

#### Usage

    Electrode$epoch(
      epoch_name,
      pre,
      post,
      types = c("volt", "power", "phase"),
      raw = FALSE,
      hybrid = TRUE
    )

#### Arguments

- `epoch_name`:

  epoch name, for example, `epoch_name="default"` refers to epoch file
  `"epoch_default.csv"` in subject meta folder

- `pre`:

  seconds before trial onset to load

- `post`:

  seconds after trial onset to load

- `types`:

  characters, data types to load; choices are `"volt"`, `"power"`, and
  `"phase"`

- `raw`:

  whether epoch pre-referenced data?

- `hybrid`:

  whether to fast-cache the data on hard-drive? See also
  [`Tensor`](https://beauchamplab.github.io/raveio/reference/Tensor.html)

#### Returns

list of data after epoch

## Examples

``` r
if (FALSE) { # \dontrun{
# Electrode with no reference
e1 <- Electrode$new('demo/YAB', electrode = 14, reference_by = 'noref')
e1$reference
#> Subject: demo/YAB
#> Electrode: noref (Reference)

# Add Common Average Reference in rave/data/reference/ref_13-63,65-84.h5
e2 <- Electrode$new('demo/YAB', electrode = 14, 
                    reference_by = 'ref_13-63,65-84')

# Electrode with bipolar reference by another electrode
e3 <- Electrode$new('demo/YAB', electrode = 14, reference_by = 'ref_15')

# Alternative way
reference <- Electrode$new('demo/YAB', electrode = 15, is_reference = TRUE)
e4 <- Electrode$new('demo/YAB', electrode = 14, reference_by = reference)

# e3, e4 are the same in scientific meaning. To test it, epoch them
power3 <- e3$epoch('YABaOutlier', 1, 2, 'power', 
                   raw = FALSE, hybrid = FALSE)$power
power4 <- e4$epoch('YABaOutlier', 1, 2, 'power',
                   raw = FALSE, hybrid = TRUE)$power

# Compare e3 and e4, result difference should be 0
range(power3$get_data() - power4$get_data())
#> 0

# With or without hybrid, the size will be different
# No hybrid, totally in memory
lobstr::obj_size(power3)
#> 12 MB
# Hybrid, data is swapped to hard-drive
lobstr::obj_size(power4)
#> 908 kB
} # }
```
