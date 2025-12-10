# R6 class for `iEEG/ECoG` data Repository

A repository to keep subject information, including electrode instances,
reference information, epoch data, and offers method to epoch data.

## Author

Zhengjia Wang

## Public fields

- `subject`:

  [`Subject`](https://beauchamplab.github.io/rave/reference/Subject.md)
  instance

- `raw`:

  dictionary to store
  [`Electrode`](https://beauchamplab.github.io/rave/reference/Electrode.md)
  instances

- `reference`:

  dictionary to store references for electrodes

- `epochs`:

  dictionary to store epoch data

- `raw_volt`:

  environment, stores pre-referenced analog traces

- `raw_power`:

  environment, stores pre-referenced power spectrum

- `raw_phase`:

  environment, stores pre-referenced phase data

- `volt`:

  environment, stores referenced analog traces

- `power`:

  environment, stores referenced power spectrum

- `phase`:

  environment, stores referenced phase data

## Methods

### Public methods

- [`ECoGRepository$info()`](#method-ECoGRepository-info)

- [`ECoGRepository$print()`](#method-ECoGRepository-print)

- [`ECoGRepository$new()`](#method-ECoGRepository-new)

- [`ECoGRepository$get_electrode()`](#method-ECoGRepository-get_electrode)

- [`ECoGRepository$load_electrodes()`](#method-ECoGRepository-load_electrodes)

- [`ECoGRepository$epoch()`](#method-ECoGRepository-epoch)

- [`ECoGRepository$load_reference()`](#method-ECoGRepository-load_reference)

- [`ECoGRepository$baseline()`](#method-ECoGRepository-baseline)

------------------------------------------------------------------------

### Method `info()`

obtain the information

#### Usage

    ECoGRepository$info(print = TRUE)

#### Arguments

- `print`:

  logical, whether to print the information, default is true

#### Returns

character of the information

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print memory address

#### Usage

    ECoGRepository$print(...)

#### Arguments

- `...`:

  ignored

#### Returns

none

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    ECoGRepository$new(subject, reference = "default", autoload = TRUE)

#### Arguments

- `subject`:

  character such as `"project/subject"` or
  [`Subject`](https://beauchamplab.github.io/rave/reference/Subject.md)
  instance

- `reference`:

  character, reference name, default is `"default"`, which refers to
  `"reference_default.csv"` in subject meta folder

- `autoload`:

  logical, whether to auto-load reference for all electrodes, default is
  yes.

#### Returns

An `ECoGRepository` instance

------------------------------------------------------------------------

### Method `get_electrode()`

get
[`Electrode`](https://beauchamplab.github.io/rave/reference/Electrode.md)
instances

#### Usage

    ECoGRepository$get_electrode(electrode, name = "raw")

#### Arguments

- `electrode`:

  integers, referring to electrode numbers

- `name`:

  character, `"raw"`, `"power"`, `"raw_phase"`, etc.

#### Returns

list of environments containing electrode instances

------------------------------------------------------------------------

### Method `load_electrodes()`

load electrodes; usually don't need to directly call this method if
`autoload` is true when initializing the repository

#### Usage

    ECoGRepository$load_electrodes(electrodes, reference = "default")

#### Arguments

- `electrodes`:

  electrode number (integer)

- `reference`:

  name of reference

#### Returns

none

------------------------------------------------------------------------

### Method `epoch()`

slice the data according to epoch table

#### Usage

    ECoGRepository$epoch(
      epoch_name,
      pre,
      post,
      electrodes = NULL,
      frequency_range = NULL,
      data_type = "power",
      referenced = TRUE,
      func = NULL,
      quiet = FALSE
    )

#### Arguments

- `epoch_name`:

  the name of epoch; for example, `"YABa"` refers to `"epoch_YABa.csv"`
  in subject meta folder.

- `pre`:

  positive number in seconds, how long should the time be kept before
  the onset

- `post`:

  positive number in seconds, how long should the time be kept after
  onset

- `electrodes`:

  integers, electrode numbers

- `frequency_range`:

  experimental, frequency range to include

- `data_type`:

  data types to epoch; default is `"power"`, which is power spectrum, or
  amplitude. Other choices are `"phase"` for phase data and `"volt"` for
  voltage or analog signal traces.

- `referenced`:

  whether to load data referenced or without reference

- `func`:

  experimental, function to apply to each electrodes

- `quiet`:

  whether to suppress output messages, default is no

#### Returns

none. However the results are stored in public fields.

------------------------------------------------------------------------

### Method `load_reference()`

load references

#### Usage

    ECoGRepository$load_reference(ref_name, electrodes = NULL)

#### Arguments

- `ref_name`:

  reference name

- `electrodes`:

  electrode numbers

#### Returns

none

------------------------------------------------------------------------

### Method [`baseline()`](https://beauchamplab.github.io/rave/reference/baseline.md)

baseline signals (deprecated)

#### Usage

    ECoGRepository$baseline(from, to, electrodes = NULL, print.time = FALSE)

#### Arguments

- `from, to, electrodes, print.time`:

  internally used

#### Returns

data after baseline. Please use
[`baseline`](https://beauchamplab.github.io/rave/reference/baseline.md)
instead

## Examples

``` r
if (FALSE) { # \dontrun{

# Two ways to create instances
repo <- ECoGRepository$new('demo/YAB')

subject <- Subject$new(project_name = 'demo', subject_code = 'YAB')
repo <- ECoGRepository$new(subject)

# Create an instance without auto collecting references, only load 
# interesting electrodes
repo <- ECoGRepository$new('demo/YAB', autoload = FALSE)
repo$load_electrodes(c(14,15))

# Create an instance with non-default reference
repo <- ECoGRepository$new('demo/YAB', reference = 'bipolar')

# Epoch data according to epoch file "epoch_YABaOutlier.csv" in meta folder
# epoch_name should be "epoch_(name).csv"
repo$epoch(epoch_name = 'YABaOutlier', pre = 1, post = 2, 
           electrodes = 14, referenced = TRUE, data_type = "power")
repo$power
#> Dimension:  287 x 16 x 301 x 1 
#> - Trial: 1, 2, 3, 4, 5, 6,...
#> - Frequency: 2, 12, 22, 32, 42...
#> - Time: -1, -0.99, -0.98,...
#> - Electrode: 14

} # }
```
