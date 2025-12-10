# R6 Class for 'RAVE' Subject

contains subject meta information after preprocessing.

## Author

Zhengjia Wang

## Public fields

- `meta`:

  environment stores subject meta data

- `subject_id`:

  character, subject ID, generated from project name and subject code.
  For example, project name is `"congruency"` and subject code is
  `"YAB"`, then the `subject_id="congruency/YAB"`

- `subject_code`:

  identifier for subject

- `project_name`:

  project name

- `dirs`:

  stores folder paths for subject data

- `is_strict`:

  whether preprocess directory is checked when initializing the instance

## Active bindings

- `electrodes`:

  electrode table (read-only)

- `frequencies`:

  frequency table (read-only)

- `time_points`:

  time-point table (read-only)

- `time_excluded`:

  (deprecated) excluded time-point table (read-only)

- `sample_rate`:

  time-point table (read-only, for compatibility issues)

- `volt_sample_rate`:

  voltage (trace) sampling rate in Hertz (read-only)

- `power_sample_rate`:

  power (amplitude) sampling rate in Hertz (read-only)

- `phase_sample_rate`:

  phase sampling rate in Hertz (read-only)

- `valid_electrodes`:

  all valid electrodes in current reference scheme (read-only)

- `id`:

  read-only version of subject ID

## Methods

### Public methods

- [`Subject$info()`](#method-Subject-info)

- [`Subject$print()`](#method-Subject-print)

- [`Subject$new()`](#method-Subject-new)

- [`Subject$preprocess_info()`](#method-Subject-preprocess_info)

- [`Subject$filter_all_electrodes()`](#method-Subject-filter_all_electrodes)

- [`Subject$filter_valid_electrodes()`](#method-Subject-filter_valid_electrodes)

- [`Subject$has_bad_time_point()`](#method-Subject-has_bad_time_point)

- [`Subject$clone()`](#method-Subject-clone)

------------------------------------------------------------------------

### Method `info()`

print the information of the subject

#### Usage

    Subject$info()

#### Returns

none

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

override of default print method

#### Usage

    Subject$print(...)

#### Arguments

- `...`:

  ignored

#### Returns

default memory address of the environment

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    Subject$new(project_name, subject_code, reference = NULL, strict = TRUE)

#### Arguments

- `project_name`:

  project name

- `subject_code`:

  subject code

- `reference`:

  what kind of reference is default for the subject, default is
  "default", referring to `"reference_default.csv"` in subject meta
  folder

- `strict`:

  whether to check if the raw folder exists

------------------------------------------------------------------------

### Method `preprocess_info()`

Obtain preprocessing information. This methods is rarely directly
called, I wrap up most commonly used fields in other functions

#### Usage

    Subject$preprocess_info(key, default = NULL, customized = FALSE)

#### Arguments

- `key`:

  the fields or items store in `SubjectInfo2` instance

- `default`:

  default value if the key is not found

- `customized`:

  indicates whether the key refers to additional items or fields in
  `SubjectInfo2`. Default is false, meaning the key is the fields.

#### Returns

the preprocess information correspond to the key

------------------------------------------------------------------------

### Method `filter_all_electrodes()`

filter, and returns existing electrodes

#### Usage

    Subject$filter_all_electrodes(electrodes)

#### Arguments

- `electrodes`:

  integer vector

#### Returns

the electrodes that the subject has, including bad, or invalid
electrodes.

------------------------------------------------------------------------

### Method `filter_valid_electrodes()`

filter, and returns valid electrodes

#### Usage

    Subject$filter_valid_electrodes(electrodes)

#### Arguments

- `electrodes`:

  integer vector

#### Returns

the valid electrodes. Invalid electrodes refers to bad electrodes, or
the end of bipolar reference. If `"Reference"` column is blank in the
reference file, then the electrode is invalid.

------------------------------------------------------------------------

### Method `has_bad_time_point()`

(deprecated) check whether the selected time is excluded

#### Usage

    Subject$has_bad_time_point(block, electrode, start, end)

#### Arguments

- `block`:

  block name

- `electrode`:

  electrode number

- `start`:

  start time

- `end`:

  end time

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Subject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{

# Load subject, use `strict=FALSE` if not sure the existence of raw files 
subject <- Subject$new(project_name = 'demo', 'YAB', strict = FALSE)

# Filter 1:14 to see which numbers refer to the valid electrodes
subject$filter_valid_electrodes(1:14)
#> [1] 13 14

} # }
```
