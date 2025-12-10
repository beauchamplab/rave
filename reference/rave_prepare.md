# Load Subject and Create `iEEG/ECoG` Data Environment

Loads subject data along with `iEEG/ECoG` data into memory.

## Usage

``` r
rave_prepare(
  subject,
  electrodes,
  epoch,
  time_range,
  frequency_range,
  data_types = c("power"),
  reference = "default",
  attach = "r",
  data_env = getDefaultDataRepository(),
  strict = FALSE,
  ...
)
```

## Arguments

- subject:

  characters, format: `"PROJECT/SUBJECT"`

- electrodes:

  integer vector, which electrodes to be loaded

- epoch:

  characters, name for epoch data. For example, `"epoch1"` refers to
  epoch file `"epoch_epoch1.csv"` in subject meta folder

- time_range:

  vector of length 2. `time_range[1]=1` indicates 1 second before onset
  will be loaded; `time_range[2]=1.5` means 1.5 seconds after onset will
  be loaded. Make sure both are positive number in seconds

- frequency_range:

  vector of length 2 - lowest and highest frequencies. By default is all
  frequencies. Only applied to power and phase data.

- data_types:

  vector of characters, data to be pre-loaded. `"power"` refers to
  referenced power (power spectrum) data, `"phase"` refers to referenced
  phase data, and `"volt"` is referenced voltage (original analog
  traces) data

- reference:

  name of reference data. For example, `"default"` refers to reference
  file `"reference_default.csv"` in subject meta folder

- attach, :

  characters or `NULL`, `NULL` if you don't want to attach it, `"r"` if
  want to load data as R environment, `"py"` is for python, and
  `"matlab"` is for Matlab. (python and Matlab are under construction)

- data_env:

  environment to load data into.

- strict:

  whether to check if raw data exists. Default is no (suggested)

- ...:

  ignored
