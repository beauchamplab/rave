# Load local cache for fast importing voltage, power, and phase

Load local cache for fast importing voltage, power, and phase

## Usage

``` r
load_local_cache(
  project_name,
  subject_code,
  epoch,
  time_range,
  frequency_range = NULL,
  electrodes,
  referenced = FALSE,
  data_type = "voltage"
)
```

## Arguments

- project_name:

  project name

- subject_code:

  subject code

- epoch:

  epoch name

- time_range:

  time range to cache

- frequency_range:

  frequency range to cache

- electrodes:

  electrodes to cache

- referenced:

  which reference to be used

- data_type:

  which type(s) of data to cache
